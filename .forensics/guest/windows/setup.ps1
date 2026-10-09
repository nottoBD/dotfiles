#Requires -RunAsAdministrator
# Windows DFIR guest setup — run once as admin, in `net` mode (re-runnable):
#   $d=(Get-Volume | ? FileSystemLabel -eq 'SETUP').DriveLetter; powershell -ExecutionPolicy Bypass -File "${d}:\setup.ps1"
# Prerequisite (GUI only): Windows Security > Virus & threat protection > Manage settings > Tamper Protection Off
# Log: C:\setup-transcript.txt
$ErrorActionPreference = 'Continue'
Start-Transcript -Path C:\setup-transcript.txt -Append | Out-Null

Write-Host "== Accounts" -ForegroundColor Cyan
if (-not (Get-LocalGroupMember Administrators | Where-Object Name -like '*\analyst')) {
    Add-LocalGroupMember -Group Administrators -Member analyst
}
net localgroup administrators

Write-Host "== Defender" -ForegroundColor Cyan
if ((Get-MpComputerStatus).IsTamperProtected) {
    Write-Warning 'Tamper Protection is ON: turn it off in Windows Security, then re-run.'
}
$rtp = 'HKLM:\SOFTWARE\Policies\Microsoft\Windows Defender\Real-Time Protection'
New-Item -Path $rtp -Force | Out-Null
Set-ItemProperty -Path $rtp -Name DisableRealtimeMonitoring -Value 1 -Type DWord
New-Item -Path C:\Cases, C:\Tools -ItemType Directory -Force | Out-Null
Add-MpPreference -ExclusionPath C:\Cases, C:\Tools
Remove-Item C:\Windows\Panther\unattend.xml -Force -ErrorAction SilentlyContinue

Write-Host "== VBS (expect 0)" -ForegroundColor Cyan
(Get-CimInstance -Namespace root\Microsoft\Windows\DeviceGuard -ClassName Win32_DeviceGuard).VirtualizationBasedSecurityStatus

Write-Host "== .NET 9 Desktop Runtime" -ForegroundColor Cyan
if (Get-Command winget -ErrorAction SilentlyContinue) {
    winget install --id Microsoft.DotNet.DesktopRuntime.9 -e --accept-source-agreements --accept-package-agreements
} else {
    Write-Warning 'winget not available yet: update "App Installer" from the Store, then re-run.'
}

Write-Host "== Eric Zimmerman tools" -ForegroundColor Cyan
Invoke-WebRequest https://download.ericzimmermanstools.com/Get-ZimmermanTools.zip -OutFile "$env:TEMP\gzt.zip"
Expand-Archive "$env:TEMP\gzt.zip" C:\Tools\EZ -Force
& C:\Tools\EZ\Get-ZimmermanTools.ps1 -Dest C:\Tools\EZ -NetVersion 9

Write-Host "== OpenSSH server" -ForegroundColor Cyan
if (-not (Get-Service sshd -ErrorAction SilentlyContinue)) {
    # winget MSI is fast; Add-WindowsCapability goes through Windows Update and can sit silent 10-20 min
    if (Get-Command winget -ErrorAction SilentlyContinue) {
        winget install --id Microsoft.OpenSSH.Preview -e --accept-source-agreements --accept-package-agreements
    }
    if (-not (Get-Service sshd -ErrorAction SilentlyContinue)) {
        Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
    }
}
Set-Service sshd -StartupType Automatic
Start-Service sshd
New-ItemProperty -Path HKLM:\SOFTWARE\OpenSSH -Name DefaultShell `
    -Value C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -PropertyType String -Force | Out-Null
# netsh, not *-NetFirewallRule: the cmdlets fail (0x80070534) if any rule is owned by a deleted user
netsh advfirewall firewall delete rule name="sshd-in" | Out-Null
netsh advfirewall firewall add rule name="sshd-in" dir=in action=allow protocol=TCP localport=22 profile=any
Get-Service sshd

Write-Host "== Orphaned firewall rules (owner SID no longer exists)" -ForegroundColor Cyan
$k = 'HKLM:\SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\FirewallRules'
$live = (Get-LocalUser).SID.Value
$orphans = (Get-Item $k).Property | Where-Object {
    (Get-ItemPropertyValue $k $_) -match 'LUOwn=(S-1-5-21-[\d-]+)' -and $Matches[1] -notin $live
}
"removing $($orphans.Count) (effective after reboot)"
$orphans | ForEach-Object { Remove-ItemProperty $k $_ }

Stop-Transcript | Out-Null
Write-Host "Done. Manual: FTK Imager (exterro.com/ftk-downloads), Thumbcache Viewer (thumbcacheviewer.github.io)." -ForegroundColor Green
