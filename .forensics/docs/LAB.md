# Forensics lab — build log and remaining work

Course: Digital Forensics (cylab.be slides, v2025-12-16). Stated requirement: *a Windows machine + the SIFT Workstation*.
Implementation: Artix host `penthrite`, two QEMU/KVM guests, case data in an encrypted vault, everything scripted and versioned in `~/projects/dotfiles/.forensics`.

Status as of 2026-10-10: both guests built, hardened, baselined, reachable over key-based SSH, driven by `dfir-lab.sh`. Remaining: a few tools, the Volatility 2 VM, lab data, and end-to-end validation of the lab workflows.

---

## 1. Architecture

```
 penthrite (Artix, dinit, Limine + sbctl Secure Boot, LUKS2 + btrfs, 14 GiB RAM, Ryzen 7 8845HS)
 │
 ├─ ~/vm/iso/                      ISOs/OVAs (CoW, checksummed)          hashes: .forensics/iso/SHA256SUMS
 ├─ ~/vm/images/  (+C nodatacow)   own btrfs subvolume → not in snapper @home snapshots
 │   ├─ win11-dfir/  win11.qcow2, OVMF_VARS.4m.fd, tpm/, *.baseline, unattend.iso, setup.iso, mon.sock
 │   └─ sift/        sift.qcow2, mon.sock
 │
 ├─ Cryptomator vault (FUSE)  DF=~/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2
 │   ├─ evidence/   → SIFT /mnt/evidence  (9p, read-only)
 │   └─ cases/      → SIFT /mnt/cases     (9p, rw)   cases/<date>/{notes.md,hashes.txt,mem/}
 │
 ├─ Windows 11 Pro 26H2  (win11-dfir.sh)   4 GiB, 6 vCPU   SSH 127.0.0.1:2222  analyst
 │     FTK Imager, Eric Zimmerman tools, .NET 9 — files in/out via scp, USB via hot-plug
 └─ SIFT 2026-04-22      (sift-dfir.sh)    4 GiB, 4 vCPU   SSH 127.0.0.1:2223  sansforensics
       Sleuth Kit, ewf-tools, RegRipper, libevtx, Volatility 3, tcpdump

 Network modes:  run = slirp restrict=on (no internet, SSH forward only)  |  net = slirp NAT with internet
```

Why this shape:
- **QEMU over VMware/VirtualBox** for the course guests: scriptable monitor (snapshots, memory dumps, ISO/USB hot-plug), no out-of-tree modules to sign under Secure Boot.
- **Evidence read-only at the hypervisor layer** (9p `readonly=on`, `-drive readonly=on`): the guest cannot write even if a tool tries (NTFS log replay, Windows indexing).
- **Isolated by default**: course images may contain live malware (stuxnet, cridex, prolaco samples in the memory chapter).
- **Defender off in Windows**: real-time protection quarantines/alters artefacts in evidence and exports.
- **VBS off** (`-cpu host,-svm`): no virtualization-based security inside the guest → simpler, standard memory images.

---

## 2. Course chapters → what each needs → where it runs

| Chapter (slides) | Tools / data | Runs on | Status |
|---|---|---|---|
| 00 Preamble | SIFT, Windows | both | ✅ |
| 01 Disks | FTK Imager 8.3 (USB key → E01), `ewfmount`, `mmls`, `fls`, `icat`, timeline; `usb-01…06` images | Windows (imaging) + SIFT (analysis) | Tools ✅ · USB passthrough ⏳ untested · data ⏳ |
| 02 Windows | RegRipper (`hives-01`), `evtxinfo/evtxexport` (`eventlogs-01`), Eric Zimmerman tools (`prefetch.zip`), Thumbcache Viewer | SIFT + Windows | RegRipper ✅ (no fix needed) · EZ ✅ · Thumbcache Viewer ⏳ · data ⏳ |
| 03.1 Memory (Windows) | **Volatility 2** (`vol.py --profile=WinXPSP2x86`, `imageinfo`, `kdbgscan`), stuxnet/cridex/prolaco images; dumping a VM's memory | Vol2 VM (course OVA) | ⏳ SIFT has only Volatility 3 |
| 03.2 Memory (Linux) | LiME (acquisition), Vol2 Linux profiles (`LinuxDebian5010x86`), victoria-v8 image | Vol2 VM | ⏳ |
| 04.1 Command line | grep/regex, `/proc/cpuinfo`, lorem.txt | SIFT | ✅ |
| 04.2 tcpdump | tcpdump, `lotsofweb.pcap`, `treasurehunt_fw_eth1.pcap`; macvendors lookup needs internet | SIFT | Tool ✅ · data ⏳ |
| Android memory (paper) | reading | — | ✅ |

---

## 3. What was done

### 3.1 Host assessment (`bin/dfir-assess.sh`, read-only)
- KVM usable: AMD-V, nested paging + nested virt on, `/dev/kvm` 0666. AVIC unsupported (firmware) — irrelevant.
- `kvm_amd` and VMware `vmmon`/`vmnet` coexist; never run QEMU and VMware guests at the same time.
- Secure Boot on (sbctl, custom keys), lockdown none → no impact on in-tree KVM.
- btrfs fully allocated (1 MiB unallocated) with ~150 GiB free inside chunks; now ~98 GiB free (VMs + vault share the root btrfs).
- RAM 14 GiB usable (780M iGPU carve-out) → guest sizes 4 + 4 GiB.
- Automount stack running (udisks2, gvfs, `pcmanfm -d`) — **still open, see §5**.
- Hardware clock UTC, ntpd running.
- `limine-conf-watch` (Secure Boot sync daemon) was failed → fixed (§3.6).

### 3.2 Windows guest
- **ISO**: Windows 11 consumer multi-edition, 26H2 (build 26300.9457), English x64, SHA-256 `bd4307df…ea650` verified against microsoft.com. Rejected: Enterprise evaluation (90 days → expires mid-exams). Installed edition: Pro, no key (unactivated: watermark only).
- **Firmware**: 26H2 setup refuses non-Secure-Boot-capable firmware → `OVMF_CODE.secboot.4m.fd` + `-machine smm=on` + `-global driver=cfi.pflash01,property=secure,value=on`; per-VM `OVMF_VARS.4m.fd`.
- **TPM 2.0**: `swtpm` socket started by the launcher, exits with QEMU.
- **Disk**: 80 GiB qcow2 (metadata prealloc) on virtio-blk; `viostor\w11\amd64` loaded during setup.
- **Answer file** (`guest/windows/autounattend.xml`): BitLocker auto-encryption off (TPM-bound keys + snapshot reverts = recovery screen), BypassNRO, local admin `analyst`, UTC time zone. OOBE network prompt still appeared once → local account via Shift+F10 `start ms-cxh:localonly`. An extra admin (`David`) created during OOBE was later removed.
- **Drivers**: `virtio-win-guest-tools.exe` (NetKVM, balloon, guest agent), loaded by hot-swapping the ISO into the empty default CD drive (`change ide2-cd0 …` on the monitor).
- **Hardening** (`guest/windows/setup.ps1`, delivered as a hot-swapped ISO because the GTK display has no clipboard):
  - `analyst` sole admin; orphaned per-user firewall rules of the deleted user removed (they broke `Get-NetFirewallRule` with 0x80070534).
  - Defender: Tamper Protection off (GUI), real-time monitoring disabled by policy, exclusions `C:\Cases`, `C:\Tools`.
  - Plaintext answer-file copy `C:\Windows\Panther\unattend.xml` deleted.
  - VBS status 0.
  - .NET 9 Desktop Runtime (winget), Eric Zimmerman tools in `C:\Tools\EZ` (`Get-ZimmermanTools.ps1 -NetVersion 9`).
  - FTK Imager 8.3 (exterro.com, free edition), installed manually.
  - OpenSSH Server (`Add-WindowsCapability`; slow/silent via Windows Update — script now tries winget first), DefaultShell = PowerShell, firewall rule via `netsh` (profile any — slirp NAT is classified Public).
- **SSH key auth** (`dfir-lab.sh keys`): admin accounts read only `C:\ProgramData\ssh\administrators_authorized_keys`. It contained a stringified PowerShell object from an earlier one-liner and a mangled key line → sshd silently refused the key. Fixed: file rewritten as ASCII with key lines only, owner and ACL Administrators/SYSTEM only. Key: `~/.ssh/id_dfir_lab` (dedicated, loopback only).
- **Baseline**: retaken 2026-10-10 10:24 — **before** the key fix → retake (§5.1).

### 3.3 SIFT guest
- `sift-2026-04-22.ova` (SANS) → extracted → `qemu-img convert` vmdk → qcow2. BIOS boot, virtio-blk.
- OpenSSH server enabled; key auth working.
- 9p shares in `/etc/fstab` (duplicate lines from a double paste removed): `evidence` ro, `cases` rw, `msize=262144`, `nofail`. Write/read-only behaviour verified.
- Tools present: `vol` (Volatility 3), `mmls fls icat ewfmount rip.pl evtxinfo tcpdump`. **No `vol.py`** (Volatility 2 dropped from this SIFT release). `rip.pl` works without the cylab.be/blog/287 fix.
- Baseline retaken 2026-10-10 10:24 (contains the key).

### 3.4 Case data
- `evidence/` and `cases/` inside the Cryptomator vault (FUSE, not btrfs → no read-only snapshots; integrity = SHA-256 logs).
- Per lesson: `cases/<date>/notes.md` (chain-of-custody log, auto-appended), `hashes.txt`, `mem/`.

### 3.5 Scripts and versioning — `~/projects/dotfiles/.forensics`
| File | Purpose |
|---|---|
| `bin/dfir-lab.sh` | lesson runner: `start [--net] [--clean]`, `stop`, `status`, `ssh`, `usb [attach/detach]`, `dump`, `keys`, `baseline` |
| `bin/win11-dfir.sh` | Windows launcher: `install` / `run` (isolated) / `net` |
| `bin/sift-dfir.sh` | SIFT launcher: `run` / `net`, 9p shares, refuses to start if the vault is locked |
| `bin/fetch-labs.sh` | downloads all exercise files from the slides into `evidence/labs/<chapter>/`, hashes to `cases/hashes-labs.txt`, plus the Vol2 OVA |
| `bin/dfir-assess.sh` | read-only host check |
| `bootstrap/host.sh` | rebuild from zero (idempotent; never overwrites disks/VARS/TPM; injects the analyst password at ISO build time) |
| `guest/windows/{autounattend.xml,setup.ps1}` | Windows install + hardening |
| `guest/sift/setup.sh` | 9p fstab, RegRipper fix if needed, tool check |
| `iso/SHA256SUMS` | Windows ISO, SIFT OVA, virtio-win |
| `.gitignore` | images, firmware state, TPM state, evidence never committed |

`~/.local/bin/*.sh` are symlinks into `bin/` → edits are versioned. Commits are GPG-signed (`GPG_TTY` set in fish config).

### 3.6 Host fixes along the way
- `limine-conf-watch` failed with ENOENT opening `/var/log/limine-conf-watch.log` at boot; started manually, `sbctl verify` all signed. Unit already had `depends-on = local.target` → root cause unconfirmed.

---

## 4. Daily use

```fish
dfir-lab.sh start            # unlock vault first; boots both guests isolated, checks, lesson notes
dfir-lab.sh start --net      # internet (updates, Vol3 symbol downloads, macvendors)
dfir-lab.sh start --clean    # revert to baselines first
dfir-lab.sh ssh sift         # or: ssh win
dfir-lab.sh usb              # list host USB; then: dfir-lab.sh usb attach <bus> <addr> / usb detach
dfir-lab.sh dump win         # → cases/<date>/mem/*.elf + sha256; in SIFT: /mnt/cases/<date>/mem/
dfir-lab.sh stop
```
Files to Windows: `scp -P 2222 -i ~/.ssh/id_dfir_lab <file> analyst@127.0.0.1:C:/Cases/`

---

## 5. Remaining work (in order)

### 5.1 Close the key-auth fix (now)
Revert sshd debug logging, commit, retake the Windows baseline:
```fish
dfir-lab.sh ssh win
```
```powershell
$c = 'C:\ProgramData\ssh\sshd_config'
(Get-Content $c) -replace '^LogLevel DEBUG3','#LogLevel INFO' -replace '^SyslogFacility LOCAL0','#SyslogFacility AUTH' | Set-Content $c -Encoding ascii
Remove-Item C:\ProgramData\ssh\logs\sshd.log -ErrorAction SilentlyContinue
Restart-Service sshd
exit
```
```fish
cd ~/projects/dotfiles/.forensics
git add bin/dfir-lab.sh README.md docs/LAB.md && git commit -m "forensics: key-auth fixes, lab doc" && git push
dfir-lab.sh stop win && dfir-lab.sh baseline win
```

### 5.2 Host: stop automounting (before plugging any evidence USB key)
`pcmanfm -d` + udisks2 can mount a USB key on the host before it is passed to Windows (journal replay, atime → altered evidence).
```fish
grep -E 'mount_(on_startup|removable)|autorun' ~/.config/pcmanfm/default/pcmanfm.conf
sed -i -e 's/^mount_on_startup=.*/mount_on_startup=0/' -e 's/^mount_removable=.*/mount_removable=0/' -e 's/^autorun=.*/autorun=0/' ~/.config/pcmanfm/default/pcmanfm.conf
pkill -x pcmanfm; pcmanfm -d &
```
Verify: plug a test key → `findmnt | grep /run/media` must show nothing. For host-side imaging, also use a hardware write blocker or `blockdev --setro`.

### 5.3 Windows: Thumbcache Viewer (last missing tool) → re-baseline
FTK Imager 8.3 is installed. Thumbcache Viewer 1.0.4.0 (thumbcacheviewer.github.io), guest must be in `--net` mode:
```fish
dfir-lab.sh ssh win '$ProgressPreference=0; Invoke-WebRequest https://github.com/thumbcacheviewer/thumbcacheviewer/releases/download/v1.0.4.0/thumbcache_viewer_64.zip -OutFile $env:TEMP\tcv.zip; Expand-Archive $env:TEMP\tcv.zip C:\Tools\ThumbcacheViewer -Force; Get-ChildItem C:\Tools\ThumbcacheViewer'
dfir-lab.sh stop win && dfir-lab.sh baseline win
```

### 5.4 Lab data
```fish
fetch-labs.sh
ls -R ~/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2/evidence/labs | head -50
```
Check the entries fetched with server-provided names (thumbcache, the three memory images, victoria-v8): rename sensibly and re-run the hash step if needed. Memory images are large — watch free space (~98 GiB).

### 5.5 Volatility 2 VM (chapters 03.1 / 03.2)
The slides use Volatility 2 syntax and profiles; Volatility 3 can handle the Windows images (`windows.pslist` etc., symbols auto-downloaded in `--net`) but not the exercises as written, and not the Linux profile exercise.
- Convert `~/vm/iso/mint-19.3-volatility.ova` (from `fetch-labs.sh`) → `~/vm/images/vol2/vol2.qcow2` (same steps as SIFT; check the `.ovf` for EFI).
- Write `bin/vol2-dfir.sh` (copy of `sift-dfir.sh`, port 2224, name `vol2`) and add `vol2` to `dfir-lab.sh` (NAME/LAUNCH/PORT/USERN/RAM_G/DISK maps). RAM: run it instead of Windows, not alongside both.
- In the VM: enable SSH, 9p fstab (reuse `guest/sift/setup.sh`), check `vol.py --info | grep Linux`, install profile `LinuxDebian5010x86` from github.com/volatilityfoundation/profiles into `…/volatility/plugins/overlays/linux/`.
- Baseline.

### 5.6 Validate every workflow end to end (first real use of `usb`, `dump`)
1. **Disk imaging (01)**: `dfir-lab.sh start` → plug test key → `dfir-lab.sh usb` → `usb attach <bus> <addr>` → FTK Imager: Create Disk Image → Physical Drive → E01 → `C:\Cases` → `scp` E01 to `evidence/` → `sha256sum` → SIFT: `sudo ewfmount /mnt/evidence/usb.E01 /mnt/e01 && sudo mmls /mnt/e01/ewf1` → mount partition read-only with offset → `fls`/`icat`. Compare hash with FTK's report.
2. **Memory (03.1)**: `dfir-lab.sh start --net` → `dfir-lab.sh dump win` → SIFT: `vol -f /mnt/cases/<date>/mem/<file>.elf windows.info` and `windows.pslist`. Confirms the `dump-guest-memory -d` / `info dump` path and Vol3 symbol download.
3. **Registry / event logs (02)**: `rip.pl -r <hive> -p <plugin>` and `evtxinfo` on `hives-01`, `eventlogs-01`; PECmd on `prefetch.zip` in Windows (scp in).
4. **Network (04.2)**: `tcpdump -n -r /mnt/evidence/labs/04_network/lotsofweb.pcap -c 12`.
5. **Revert test**: `dfir-lab.sh stop && dfir-lab.sh start --clean` → Windows boots without BitLocker/TPM prompts, SSH keys still work.

### 5.7 Housekeeping
- Reboot once → `sudo dinitctl status limine-conf-watch` must be STARTED; if not, investigate the `/var` mount ordering.
- Disk: `sudo btrfs filesystem usage /` → if unallocated < 5 GiB: `sudo btrfs balance start -dusage=30 /`; prune old snapper snapshots.
- Cleanup: `~/vm/ubuntu-12-i386`, `ubuntu-12.04.3-desktop-i386.iso` if unused; `unattend.iso` / `setup.iso` contain the analyst password in plaintext (not versioned, but on disk).
- Windows Update: occasionally `start --net win`, update, then re-baseline (or never update an analysis box mid-course — pick one).
- Optional: test `bootstrap/host.sh` on a scratch path to confirm a from-zero rebuild works.
- Optional: Linux LiME acquisition practice needs a Linux target VM with matching kernel headers (Arch ISO is in `~/vm/iso`); the course supplies a ready image (victoria-v8), so this is extra.

---

## 6. Troubleshooting reference (all hit during the build)

| Symptom | Cause | Fix |
|---|---|---|
| Windows setup: "PC must support Secure Boot" | non-secboot OVMF | secboot OVMF + `smm=on` + secure pflash |
| Boot menu shows only "UEFI Misc Device" / PXE | install never completed | wipe qcow2, `win11-dfir.sh install` |
| OOBE asks for network / Wi-Fi driver | VM has no NIC in install mode; unattend not applied | Shift+F10 → `start ms-cxh:localonly` |
| Empty CD drive in Windows | QEMU default IDE CD | `change ide2-cd0 /abs/path.iso` on the monitor |
| `set VM …` paths resolve to `/` | command run in bash, not fish | full paths, or fish |
| `Add-WindowsCapability OpenSSH.Server` silent for 15+ min | Windows Update FoD download | wait, or winget `Microsoft.OpenSSH.Preview` |
| `hostfwd_add` "could not set up" | forward already exists | ignore |
| SSH to Windows hangs (no refuse) | firewall drops on Public profile | `netsh … profile=any` |
| `Get-NetFirewallRule` 0x80070534 | rules owned by deleted user SID | remove orphan rule values, reboot |
| `Restart-Service mpssvc` refused | protected service | reboot |
| `dfir-lab.sh keys` hangs after password | PowerShell 5.1 reads redirected stdin | `ssh -n`, `-InputFormat None` |
| Key offered, `Failed publickey`, nothing else logged | junk/mangled lines in `administrators_authorized_keys` | ASCII, key lines only, owner/ACL Administrators+SYSTEM |
| `pidwait …; and qemu-img snapshot` skipped | `pidwait` exits 1 if nothing matches | run the snapshot unconditionally |
| `fstab` lines doubled | block pasted twice | `awk '!(/^(evidence|cases) / && seen[$0]++)'` |
| GPG: "Inappropriate ioctl for device" | `GPG_TTY` unset | `set -gx GPG_TTY (tty)` in fish config |
| `limine-conf-watch` STOPPED at boot | logfile ENOENT in `/var` | manual start; root cause open |
