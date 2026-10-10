# DFIR lab (forensics course)

Artix host + two QEMU/KVM guests: Windows 11 Pro (FTK Imager, EZ tools) and SANS SIFT.
Case data lives in the Cryptomator vault (unlock first):
`DF=~/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2` → `evidence/` (ro in guests), `cases/` (rw).

```
bin/dfir-lab.sh          lesson runner: start|stop|status|ssh|usb|dump|keys|baseline
bin/win11-dfir.sh        install|run|net   SSH 127.0.0.1:2222 (analyst)
bin/sift-dfir.sh         run|net           SSH 127.0.0.1:2223 (sansforensics), 9p evidence(ro)/cases(rw)
bin/fetch-labs.sh        course exercise files -> $DF/evidence/labs, hashes -> $DF/cases
bin/dfir-assess.sh       read-only host check
bootstrap/host.sh        rebuild ~/vm, symlink bin/ into ~/.local/bin, build unattend/setup ISOs, convert SIFT OVA
guest/windows/           autounattend.xml (template, password injected at build), setup.ps1
guest/sift/setup.sh      9p fstab, RegRipper fix, tool check
iso/SHA256SUMS           expected ISO/OVA hashes (sha256sum -c --ignore-missing)
```
`run` = isolated NIC (`restrict=on`, no internet, SSH works). `net` = internet.

## Each lesson
```
dfir-lab.sh start            # vault check, boot both guests isolated, SSH wait, sanity checks, cases/<date>/notes.md
dfir-lab.sh start --net      # same, with internet (updates, Volatility symbols)
dfir-lab.sh start --clean    # revert to baseline first
dfir-lab.sh usb attach B D   # FTK lab: hot-plug USB key into Windows
dfir-lab.sh dump win         # memory dump -> cases/<date>/mem/, sha256 -> cases/<date>/hashes.txt
dfir-lab.sh stop
```
One-time after install: `dfir-lab.sh keys` (key auth for both guests), then `dfir-lab.sh stop && dfir-lab.sh baseline`.

## Rebuild from zero
1. Download ISOs into `~/vm/iso` (Windows: microsoft.com/software-download/windows11, multi-edition x64 English; SIFT OVA: sans.org).
2. `bash bootstrap/host.sh` and follow the printed steps.
3. Baselines (guests shut down):
```
qemu-img snapshot -c baseline ~/vm/images/win11-dfir/win11.qcow2
cp ~/vm/images/win11-dfir/OVMF_VARS.4m.fd ~/vm/images/win11-dfir/OVMF_VARS.baseline.fd
cp -a ~/vm/images/win11-dfir/tpm ~/vm/images/win11-dfir/tpm.baseline
qemu-img snapshot -c baseline ~/vm/images/sift/sift.qcow2
```
Revert Windows: `qemu-img snapshot -a baseline …/win11.qcow2`, copy `OVMF_VARS.baseline.fd` and `tpm.baseline/` back.
Revert SIFT: `qemu-img snapshot -a baseline ~/vm/images/sift/sift.qcow2`.

## Gotchas hit during the first build
- Win11 26H2 setup checks Secure Boot capability → `OVMF_CODE.secboot.4m.fd` + `smm=on` + secure pflash.
- `dump-guest-memory -z` writes kdump, not ELF; use plain `dump-guest-memory`.
- `Get-NetFirewallRule` → 0x80070534 after deleting a user: orphaned per-user rules; setup.ps1 removes them, uses netsh.
- `Add-WindowsCapability OpenSSH.Server` can hang silently via Windows Update; setup.ps1 tries winget first.
- `pidwait` exits 1 when nothing matches: don't chain `; and qemu-img snapshot`.
- Memory slides use Volatility 2 (`vol.py --profile`); SIFT 2026 ships only Volatility 3 (`vol`). Use the course OVA `mint-19.3-volatility.ova`.
- Host: `limine-conf-watch` (dinit) needs `depends-on = local.target`, else its logfile in `@var` doesn't exist yet at boot.
