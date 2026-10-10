# Forensics lab — build log and remaining work

Course: Digital Forensics (cylab.be slides, v2025-12-16). Stated requirement: *a Windows machine + the SIFT Workstation*.
Implementation: Artix host `penthrite`, three QEMU/KVM guests, case data in an encrypted vault, everything scripted and versioned in `~/projects/dotfiles/.forensics` (a subdirectory of the dotfiles repo).

Status as of 2026-10-10 12:45: all three guests built, baselined, key-based SSH, driven by `dfir-lab.sh`. All lab data fetched and verified. Vol2 memory workflow validated end to end (stuxnet pslist). Remaining: confirm a few unverified steps (§5.1), commit, the other end-to-end tests (§5.2), housekeeping (§5.3).

---

## 1. Architecture

```
 penthrite (Artix, dinit, Limine + sbctl Secure Boot, LUKS2 + btrfs, 14 GiB RAM, Ryzen 7 8845HS)
 │
 ├─ ~/vm/iso/                      ISOs/OVAs (CoW, checksummed)          hashes: .forensics/iso/SHA256SUMS
 ├─ ~/vm/images/  (+C nodatacow)   own btrfs subvolume → not in snapper @home snapshots
 │   ├─ win11-dfir/  win11.qcow2, OVMF_VARS.4m.fd, tpm/, *.baseline, unattend.iso, setup.iso, mon.sock
 │   ├─ sift/        sift.qcow2, mon.sock
 │   └─ vol2/        vol2.qcow2 (1 TiB virtual, ~10 GiB allocated), mon.sock
 │
 ├─ Cryptomator vault (FUSE)  DF=~/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2
 │   ├─ *_slides.pdf
 │   ├─ scripts/    rsync copy of .forensics (read-only mirror; edit in ~/projects)
 │   ├─ evidence/   → SIFT + Vol2 /mnt/evidence  (9p, read-only)
 │   │    └─ labs/<chapter>/  downloads; labs/03_memory/extracted/  unzipped images
 │   └─ cases/      → SIFT + Vol2 /mnt/cases     (9p, rw)
 │        ├─ <date>/{notes.md,hashes.txt,mem/}
 │        ├─ hashes-labs.2026-10-10.txt   frozen reference (444)
 │        ├─ hashes-labs.txt              recomputed each fetch-labs run, verified against the reference
 │        └─ hashes-labs-extracted.txt    hashes of unzipped images
 │
 ├─ Windows 11 Pro 26H2  (win11-dfir.sh)   4 GiB, 6 vCPU   SSH 127.0.0.1:2222  analyst
 │     FTK Imager 8.3, Eric Zimmerman tools, .NET 9, Thumbcache Viewer 1.0.4.0 — files via scp, USB via hot-plug
 ├─ SIFT 2026-04-22      (sift-dfir.sh)    4 GiB, 4 vCPU   SSH 127.0.0.1:2223  sansforensics
 │     Sleuth Kit, ewf-tools, RegRipper, libevtx, Volatility 3, tcpdump
 └─ Vol2 (Mint 19.3)     (vol2-dfir.sh)    4 GiB, 2 vCPU   SSH 127.0.0.1:2224  vagrant
       Volatility 2.6.1 in /opt/volatility, profile LinuxDebian5010x86 — run instead of Windows

 Network modes:  run = slirp restrict=on (no internet, SSH forward only)  |  net = slirp NAT with internet
```

Why this shape:
- **QEMU over VMware/VirtualBox** for the course guests: scriptable monitor (snapshots, memory dumps, ISO/USB hot-plug), no out-of-tree modules to sign under Secure Boot.
- **Evidence read-only at the hypervisor layer** (9p `readonly=on`, `-drive readonly=on`): the guest cannot write even if a tool tries (NTFS log replay, Windows indexing).
- **Isolated by default**: course images contain live malware (stuxnet, cridex, prolaco in the memory chapter).
- **Defender off in Windows**: real-time protection quarantines/alters artefacts in evidence and exports.
- **VBS off** (`-cpu host,-svm`): no virtualization-based security inside the guest → simpler, standard memory images.
- **Separate Vol2 guest**: the slides use Volatility 2 syntax and profiles; this SIFT release ships only Volatility 3.

---

## 2. Course chapters → what each needs → where it runs

| Chapter (slides) | Tools / data | Runs on | Status |
|---|---|---|---|
| 00 Preamble | SIFT, Windows | both | ✅ |
| 01 Disks | FTK Imager 8.3 (USB key → E01), `ewfmount`, `mmls`, `fls`, `icat`; `usb-01…06` | Windows (imaging) + SIFT | Tools ✅ · data ✅ (ewfverify OK) · USB passthrough ⏳ untested |
| 02 Windows | RegRipper (`hives-01`), `evtxinfo` (`eventlogs-01`), EZ tools (`prefetch.zip`), Thumbcache Viewer (`thumbcache-01`) | SIFT + Windows | Tools ✅ · data ✅ · workflow ⏳ |
| 03.1 Memory (Windows) | Volatility 2 (`--profile=WinXPSP2x86`, `imageinfo`, `kdbgscan`), stuxnet/cridex/prolaco; dumping a VM | Vol2 | ✅ stuxnet pslist validated · `dump win` + Vol3 ⏳ |
| 03.2 Memory (Linux) | Vol2 profile `LinuxDebian5010x86`, victoria-v8 | Vol2 | Profile ✅ · data ✅ · run ⏳ |
| 04.1 Command line | grep/regex, `/proc/cpuinfo`, lorem.txt | SIFT | ✅ (lorem.txt not in fetch list — add if the slides link it) |
| 04.2 tcpdump | tcpdump, `lotsofweb.pcap`, `treasurehunt_fw_eth1.pcap`; macvendors needs internet | SIFT | Tool ✅ · data ✅ · run ⏳ |
| Android memory (paper) | reading | — | ✅ |

---

## 3. What was done

### 3.1 Host assessment (`bin/dfir-assess.sh`, read-only)
- KVM usable: AMD-V, nested paging + nested virt on, `/dev/kvm` 0666. AVIC unsupported (firmware), irrelevant.
- `kvm_amd` and VMware `vmmon`/`vmnet` coexist; never run QEMU and VMware guests at the same time.
- Secure Boot on (sbctl, custom keys), lockdown none → no impact on in-tree KVM.
- btrfs fully allocated (1 MiB unallocated); ~82 GiB free on `~/vm/images` after lab data and Vol2.
- RAM 14 GiB usable (780M iGPU carve-out) → 4 GiB guests, at most two at a time plus headroom.
- Host automount: `~/.config/pcmanfm/default/pcmanfm.conf` set to `mount_on_startup=0`, `mount_removable=0`, `autorun=0`. Daemon restart and test-key check **unconfirmed** (§5.1).
- Hardware clock UTC, ntpd running.

### 3.2 Windows guest
- **ISO**: Windows 11 consumer multi-edition, 26H2 (build 26300.9457), English x64, SHA-256 `bd4307df…ea650` verified against microsoft.com. Rejected: Enterprise evaluation (90 days → expires mid-exams). Installed edition: Pro, no key (unactivated: watermark only).
- **Firmware**: 26H2 setup refuses non-Secure-Boot-capable firmware → `OVMF_CODE.secboot.4m.fd` + `-machine smm=on` + `-global driver=cfi.pflash01,property=secure,value=on`; per-VM `OVMF_VARS.4m.fd`.
- **TPM 2.0**: `swtpm` socket started by the launcher, exits with QEMU.
- **Disk**: 80 GiB qcow2 (metadata prealloc) on virtio-blk; `viostor\w11\amd64` loaded during setup.
- **Answer file** (`guest/windows/autounattend.xml`): BitLocker auto-encryption off, BypassNRO, local admin `analyst`, UTC. OOBE network prompt still appeared once → Shift+F10 `start ms-cxh:localonly`. Extra admin `David` created during OOBE later removed.
- **Drivers**: `virtio-win-guest-tools.exe`, loaded by hot-swapping the ISO into the empty default CD drive (`change ide2-cd0 …`).
- **Hardening** (`guest/windows/setup.ps1`, delivered as a hot-swapped ISO because the GTK display has no clipboard):
  - `analyst` sole admin; orphaned firewall rules of the deleted user removed (0x80070534).
  - Defender: Tamper Protection off (GUI), real-time monitoring off by policy, exclusions `C:\Cases`, `C:\Tools`.
  - `C:\Windows\Panther\unattend.xml` deleted. VBS status 0.
  - .NET 9 Desktop Runtime, Eric Zimmerman tools in `C:\Tools\EZ`, FTK Imager 8.3.
  - Thumbcache Viewer 1.0.4.0 in `C:\Tools\ThumbcacheViewer` (latest release, checked 2026-10-10). `thumbcache_viewer_64.zip` SHA-256 `8d91a3156318ed26df11202a86dc0b53e2d85fd7e2cb02caf67f2fb63006e5ed`.
  - OpenSSH Server, DefaultShell PowerShell, firewall rule profile any.
- **SSH key auth**: `C:\ProgramData\ssh\administrators_authorized_keys` rewritten as ASCII, key lines only, ACL Administrators/SYSTEM. Key `~/.ssh/id_dfir_lab`. sshd debug logging reverted (`#LogLevel INFO`, `#SyslogFacility AUTH`), `sshd.log` deleted.
- **Baseline**: retake after the sshd revert + Thumbcache Viewer **unconfirmed** (§5.1).

### 3.3 SIFT guest
- `sift-2026-04-22.ova` → vmdk → qcow2. BIOS boot, virtio-blk. OpenSSH, key auth.
- 9p fstab: `evidence` ro, `cases` rw, `msize=262144`, `nofail`. ewfverify on all three E01s: SUCCESS.
- Tools: `vol` (Volatility 3), `mmls fls icat ewfmount rip.pl evtxinfo tcpdump`. No `vol.py`.
- Baseline 2026-10-10 10:24 (contains the key).

### 3.4 Case data
- `fetch-labs.sh` fetched 16 lab files into `evidence/labs/{01_disks,02_windows,03_memory,04_network}`. Short-link entries (`cylab.be/s/<code>`) are saved with the server's filename (`curl -OJ`) and tracked by hidden `.<code>.done` markers.
- Integrity: frozen reference `cases/hashes-labs.2026-10-10.txt` (chmod 444); `sha256sum -c` all OK; `unzip -t` all zips OK; `ewfverify` all E01s OK.
- Extracted: `evidence/labs/03_memory/extracted/stuxnet.vmem` (512 MiB) SHA-256 `5f19ff1333fc3901fbf3fafb50d2adb0c495cf6d33789e5a959499a92aeefe77` → `cases/hashes-labs-extracted.txt`. Extract with `unzip -n <zip> -x '__MACOSX/*' -d …/extracted`. Never unzip onto a guest disk.

### 3.5 Scripts and versioning — `~/projects/dotfiles/.forensics`
| File | Purpose |
|---|---|
| `bin/dfir-lab.sh` | lesson runner for `win`, `sift`, `vol2`: `start [--net] [--clean]`, `stop`, `status`, `ssh`, `usb`, `dump`, `keys`, `baseline`. Default guests: win + sift, plus vol2 if running |
| `bin/win11-dfir.sh` | Windows launcher: `install` / `run` / `net` |
| `bin/sift-dfir.sh` | SIFT launcher: `run` / `net`, 9p shares, refuses to start if the vault is locked |
| `bin/vol2-dfir.sh` | Vol2 launcher: `run` / `net`, same 9p shares, port 2224, `DISK_BUS=ahci` fallback |
| `bin/fetch-labs.sh` | lab files + Vol2 OVA. OVA: download to `.part`, resume (`curl -C -`, restart on rc 33), rename only after `tar -tf` passes. Hashes verified against the frozen reference |
| `bin/dfir-assess.sh` | read-only host check |
| `bootstrap/host.sh` | rebuild from zero (idempotent; never overwrites disks/VARS/TPM) |
| `guest/windows/{autounattend.xml,setup.ps1}` | Windows install + hardening |
| `guest/sift/setup.sh` | 9p fstab, RegRipper fix if needed, tool check |
| `iso/SHA256SUMS` | Windows ISO, SIFT OVA, virtio-win, Mint Vol2 OVA |

`~/.local/bin/*.sh` are symlinks into `bin/`. Commits GPG-signed. The vault `scripts/` copy is refreshed with `rsync -a --delete ~/projects/dotfiles/.forensics/ $DF/scripts/`.

### 3.6 Vol2 guest
- `mint-19.3-volatility.ova` (cylab.be/s/jfp0o, VirtualBox 6.0.14 export): 4,129,124,352 bytes, SHA-256 `c5dcf541f244a7c2bd8267f1d0ce2ffeb7f50fabde0bc6ddd8f1f2ae6e8719bc`. OVF: BIOS, SATA/AHCI, 2 vCPU, 4096 MB, NAT NIC.
- Converted to qcow2 (metadata prealloc). Virtual size 1 TiB as created upstream: the guest `df` says nothing about host space.
- Boots on virtio-blk / virtio-net / virtio-vga with kernel 5.0.0-32-generic.
- User `vagrant` (passwordless sudo). openssh-server installed, `PasswordAuthentication no`, `authorized_keys` holds only the `dfir-lab` key.
- 9p fstab: same two lines as SIFT. `9p 9pnet 9pnet_virtio` added to `/etc/initramfs-tools/modules` + `update-initramfs -u` (see §6).
- Volatility 2.6.1 in `/opt/volatility` (`/usr/bin/vol.py` symlink). Default install has no Linux profiles. `Debian5010.zip` (from `raw.githubusercontent.com/volatilityfoundation/profiles/master/Linux/Debian/x86/`) in `/opt/volatility/volatility/plugins/overlays/linux/` → `LinuxDebian5010x86`.
- Validation: `vol.py -f stuxnet.vmem --profile=WinXPSP2x86 pslist` → 3× `lsass.exe` (PID 680 parent winlogon; 868 and 1928 parent services.exe, started 2011-06-03).
- Baseline 2026-10-10 12:43:33.

### 3.7 Host fixes along the way
- `limine-conf-watch` failed with ENOENT opening `/var/log/limine-conf-watch.log` at boot; started manually, `sbctl verify` all signed. Root cause unconfirmed (§5.3).

---

## 4. Daily use

```fish
dfir-lab.sh start                 # unlock vault first; win + sift, isolated, checks, lesson notes
dfir-lab.sh start --net           # internet (updates, Vol3 symbols, macvendors)
dfir-lab.sh start --clean         # revert to baselines first
dfir-lab.sh start vol2            # memory chapters: stop win first (RAM)
dfir-lab.sh ssh sift              # or: ssh win | ssh vol2
dfir-lab.sh usb                   # list host USB; then: usb attach <bus> <addr> / usb detach
dfir-lab.sh dump win              # → cases/<date>/mem/*.elf + sha256
dfir-lab.sh stop                  # all running guests (vol2 included)
dfir-lab.sh baseline vol2         # baseline never includes vol2 unless named
```
Files to Windows: `scp -P 2222 -i ~/.ssh/id_dfir_lab <file> analyst@127.0.0.1:C:/Cases/`
Shell: host is **fish** (no heredocs, `set X …` not `X=…`); guests are bash. Check the prompt before pasting.

---

## 5. Remaining work (in order)

### 5.1 Confirm unverified steps, then commit
1. pcmanfm: `pkill -x pcmanfm`, re-check the three keys are `0`, `pcmanfm -d &; disown`, plug a test key → `findmnt | grep /run/media` prints nothing.
2. Windows baseline after the sshd revert + Thumbcache Viewer: `dfir-lab.sh stop win; and dfir-lab.sh baseline win` (check the timestamp in today's `notes.md` or `qemu-img snapshot -l`).
3. Dotfiles repo has unrelated uncommitted changes (`.scripts/dm-logout` deleted, `.dinit/…/.KEEP` emptied, `packages` rewritten) — decide separately; `git restore .scripts/dm-logout` if unintended.
4. Commit only forensics paths:
   ```fish
   cd ~/projects/dotfiles
   git add .forensics/bin/dfir-lab.sh .forensics/bin/vol2-dfir.sh .forensics/bin/fetch-labs.sh .forensics/iso/SHA256SUMS .forensics/docs/LAB.md
   git diff --cached --stat
   git commit -m "forensics: vol2 guest, atomic OVA fetch, frozen lab hashes"; and git push
   rsync -a --delete ~/projects/dotfiles/.forensics/ $DF/scripts/
   ```

### 5.2 End-to-end tests (first real use of `usb`, `dump`)
1. ~~**Memory, Vol2 (03.1)**: stuxnet pslist~~ ✅ 2026-10-10.
2. **Disk imaging (01)**: `dfir-lab.sh start win sift` → plug test key → `dfir-lab.sh usb` → `usb attach <bus> <addr>` → FTK Imager: Create Disk Image → Physical Drive → E01 → `C:\Cases` → `scp` to `evidence/` → `sha256sum` → SIFT: `sudo ewfmount /mnt/evidence/<x>.E01 /mnt/e01 && sudo mmls /mnt/e01/ewf1` → mount partition read-only with offset → `fls`/`icat`. Compare with FTK's report.
3. **Memory dump + Vol3 (03.1)**: `dfir-lab.sh start --net` → `dfir-lab.sh dump win` → SIFT: `vol -f /mnt/cases/<date>/mem/<file>.elf windows.info` and `windows.pslist`.
4. **Linux memory (03.2)**: extract `victoria-v8.memdump.img.zip` like stuxnet → Vol2: `vol.py -f … --profile=LinuxDebian5010x86 linux_cpuinfo`.
5. **Registry / event logs (02)**: `rip.pl -r <hive> -p <plugin>`, `evtxinfo` on `hives-01`, `eventlogs-01`; PECmd on `prefetch.zip` in Windows.
6. **Network (04.2)**: `tcpdump -n -r /mnt/evidence/labs/04_network/lotsofweb.pcap -c 12`.
7. **Revert test**: `dfir-lab.sh stop && dfir-lab.sh start --clean` → Windows boots without BitLocker/TPM prompts, keys still work; `start --clean vol2` → 9p mounted, profile present.

### 5.3 Housekeeping
- Reboot once → `sudo dinitctl status limine-conf-watch` must be STARTED; if not, investigate the `/var` mount ordering.
- Disk: `sudo btrfs filesystem usage /` → if unallocated < 5 GiB: `sudo btrfs balance start -dusage=30 /`; prune old snapper snapshots. Extracted memory images add ~0.5–1 GiB each.
- Cleanup: `~/vm/ubuntu-12-i386`, `ubuntu-12.04.3-desktop-i386.iso` if unused; `unattend.iso` / `setup.iso` contain the analyst password in plaintext.
- Windows Update: pick one — update + re-baseline occasionally, or freeze for the course.
- Optional: `sift-dfir.sh` comment still says "Windows guest uses 6G" (it's 4 GiB).
- Optional: test `bootstrap/host.sh` on a scratch path; add Vol2 (OVA convert, initramfs modules, profile) to it.

---

## 6. Troubleshooting reference (all hit during the build)

| Symptom | Cause | Fix |
|---|---|---|
| Windows setup: "PC must support Secure Boot" | non-secboot OVMF | secboot OVMF + `smm=on` + secure pflash |
| Boot menu shows only "UEFI Misc Device" / PXE | install never completed | wipe qcow2, `win11-dfir.sh install` |
| OOBE asks for network / Wi-Fi driver | no NIC in install mode; unattend not applied | Shift+F10 → `start ms-cxh:localonly` |
| Empty CD drive in Windows | QEMU default IDE CD | `change ide2-cd0 /abs/path.iso` on the monitor |
| `set VM …` paths resolve to `/` | command run in bash, not fish | full paths, or fish |
| `D=…`: "Unsupported use of '='" | bash syntax pasted into fish; fish rejects the whole multi-line paste, nothing runs | `set D …`, or run in `bash` |
| Guest commands run on the host | pasted into the wrong shell | check the prompt (`vagrant@mint19`, `sansforensics@…`) first |
| `Add-WindowsCapability OpenSSH.Server` silent 15+ min | Windows Update FoD download | wait, or winget `Microsoft.OpenSSH.Preview` |
| `hostfwd_add` "could not set up" | forward already exists | ignore |
| SSH to Windows hangs (no refuse) | firewall drops on Public profile | `netsh … profile=any` |
| `Get-NetFirewallRule` 0x80070534 | rules owned by deleted user SID | remove orphan rule values, reboot |
| `Restart-Service mpssvc` refused | protected service | reboot |
| `dfir-lab.sh keys` hangs after password | PowerShell 5.1 reads redirected stdin | `ssh -n`, `-InputFormat None` |
| Key offered, `Failed publickey`, nothing logged | junk lines in `administrators_authorized_keys` | ASCII, key lines only, ACL Administrators+SYSTEM |
| `Remove-Item sshd.log` silently does nothing | current session's sshd holds the file open (hidden by `-ErrorAction SilentlyContinue`) | restart sshd, reconnect, then delete |
| `pidwait …; and qemu-img snapshot` skipped | `pidwait` exits 1 if nothing matches | run the snapshot unconditionally |
| `fstab` lines doubled | block pasted twice | `awk '!(/^(evidence|cases) / && seen[$0]++)'` |
| GPG: "Inappropriate ioctl for device" | `GPG_TTY` unset | `set -gx GPG_TTY (tty)` in fish config |
| `limine-conf-watch` STOPPED at boot | logfile ENOENT in `/var` | manual start; root cause open |
| OVA: `tar: Unexpected EOF`, `qemu-img: Invalid footer` | interrupted download left a partial file; old `[[ -e ]]` check skipped it | `.part` + resume + `tar -tf` check before rename (now in `fetch-labs.sh`) |
| `fetch-labs.sh` prints `skip <chap>/<code>` but files have real names | `.<code>.done` marker + `curl -OJ` | expected |
| Vol2 boot: `!! 9p shares not mounted`; journal: `mount: bad option` | `9pnet_virtio` not loaded yet when systemd mounts → `trans=virtio` rejected | `9p 9pnet 9pnet_virtio` in `/etc/initramfs-tools/modules`, `update-initramfs -u` |
| `sha256sum: __MACOSX: Is a directory` | macOS resource forks in course zips | `unzip -x '__MACOSX/*'` |
