# Forensics lab — build log and remaining work

Course: Digital Forensics (cylab.be slides, v2025-12-16). Stated requirement: *a Windows machine + the SIFT Workstation*.
Implementation: Artix host `penthrite`, three QEMU/KVM guests, case data in an encrypted vault, everything scripted and versioned in `~/projects/dotfiles/.forensics` (a subdirectory of the dotfiles repo). A second machine (Arch desktop, §7) carries an identical copy.

Status as of 2026-10-19: **setup complete and validated end to end** on `penthrite` (all six checks of §5.2). Rebuild path updated with the 10-19 fixes (§5.1). Second machine: guests, ISOs and key copied 2026-10-19 (all verified); large vault files and first start pending (§7). Lesson-day routine: §4.

---

## 1. Architecture

```
 penthrite (Artix, dinit, Limine + sbctl Secure Boot, LUKS2 + btrfs, 14 GiB RAM, Ryzen 7 8845HS)
 │
 ├─ ~/vm/iso/                      ISOs/OVAs (CoW, checksummed)          hashes: .forensics/iso/SHA256SUMS
 ├─ ~/vm/images/  (+C nodatacow)   own btrfs subvolume → not in snapper @home snapshots
 │   ├─ win11-dfir/  win11.qcow2, OVMF_VARS.4m.fd, tpm/, *.baseline, unattend.iso, setup.iso, mon.sock
 │   ├─ sift/        sift.qcow2, mon.sock
 │   └─ vol2/        vol2.qcow2 (1 TiB virtual, ~7 GiB allocated), mon.sock
 │
 ├─ Cryptomator vault (FUSE)  DF=~/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2
 │   ├─ *_slides.pdf
 │   ├─ scripts/    rsync copy of .forensics (read-only mirror; edit in ~/projects)
 │   ├─ evidence/   → SIFT + Vol2 /mnt/evidence  (9p, read-only)
 │   │    ├─ labs/<chapter>/            downloads
 │   │    ├─ labs/<chapter>/extracted/  unzipped on the host (03_memory, 02_windows, 04_network)
 │   │    └─ received/<date>/           files pulled out of guests with `get` (E01s, CSVs)
 │   └─ cases/      → SIFT + Vol2 /mnt/cases     (9p, rw)
 │        ├─ <date>/{notes.md,hashes.txt,mem/,02_windows/}
 │        ├─ vol3-symbols/                  Vol3 symbol cache (vol -s), survives --clean
 │        ├─ hashes-labs.2026-10-10.txt     frozen reference (444)
 │        ├─ hashes-labs.txt                recomputed each fetch-labs run (downloads only), verified against the reference
 │        └─ hashes-labs-extracted.txt      hashes of everything unzipped on the host
 │
 ├─ Windows 11 Pro 26H2  (win11-dfir.sh)   4 GiB, 6 vCPU   SSH 127.0.0.1:2222  analyst
 │     FTK Imager 8.3, Eric Zimmerman tools (C:\Tools\EZ\net9), .NET 9, Thumbcache Viewer 1.0.4.0
 │     No S3/S4 (QEMU ICH9-LPC disable_s3/s4 + powercfg). Clock: TZ UTC, RealTimeIsUniversal=1.
 ├─ SIFT 2026-04-22      (sift-dfir.sh)    4 GiB, 4 vCPU   SSH 127.0.0.1:2223  sansforensics
 │     Sleuth Kit, ewf-tools, RegRipper (rip.pl), libevtx, Volatility 3 2.27.0, tcpdump
 └─ Vol2 (Mint 19.3)     (vol2-dfir.sh)    4 GiB, 2 vCPU   SSH 127.0.0.1:2224  vagrant
       Volatility 2.6.1 in /opt/volatility, profile LinuxDebian5010x86 — run instead of Windows

 Network modes:  run = slirp restrict=on (no internet, SSH forward only)  |  net = slirp NAT with internet

 Second machine (§7): Arch desktop, devid@192.168.42.2 over direct Ethernet, Ryzen 5 2600, 15 GiB RAM,
 ext4 /home. Same user, same paths, same vault (synced through its own git repo; files ≥100 MiB copied by
 dfir-push.sh), guests copied with dfir-push.sh.
```

Why this shape:
- **QEMU over VMware/VirtualBox** for the course guests: scriptable monitor (snapshots, memory dumps, ISO/USB hot-plug), no out-of-tree modules to sign under Secure Boot.
- **Evidence read-only at the hypervisor layer** (9p `readonly=on`, `-drive readonly=on`): the guest cannot write even if a tool tries (NTFS log replay, Windows indexing).
- **Isolated by default**: course images contain live malware (stuxnet, cridex, prolaco in the memory chapter).
- **Defender off in Windows**: real-time protection quarantines/alters artefacts in evidence and exports.
- **VBS off** (`-cpu host,-svm`): no virtualization-based security inside the guest → simpler, standard memory images.
- **Separate Vol2 guest**: the slides use Volatility 2 syntax and profiles; this SIFT release ships only Volatility 3.
- **Guest clock in UTC, no guest sleep**: a lab VM must produce correct UTC timestamps and stay reachable over SSH.

---

## 2. Course chapters → what each needs → where it runs

| Chapter (slides) | Tools / data | Runs on | Status |
|---|---|---|---|
| 00 Preamble | SIFT, Windows | both | ✅ |
| 01 Disks | FTK Imager 8.3 (USB key → E01), `ewfmount`, `mmls`, `fls`, `icat`; `usb-01…06` | Windows (imaging) + SIFT | ✅ 2026-10-12: test key imaged read-only, MD5/SHA1 identical on key, FTK, host and `ewfverify` |
| 02 Windows | RegRipper (`hives-01`), `evtxexport` (`eventlogs-01`), PECmd (`prefetch.zip`), Thumbcache Viewer (`thumbcache-01`, cylab.be/s/XaVkj) | SIFT + Windows | ✅ 2026-10-19 (Thumbcache Viewer GUI not exercised yet) |
| 03.1 Memory (Windows) | Vol2 `--profile=WinXPSP2x86` on course images; own VM dump + Vol3 | Vol2 + SIFT | ✅ stuxnet pslist (10-10), `dump win` + Vol3 `windows.info/pslist` (10-19); cridex/prolaco not run yet (same workflow) |
| 03.2 Memory (Linux) | Vol2 profile `LinuxDebian5010x86`, victoria-v8 | Vol2 | ✅ 2026-10-10 |
| 04.1 Command line | grep/regex, `/proc/cpuinfo`, `lorem.txt` | SIFT | ✅ tools. `lorem.txt` has **no download link in the slides** (all 40 checked); the slides' `wc` gives `39 1939 13222` — ask the teacher for the file |
| 04.2 tcpdump | `lotsofweb.pcap`, `treasurehunt_fw_eth1.pcap` (zip in disguise → `extracted/`) | SIFT | ✅ 2026-10-19: slide 44 filter → 50 echo requests |
| Android memory (paper) | reading | — | ✅ |

---

## 3. What was done

### 3.1 Host assessment (`bin/dfir-assess.sh`, read-only)
- KVM usable: AMD-V, nested paging + nested virt on, `/dev/kvm` 0666. AVIC unsupported (firmware), irrelevant.
- `kvm_amd` and VMware `vmmon`/`vmnet` coexist; never run QEMU and VMware guests at the same time.
- Secure Boot on (sbctl, custom keys), lockdown none → no impact on in-tree KVM.
- RAM 14 GiB usable (780M iGPU carve-out) → 4 GiB guests, at most two at a time plus headroom.
- Host automount: `~/.config/pcmanfm/default/pcmanfm.conf` → `mount_on_startup=0`, `mount_removable=0`, `autorun=0`. **Verified 2026-10-12**: test key plugged in, no mount under `/run/media`.
- Hardware clock UTC, ntpd running.

### 3.2 Windows guest
- **ISO**: Windows 11 consumer multi-edition, 26H2 (build 26300.9457), English x64, SHA-256 `bd4307df…ea650` verified against microsoft.com. Rejected: Enterprise evaluation (90 days → expires mid-exams). Installed edition: Pro, no key (unactivated: watermark only).
- **Firmware**: 26H2 setup refuses non-Secure-Boot-capable firmware → `OVMF_CODE.secboot.4m.fd` + `-machine smm=on` + `-global driver=cfi.pflash01,property=secure,value=on`; per-VM `OVMF_VARS.4m.fd`.
- **TPM 2.0**: `swtpm` socket started by the launcher, exits with QEMU.
- **Disk**: 80 GiB qcow2 (metadata prealloc) on virtio-blk; `viostor\w11\amd64` loaded during setup.
- **Answer file** (`guest/windows/autounattend.xml`, template with `changeme`; `host.sh` fills the password into `unattend.iso` at build time, so the repo holds no password): BitLocker auto-encryption off, BypassNRO, local admin `analyst`, `<TimeZone>UTC</TimeZone>` in the `oobeSystem` Shell-Setup component. The time zone was **not applied**: OOBE was bypassed with `ms-cxh:localonly`, which most likely skipped the pass's settings, leaving the en-US default (Pacific). `setup.ps1` now sets it explicitly.
- **Drivers**: `virtio-win-guest-tools.exe`, loaded by hot-swapping the ISO into the empty default CD drive (`change ide2-cd0 …`).
- **Hardening** (`guest/windows/setup.ps1`, delivered as a hot-swapped ISO because the GTK display has no clipboard):
  - `analyst` sole admin; orphaned firewall rules of the deleted user removed (0x80070534).
  - Defender: Tamper Protection off (GUI), real-time monitoring off by policy, exclusions `C:\Cases`, `C:\Tools`.
  - `C:\Windows\Panther\unattend.xml` deleted. VBS status 0.
  - .NET 9 Desktop Runtime, Eric Zimmerman tools in `C:\Tools\EZ\net9`, FTK Imager 8.3.
  - Thumbcache Viewer 1.0.4.0 in `C:\Tools\ThumbcacheViewer`. `thumbcache_viewer_64.zip` SHA-256 `8d91a3156318ed26df11202a86dc0b53e2d85fd7e2cb02caf67f2fb63006e5ed`.
  - OpenSSH Server, DefaultShell PowerShell, firewall rule profile any.
  - Power + clock (since commit `1b0ce5b`, 2026-10-19): powercfg timeouts 0, hibernate off, `Set-TimeZone -Id UTC`, `RealTimeIsUniversal=1`, `tzautoupdate` disabled.
- **SSH key auth**: `C:\ProgramData\ssh\administrators_authorized_keys` rewritten as ASCII, key lines only, ACL Administrators/SYSTEM. Key `~/.ssh/id_dfir_lab`. sshd debug logging reverted.
- **Baseline**: 2026-10-19 14:14:04 (includes power + clock fixes; verified by `start --clean` at 15:46).

### 3.3 SIFT guest
- `sift-2026-04-22.ova` → vmdk → qcow2. BIOS boot, virtio-blk. OpenSSH, key auth.
- 9p fstab: `evidence` ro, `cases` rw, `msize=262144`, `nofail`.
- Tools: `vol` (Volatility 3 2.27.0), `mmls fls icat fsstat ewfmount ewfverify ewfinfo rip.pl evtxinfo evtxexport tcpdump`. No `vol.py`.
- Vol3 cannot write downloaded symbols into its root-owned install dir → always `vol -s /mnt/cases/vol3-symbols …`. Cache holds `windows/ntkrnlmp.pdb/C29EBFB06B78B3C020DCA66D99713F9E-1.json.xz` (the lab's Windows 11 kernel); survives `--clean` because it lives on the share.
- Baseline 2026-10-10 10:24 (contains the key). Do **not** run `do-release-upgrade` (26.04 offered in the banner).

### 3.4 Case data
- `fetch-labs.sh` fetched 16 lab files into `evidence/labs/{01_disks,02_windows,03_memory,04_network}`. Short-link entries (`cylab.be/s/<code>`) are saved with the server's filename (`curl -OJ`) and tracked by hidden `.<code>.done` markers.
- Integrity: frozen reference `cases/hashes-labs.2026-10-10.txt` (chmod 444); `sha256sum -c` all OK; `unzip -t` all zips OK; `ewfverify` all E01s OK.
- Extracted on the host (hashes appended to `cases/hashes-labs-extracted.txt`):
  - `03_memory/extracted/stuxnet.vmem` (512 MiB) SHA-256 `5f19ff13…aeefe77`; victoria-v8 SHA-256 `dd225a5a…eaf1a7ec`.
  - `02_windows/extracted/hives/{system,software,SAM,SECURITY,default,Users/IEUser/NTUSER.DAT,Users/Default/NTUSER.DAT}` (lowercase names as shipped; no .LOG1/.LOG2) and `02_windows/extracted/eventlogs-01/{Application,Security,System}.evtx`.
  - `04_network/extracted/treasurehunt_fw_eth1.pcap` (380,018,256 bytes, pcap 2.4). The downloaded `treasurehunt_fw_eth1.pcap` is a **zip archive** with the same inner name.
- Extract with `unzip -n <zip> -x '__MACOSX/*' -d …/extracted`. Never unzip onto a guest disk.

### 3.5 Scripts and versioning — `~/projects/dotfiles/.forensics`
| File | Purpose |
|---|---|
| `bin/dfir-lab.sh` | lesson runner for `win`, `sift`, `vol2`: `start [--net] [--clean] [guests]`, `stop`, `status`, `ssh`, `put`, `get` (hash + note), `usb` (`attach-ro`, raw `attach`, `detach`), `dump`, `keys`, `baseline`. Default guests: win + sift, plus vol2 if running |
| `bin/win11-dfir.sh` | Windows launcher: `install` / `run` / `net`; `-global ICH9-LPC.disable_s3=1 -global ICH9-LPC.disable_s4=1` |
| `bin/sift-dfir.sh` | SIFT launcher: `run` / `net`, 9p shares, refuses to start if the vault is locked |
| `bin/vol2-dfir.sh` | Vol2 launcher: `run` / `net`, same 9p shares, port 2224, `DISK_BUS=ahci` fallback |
| `bin/fetch-labs.sh` | lab files + Vol2 OVA. OVA: `.part` + resume + `tar -tf` check before rename. Flags files whose content doesn't match the extension (prints the `unzip` command). Hashes downloads only (not `extracted/`), verified against the frozen reference |
| `bin/dfir-push.sh` | copy disks (with baselines), Windows VARS/TPM, lab ISOs and the guest SSH key to another machine with the same layout (§7). `--vault-only`: the vault files too large for its git sync, compared by sha256 and resent when they differ |
| `bin/dfir-assess.sh` | read-only host check |
| `bootstrap/host.sh` | rebuild from zero (idempotent; never overwrites disks/VARS/TPM). `--no-guests`: packages, layout, launchers only (target of `dfir-push.sh`). Layout works on btrfs (subvolume + nodatacow) and ext4. Builds `unattend.iso` only for a fresh Windows disk |
| `guest/windows/{autounattend.xml,setup.ps1}` | Windows install + hardening (incl. power + clock) |
| `guest/sift/setup.sh` | 9p fstab, RegRipper fix if needed, tool check |
| `guest/vol2/setup.sh` | Vol2 rebuild: key-only SSH, 9p fstab, initramfs modules, text console, Debian5010 profile (untested replay of the 10-10 manual steps) |
| `iso/SHA256SUMS` | Windows ISO, SIFT OVA, virtio-win, Mint Vol2 OVA |
| `docs/LAB.md` | this file |

`~/.local/bin/*.sh` are symlinks into `bin/`. Commits GPG-signed. The vault `scripts/` copy is refreshed with `rsync -a --delete ~/projects/dotfiles/.forensics/ $DF/scripts/`.

Never in git (`.gitignore`): disks, ISOs/OVAs, firmware (`*.fd`), TPM state, evidence, the SSH private key, `unattend.iso`.

### 3.6 Vol2 guest
- `mint-19.3-volatility.ova` (cylab.be/s/jfp0o, VirtualBox 6.0.14 export): SHA-256 `c5dcf541…19bc`. OVF: BIOS, SATA/AHCI, 2 vCPU, 4096 MB.
- Converted to qcow2 (metadata prealloc). Boots on virtio-blk / virtio-net / virtio-vga, kernel 5.0.0-32-generic, `multi-user.target` (no Cinnamon) so ACPI shutdown works.
- User `vagrant`, key-only SSH. 9p via initramfs modules `9p 9pnet 9pnet_virtio`.
- Volatility 2.6.1 in `/opt/volatility`, `Debian5010.zip` → `LinuxDebian5010x86`.
- Validated: stuxnet `pslist` (3× lsass.exe), victoria-v8 `linux_cpuinfo` (Core2 T7200).
- Baseline 2026-10-10 13:00:57; `start --clean vol2` verified 2026-10-19.

### 3.7 Host fixes along the way
- `limine-conf-watch` failed with ENOENT opening `/var/log/limine-conf-watch.log` at boot; started manually, `sbctl verify` all signed. Root cause unconfirmed (§5.3).

### 3.8 Fixes found during end-to-end testing (2026-10-12 → 10-19)
- **Windows went to sleep when idle** (Kernel-Power 42, "System Idle", ~1 h after FTK finished): black screen, SSH reset at key exchange, USB attachment lost when the window was closed. Fix: `powercfg` timeouts 0 + hibernate off in the guest, and `-global ICH9-LPC.disable_s3=1 -global ICH9-LPC.disable_s4=1` in the launcher. `powercfg /a` now lists no sleep state.
- **Windows UTC clock 7 h ahead**: the Vol3 `SystemTime` of the 10-12 dump read 03:01 UTC for a dump taken at 20:01 UTC. Mechanism: QEMU `-rtc base=utc` + Windows reading the RTC as local time + TZ Pacific Daylight (UTC−7). On-screen times looked right; every stored UTC timestamp was +7 h. Fix: `Set-TimeZone -Id UTC`, `RealTimeIsUniversal=1`, `tzautoupdate` disabled. Verified 10-19: guest UTC = host `date -u`. Dumps taken before 10-19 carry the skew.
- **Vol3 symbols not writable** → `-s /mnt/cases/vol3-symbols`.
- **`treasurehunt_fw_eth1.pcap` is a zip** (`PK\3\4`, inner name identical) → extracted to `04_network/extracted/`; `fetch-labs.sh` now flags this.

---

## 4. Daily use

Script: `~/projects/dotfiles/.forensics/bin/dfir-lab.sh` (edit here), run as `dfir-lab.sh` via the symlink in `~/.local/bin`. Same on both machines.

### Lesson day

1. Unlock the Cryptomator vault.
2. Start what the chapter needs (table below). `start` prints the lesson dir, creates `cases/<date>/notes.md`, and checks SSH, 9p shares and Defender.
3. Work. Files: `put` into a guest, `get` out of it. `get` lands in `evidence/received/<date>/` (or `cases/…`), is hashed into `cases/<date>/hashes.txt`, and logged in `notes.md`. One `get` with a glob is enough (`'C:/Cases/x.E0*'` already includes `x.E01.txt`).
4. Before stopping: `get` anything you produced inside Windows (`C:\Cases`) — a later `--clean` erases it.
5. `dfir-lab.sh stop`. Then `rsync -a --delete ~/projects/dotfiles/.forensics/ $DF/scripts/` if you changed scripts.

Run the lab on **one machine at a time**. The `synthesis` vault syncs through its own git repo (encrypted files, pushed to GitHub); `gitfit` keeps every file whose ciphertext is ≥100 MiB out of git. Before switching machines:
1. On the machine you used: in the vault repo, `gitfit`, `git commit`, `git push`; then `dfir-push.sh <other> --vault-only` for the large files (E01s, dumps, extracted images).
2. On the other machine: `git pull` in the vault repo, unlock the vault.

| Chapter | Start | Typical commands |
|---|---|---|
| 01 Disks | `start` (win + sift) | `usb` → `usb attach-ro /dev/sda` → Windows `Get-Disk` shows `IsReadOnly True` → FTK Imager E01 to `C:\Cases` → `get win 'C:/Cases/<name>.E0*'` → `usb detach` → host `sudo md5sum /dev/sda` = FTK MD5 → SIFT `ewfverify`, `ewfmount … /mnt/e01`, `mmls /mnt/e01/ewf1`, `fsstat/fls -o <start>` |
| 02 Windows | `start` | SIFT: `rip.pl -r …/extracted/hives/system -p compname` (or `-a > file`), `evtxexport …/Security.evtx > /mnt/cases/<date>/…`; Windows: `put win prefetch.zip`, `C:\Tools\EZ\net9\PECmd.exe -d … --csv C:\Cases\out` |
| 03.1 Memory (Win) | `start vol2 sift` (stop win first) | Vol2: `vol.py -f /mnt/evidence/labs/03_memory/extracted/<img> --profile=…`; own dump: `start --net win sift` → `dump win` → SIFT `vol -s /mnt/cases/vol3-symbols -f /mnt/cases/<date>/mem/<f>.elf windows.pslist` |
| 03.2 Memory (Linux) | `start vol2` | `vol.py … --profile=LinuxDebian5010x86 linux_…` |
| 04.1 Command line | `start sift` | SIFT shell |
| 04.2 tcpdump | `start sift` (or `--net` for macvendors) | `tcpdump -n -r /mnt/evidence/labs/04_network/lotsofweb.pcap`; treasurehunt: `/mnt/evidence/labs/04_network/extracted/treasurehunt_fw_eth1.pcap` |

### Reference

```fish
dfir-lab.sh status                # guests, isolated/INTERNET, disk, lesson dir
dfir-lab.sh start --clean         # revert to baselines first (discards guest-side changes)
dfir-lab.sh ssh sift              # or win | vol2
dfir-lab.sh put win ./file        # → C:/Cases/
dfir-lab.sh get win 'C:/Cases/x'  # quote globs in fish
dfir-lab.sh usb                   # host USB devices + USB disks
dfir-lab.sh usb attach-ro /dev/sdX   # evidence disk, write-protected for Windows
dfir-lab.sh dump win              # → cases/<date>/mem/*.elf + sha256
dfir-lab.sh stop                  # every running guest
dfir-lab.sh baseline win          # baseline: name the guest(s), they must be stopped
dfir-push.sh devid@192.168.42.2   # copy the lab to the second machine (§7)
```
Network mode is per boot: stop a guest to switch between isolated and `--net`.
Shell: the host terminal is either **fish** or **bash** — check the prompt. Fish: `set X …`, `(cmd)` substitution, no heredocs, one bad line rejects the whole multi-line paste. Bash: `X=…`, `$(cmd)`. Guests are bash; Windows over SSH is PowerShell. Never paste a line with a `<placeholder>` left in it: fish rejects the paste, bash parses `<x>` as a redirection.

---

## 5. Remaining work

### 5.1 Rebuild path — done 2026-10-19
1. ✅ `setup.ps1`: power + UTC clock (commit `1b0ce5b`).
2. ✅ `autounattend.xml`: cause understood (§3.2); `setup.ps1` covers it.
3. ✅ `fetch-labs.sh`: content-vs-extension check, `extracted/` excluded from `hashes-labs.txt`.
4. ✅ `host.sh`: `--no-guests`, ext4/btrfs layout, Vol2 OVA conversion, `unattend.iso` only for a fresh disk. `guest/vol2/setup.sh` added. `dfir-push.sh` added.
5. Commit these, push, `git pull` on the second machine (§7).

### 5.2 End-to-end tests — all passed on penthrite
1. ✅ Automount (10-12). 2. ✅ Commit of revised `dfir-lab.sh` (10-10, `f63e4c2`). 3. ✅ USB imaging, hashes identical end to end (10-12). 4. ✅ `dump win` + Vol3 (10-19). 5. ✅ RegRipper, evtxexport, PECmd, tcpdump (10-19). 6. ✅ `start --clean` on win, sift, vol2 (10-19).

### 5.3 Housekeeping
- Reboot once → `sudo dinitctl status limine-conf-watch` must be STARTED; if not, investigate the `/var` mount ordering.
- Disk: `sudo btrfs filesystem usage /` → if unallocated < 5 GiB: `sudo btrfs balance start -dusage=30 /`. `evidence/received/2026-10-12/usb-test.E0*` is 4.4 GB — delete when no longer needed.
- `~/vm/images/win11-dfir/unattend.iso` (analyst password in plaintext) can be deleted: `host.sh` rebuilds it only for a fresh disk. `setup.iso` holds no secret and is rebuilt on every `host.sh` run.
- Not part of the lab: `~/vm/ubuntu-12-i386`, `ubuntu-12.04.3-desktop-i386.iso`, Arch and CachyOS ISOs.
- Dotfiles repo: unrelated uncommitted changes (`.scripts/dm-logout`, `.dinit/…/.KEEP`, `packages`) — decide separately.
- Windows Update: pick one — update + re-baseline occasionally (then re-push, §7), or freeze for the course.
- `lorem.txt` (04.1): ask the teacher; save it to `evidence/labs/04_cli/`.
- Optional: `sift-dfir.sh` comment still says "Windows guest uses 6G" (it's 4 GiB).

---

## 6. Troubleshooting reference (all hit during the build)

| Symptom | Cause | Fix |
|---|---|---|
| Windows setup: "PC must support Secure Boot" | non-secboot OVMF | secboot OVMF + `smm=on` + secure pflash |
| Boot menu shows only "UEFI Misc Device" / PXE | install never completed | wipe qcow2, `win11-dfir.sh install` |
| OOBE asks for network / Wi-Fi driver | no NIC in install mode; unattend not applied | Shift+F10 → `start ms-cxh:localonly` |
| Empty CD drive in Windows | QEMU default IDE CD | `change ide2-cd0 /abs/path.iso` on the monitor |
| `D=…`: "Unsupported use of '='" | bash syntax pasted into fish; fish rejects the whole multi-line paste | `set D …`, or run in `bash` |
| bash: "syntax error near unexpected token" on a line with `cp (…)` | fish command substitution pasted into bash | `$(…)` in bash |
| fish: "Expected a string, but found end of the statement"; nothing in the paste ran | a `<placeholder>` left in one line | replace placeholders, paste again |
| bash: `file: No such file or directory`, then `Is a directory` | `F=…/<file>.elf` — bash parsed `<file` as a redirection | use the real filename |
| Guest commands run on the host | pasted into the wrong shell | check the prompt first |
| `Add-WindowsCapability OpenSSH.Server` silent 15+ min | Windows Update FoD download | wait, or winget `Microsoft.OpenSSH.Preview` |
| SSH to Windows hangs (no refuse) | firewall drops on Public profile | `netsh … profile=any` |
| `Get-NetFirewallRule` 0x80070534 | rules owned by deleted user SID | remove orphan rule values, reboot |
| `dfir-lab.sh keys` hangs after password | PowerShell 5.1 reads redirected stdin | `ssh -n`, `-InputFormat None` |
| Key offered, `Failed publickey`, nothing logged | junk lines in `administrators_authorized_keys` | ASCII, key lines only, ACL Administrators+SYSTEM |
| Windows black screen; SSH `kex_exchange_identification: Connection reset by peer`; status "sshd not answering" | guest entered S3 sleep when idle (Kernel-Power 42) | now prevented (§3.8). If it recurs: `echo system_wakeup \| socat - UNIX-CONNECT:$HOME/vm/images/win11-dfir/mon.sock` — don't close the window (hard power-off, USB attachment lost) |
| `usb detach`: "nothing attached" | attachment died with the QEMU process | re-attach after restart |
| Windows-side UTC timestamps +7 h (Vol3 `SystemTime`, process CreateTime) | RTC is UTC, Windows read it as local time with TZ Pacific | TZ UTC + `RealTimeIsUniversal=1` (§3.8) |
| Vol3: "Cannot write necessary symbol file", `symbol_table_name` unsatisfied | symbol dir in `/opt` is root-owned | `vol -s /mnt/cases/vol3-symbols …` (needs `--net` the first time for a new kernel) |
| Vol3 `PE TimeDateStamp` in 2104 | reproducible-build hash, not a date | ignore |
| Vol3 traceback `BrokenPipeError` | `\| head` closed the pipe | ignore |
| tcpdump: "unknown file format" on `treasurehunt_fw_eth1.pcap` | file is a zip (`PK\3\4`) | `unzip -n … -d …/extracted` on the host |
| tcpdump shows `ack 4294962952` | capture starts mid-stream; relative numbering wraps | `-S` for absolute numbers |
| `get`: "already in … new copy left in .incoming.*" | file fetched twice (glob already matched it) | `rm -r` the `.incoming.*` dir on the host |
| evtxinfo: "Is dirty / Is corrupted" | log copied from a live system | records still parse; normal for collected logs |
| `pidwait …; and qemu-img snapshot` skipped | `pidwait` exits 1 if nothing matches | run the snapshot unconditionally |
| GPG: "Inappropriate ioctl for device" | `GPG_TTY` unset | `set -gx GPG_TTY (tty)` in fish config |
| `limine-conf-watch` STOPPED at boot | logfile ENOENT in `/var` | manual start; root cause open |
| OVA: `tar: Unexpected EOF`, `qemu-img: Invalid footer` | interrupted download left a partial file | `.part` + resume + `tar -tf` check (in `fetch-labs.sh`) |
| Vol2 boot: `!! 9p shares not mounted` | `9pnet_virtio` not in initramfs | `9p 9pnet 9pnet_virtio` in `/etc/initramfs-tools/modules`, `update-initramfs -u` |
| `sha256sum: __MACOSX: Is a directory` | macOS resource forks in course zips | `unzip -x '__MACOSX/*'` |
| Vol2 ignores `stop` | Cinnamon intercepts ACPI power button | `systemctl set-default multi-user.target` |
| Bare `dfir-lab.sh start` / `stop` exits silently | `set -e` + `running vol2 && …` returns 1 | `if running vol2; then …; fi` (fixed 2026-10-10) |
| `dfir-push.sh`: "differs on the target (size/mtime)" | a baseline was re-taken on one machine | `--force` from the machine whose copy you want to keep |
| `dfir-push.sh`: "missing on target: qemu-img…" / launchers not linked | target not bootstrapped | `bash ~/projects/dotfiles/.forensics/bootstrap/host.sh --no-guests` there |
| A lab file present on one machine is missing on the other (first seen: `stuxnet.vmem.zip` on nitropenta) | the vault syncs through git and `gitfit` ignores files ≥100 MiB | `dfir-push.sh <other> --vault-only` |
| `dfir-push.sh --vault-only` printed `sending 0` and finished instantly; the copies on the target are all zeros (2026-10-19, first version) | the Cryptomator FUSE mount reports 0 allocated blocks per file, and `tar --sparse` archives a 0-block file as one big hole | fixed: plain tar for vault files, and every large vault file compared by sha256 on both machines; a differing copy is resent. Rerun `dfir-push.sh <other> --vault-only` |

---

## 7. Second machine and long-term preservation

**Target**: Arch desktop, `devid@192.168.42.2` (direct Ethernet), Ryzen 5 2600 (AMD → `-cpu host,-svm` unchanged; Zen+ is on the Windows 11 CPU list), 15 GiB RAM (~7 GiB free with the desktop running: `dfir-lab.sh start` needs 8 GiB for win + sift), ext4 `/home` (no nodatacow subvolume needed), Cryptomator vault `synthesis` already mounted at the same path.

**What lives where**

| Item | Size | Kept by |
|---|---|---|
| Scripts, launchers, guest configs, hashes, this runbook | < 1 MiB | git (`nottoBD/dotfiles`, `.forensics/`) |
| `sift.qcow2` 19 GiB, `win11.qcow2` 25 GiB, `vol2.qcow2` 7.3 GiB (allocated), each with its internal `baseline` snapshot | ~51 GiB | both machines (`dfir-push.sh`) |
| Windows `OVMF_VARS*.fd`, `tpm*/tpm2-00.permall` | < 1 MiB, travel with the Windows disk | both machines |
| Lab ISOs/OVAs (Windows 8.5G, SIFT 8.9G, Mint 3.9G, virtio 0.8G) | ~22 GiB, pinned by `iso/SHA256SUMS` | both machines |
| `~/.ssh/id_dfir_lab` | — | both machines (never git) |
| Evidence, cases, Vol3 symbol cache (files < 100 MiB) | vault | vault git repo (`gitfit`, commit, push / pull) |
| Vault files ≥ 100 MiB: lab zips such as `stuxnet.vmem.zip`, `extracted/` images, E01s, memory dumps | several GiB | both machines (`dfir-push.sh --vault-only`) |

Over GitHub's limits (100 MiB per file, ~1 GB per repo recommended): disks and ISOs can never go to git. Two full copies on two machines are the backup; the ISOs make a rebuild possible even if a download link disappears.

**First copy**

1. Laptop: commit + push the repo.
2. Target: `git -C ~/projects/dotfiles pull --ff-only`, then `bash ~/projects/dotfiles/.forensics/bootstrap/host.sh --no-guests` (installs `qemu-desktop` — the target only had `qemu-base`, which lacks the GTK display — plus `socat`, `pv`; links the launchers).
3. Laptop: `dfir-lab.sh stop`, then `dfir-push.sh devid@192.168.42.2` (vault unlocked on both: the run ends with the large vault files). One SSH password prompt for the whole run (multiplexed connection). Transport: GNU `tar --sparse` over SSH — holes skipped (vol2 is 1 TiB apparent), files byte-identical, internal snapshots kept. Verifies on the target: `qemu-img check` + baseline present on each disk, VARS/TPM sha256, ISOs against `SHA256SUMS`, lab files present in the vault.
4. Target: unlock the vault, `dfir-lab.sh start --clean`, check the three guests as in §5.2-6, plug a test key and check nothing automounts.

**Keeping the copies in step**: the machine that last re-took a baseline is the master. After re-taking one: `dfir-push.sh <other> --no-iso --force`. After a lesson that produced big evidence: `dfir-push.sh <other> --vault-only`. Files with the same size and mtime are skipped, so re-runs only send what changed (a whole disk, since qcow2 isn't diffed).

**Portability notes for any further machine**: same user name and home path; Arch-family (`host.sh` uses pacman and `/usr/share/edk2/x64/`); on an **Intel** host change `-svm` to `-vmx` in `win11-dfir.sh`, otherwise Windows sees VT-x and may enable VBS.
