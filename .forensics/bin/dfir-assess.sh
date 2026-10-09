#!/usr/bin/env bash
# DFIR workstation assessment — READ-ONLY, changes nothing on the system.
# Usage:  bash dfir-assess.sh 2>&1 | tee ~/dfir-assess.txt

sec() { printf '\n===== %s =====\n' "$1"; }
sudo -v || exit 1

sec "SYSTEM"
uname -r
grep -E '^(NAME|ID)=' /etc/os-release
echo "cores: $(nproc)"
free -h

sec "CPU / VIRTUALIZATION"
lscpu | grep -E 'Model name|Virtualization|^CPU\(s\)|Thread\(s\)'
grep -qw svm /proc/cpuinfo && echo "svm flag: present" || echo "svm flag: MISSING (check BIOS)"
ls -l /dev/kvm 2>&1
echo "groups: $(id -nG)"
lsmod | grep -E '^(kvm|kvm_amd|vmmon|vmnet|vboxdrv)\b' || echo "no hypervisor modules loaded"
for p in nested avic sev; do
  printf 'kvm_amd.%s = ' "$p"
  cat "/sys/module/kvm_amd/parameters/$p" 2>/dev/null || echo n/a
done
sudo dmesg | grep -iE 'kvm|svm|AMD-Vi|iommu' | tail -n 20

sec "SECURE BOOT / LOCKDOWN"
command -v sbctl >/dev/null && sudo sbctl status
printf 'lockdown: '; cat /sys/kernel/security/lockdown 2>/dev/null || echo n/a

sec "STORAGE"
lsblk -o NAME,SIZE,TYPE,FSTYPE,RO,RM,TRAN,MOUNTPOINTS
findmnt -t btrfs -o TARGET,SOURCE,OPTIONS
sudo btrfs subvolume list /
sudo btrfs filesystem usage / | head -n 15
df -h / /home

sec "EXISTING VM ARTIFACTS"
find "$HOME" -maxdepth 4 \( -iname '*.qcow2' -o -iname '*.vmx' -o -iname '*.vmdk' \
  -o -iname '*.iso' -o -iname 'OVMF_VARS*' \) -exec du -h {} + 2>/dev/null
echo "-- CoW attribute (C = nodatacow) on dirs holding qcow2:"
find "$HOME" -maxdepth 4 -iname '*.qcow2' -printf '%h\n' 2>/dev/null | sort -u | xargs -r lsattr -d

sec "PACKAGES"
for p in qemu-full qemu-desktop qemu-base edk2-ovmf swtpm virtio-win samba \
         sleuthkit libewf testdisk ddrescue dc3dd guymager hashdeep bulk_extractor \
         python-pipx podman distrobox udisks2 ntfs-3g; do
  pacman -Q "$p" 2>/dev/null || echo "-- $p: not installed"
done
echo "-- vmware packages:"; pacman -Qqs vmware || echo none
echo "-- OVMF firmware files:"; ls /usr/share/edk2/x64/ 2>&1
echo "-- AUR helper:"; command -v paru yay pikaur 2>/dev/null || echo none
echo "-- enabled repos:"; pacman-conf --repo-list

sec "AUTOMOUNT / UDEV"
pgrep -a udisksd || echo "udisksd not running"
pgrep -af 'udiskie|gvfs|thunar|pcmanfm|nautilus' || echo "no automounter/file manager running"
ls -1 /etc/udev/rules.d/ 2>&1

sec "DINIT SERVICES"
sudo dinitctl list 2>&1 | head -n 60

sec "TIME"
date; date -u
echo "localtime -> $(readlink /etc/localtime)"
cat /etc/adjtime 2>/dev/null || echo "no /etc/adjtime (hwclock assumes UTC)"

sec "DONE"
