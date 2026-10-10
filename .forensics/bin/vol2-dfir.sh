#!/usr/bin/env bash
# vol2-dfir.sh — Volatility 2 guest (cylab.be "Mint 19.3 Volatility" OVA) under QEMU/KVM
#
#   run : isolated (no internet), SSH only:  ssh -p 2224 <user>@127.0.0.1
#   net : user-mode NAT with internet (first boot: apt install openssh-server)
#
# Chapters 03.1 / 03.2 (vol.py, --profile=..., Linux profiles).
# Run it INSTEAD of the Windows guest, not alongside SIFT + Windows (14 GiB host).
#
# Same 9p shares as SIFT:
#   evidence -> /mnt/evidence  (read-only)
#   cases    -> /mnt/cases     (read-write)
#
# Disk: imported from a VirtualBox SATA disk. The Mint 19.3 kernel has virtio-blk built in;
# if boot drops to an initramfs shell ("ALERT! UUID=... does not exist"), use:
#   DISK_BUS=ahci vol2-dfir.sh net
# Virtual size is 1 TiB (sparse, as created upstream): the guest's df says nothing about
# host space. Never unzip memory images onto the guest disk; watch `btrfs filesystem usage /`.
#
# Anything after the mode is appended to QEMU.
# Monitor:  socat - UNIX-CONNECT:$HOME/vm/images/vol2/mon.sock
set -euo pipefail

VM="$HOME/vm/images/vol2"
DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
EVIDENCE="$DF/evidence"
CASES="$DF/cases"
SSH_FWD="hostfwd=tcp:127.0.0.1:2224-:22"

RAM=4G               # as in the OVF
CPUS=2               # as in the OVF; Volatility 2 is single-threaded
DISK_BUS="${DISK_BUS:-virtio}"

mode="${1:-run}"
[[ $# -gt 0 ]] && shift

[[ -f "$VM/vol2.qcow2" ]] || { echo "missing $VM/vol2.qcow2" >&2; exit 1; }
for d in "$EVIDENCE" "$CASES"; do
  [[ -d "$d" ]] || { echo "missing $d (Cryptomator vault unlocked?)" >&2; exit 1; }
done

args=(
  -name vol2
  -machine q35,accel=kvm
  -cpu host
  -smp "$CPUS"
  -m "$RAM"
  -rtc base=utc

  -drive file="$VM/vol2.qcow2",if=none,id=sys,format=qcow2,cache=none,aio=native,discard=unmap
)

case "$DISK_BUS" in
  virtio) args+=(-device virtio-blk-pci,drive=sys,bootindex=1) ;;
  ahci)   args+=(-device ide-hd,drive=sys,bus=ide.0,bootindex=1) ;;   # q35 built-in ICH9 AHCI
  *)      echo "DISK_BUS must be virtio or ahci" >&2; exit 1 ;;
esac

args+=(
  -virtfs local,path="$EVIDENCE",mount_tag=evidence,security_model=none,readonly=on
  -virtfs local,path="$CASES",mount_tag=cases,security_model=none

  -device qemu-xhci
  -device usb-tablet
  -vga virtio
  -display gtk,gl=off

  -monitor unix:"$VM/mon.sock",server,nowait
)

case "$mode" in
  run) args+=(-nic "user,model=virtio-net-pci,restrict=on,$SSH_FWD") ;;
  net) args+=(-nic "user,model=virtio-net-pci,$SSH_FWD") ;;
  *)   echo "usage: $(basename "$0") run|net [extra qemu args...]" >&2; exit 1 ;;
esac

exec qemu-system-x86_64 "${args[@]}" "$@"
