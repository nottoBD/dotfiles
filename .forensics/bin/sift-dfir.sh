#!/usr/bin/env bash
# sift-dfir.sh — SANS SIFT Workstation guest under QEMU/KVM
#
#   run : isolated (no internet), SSH only:  ssh -p 2223 sansforensics@127.0.0.1
#   net : user-mode NAT with internet (apt, tool fixes) + same SSH forward
#
# Host case data (Cryptomator mount must be unlocked) shared over 9p:
#   evidence -> /mnt/evidence  (read-only)
#   cases    -> /mnt/cases     (read-write)
#
# Anything after the mode is appended to QEMU, e.g. a raw evidence disk:
#   sift-dfir.sh run -drive file=/mnt/ewf/ewf1,if=virtio,format=raw,readonly=on
#
# Monitor:  socat - UNIX-CONNECT:$HOME/vm/images/sift/mon.sock
# Login:    sansforensics / forensics
set -euo pipefail

VM="$HOME/vm/images/sift"
DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
EVIDENCE="$DF/evidence"
CASES="$DF/cases"
SSH_FWD="hostfwd=tcp:127.0.0.1:2223-:22"

RAM=4G      # Windows guest uses 6G; avoid running both under heavy load on a 14 GiB host
CPUS=4

mode="${1:-run}"
[[ $# -gt 0 ]] && shift

for d in "$EVIDENCE" "$CASES"; do
  [[ -d "$d" ]] || { echo "missing $d (Cryptomator vault unlocked?)" >&2; exit 1; }
done

args=(
  -name sift
  -machine q35,accel=kvm
  -cpu host
  -smp "$CPUS"
  -m "$RAM"
  -rtc base=utc

  -drive file="$VM/sift.qcow2",if=none,id=sys,format=qcow2,cache=none,aio=native,discard=unmap
  -device virtio-blk-pci,drive=sys,bootindex=1

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
