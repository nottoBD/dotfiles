#!/usr/bin/env bash
# win11-dfir.sh — Windows 11 DFIR guest under QEMU/KVM
#
#   install : Windows ISO + virtio-win + unattend ISO attached, no NIC
#   run     : isolated NIC (restrict=on: no internet), SSH only (default)
#   net     : user-mode NAT with internet (updates, tool installs)
#
# SSH (run|net):  ssh -p 2222 analyst@127.0.0.1
# Anything after the mode is appended to QEMU, e.g.:
#   win11-dfir.sh run -drive file=/mnt/ewf/ewf1,if=virtio,format=raw,readonly=on
#   win11-dfir.sh run -device usb-host,hostbus=3,hostaddr=7      # FTK Imager lab
# Monitor:  socat - UNIX-CONNECT:$HOME/vm/images/win11-dfir/mon.sock
#   ISO hot-swap:  change ide2-cd0 /abs/path.iso
#   memory dump:   dump-guest-memory /abs/path/mem.elf   (no -z: that is kdump, not ELF)
set -euo pipefail

VM="$HOME/vm/images/win11-dfir"
ISO="$HOME/vm/iso"
WIN_ISO="$ISO/Windows11_Client_x64_en-us_26300_9457.iso"
VIRTIO_ISO="$ISO/virtio-win.iso"
UNATTEND_ISO="$VM/unattend.iso"
SSH_FWD="hostfwd=tcp:127.0.0.1:2222-:22"

RAM=6G          # 14 GiB host; don't go above 8G
CPUS=6

mode="${1:-run}"
[[ $# -gt 0 ]] && shift

# TPM 2.0 emulator (Windows 11 requirement). --terminate: exits with QEMU.
mkdir -p "$VM/tpm"
rm -f "$VM/tpm/sock"
swtpm socket --tpm2 \
  --tpmstate dir="$VM/tpm" \
  --ctrl type=unixio,path="$VM/tpm/sock" \
  --log file="$VM/tpm/swtpm.log" \
  --terminate --daemon
for _ in $(seq 1 50); do [[ -S "$VM/tpm/sock" ]] && break; sleep 0.1; done
[[ -S "$VM/tpm/sock" ]] || { echo "swtpm socket did not appear" >&2; exit 1; }

args=(
  -name win11-dfir
  # smm + secure pflash: required by the Secure Boot OVMF build (26H2 setup checks Secure Boot capability)
  -machine q35,accel=kvm,smm=on
  -global driver=cfi.pflash01,property=secure,value=on
  # -svm: hide AMD-V from the guest -> no VBS/HVCI, cleaner memory images
  -cpu host,-svm,hv_relaxed,hv_vapic,hv_spinlocks=0x1fff,hv_time,hv_vpindex,hv_synic,hv_stimer,hv_frequencies
  -smp "$CPUS",sockets=1,cores=$((CPUS / 2)),threads=2
  -m "$RAM"
  -rtc base=utc

  -drive if=pflash,format=raw,readonly=on,file=/usr/share/edk2/x64/OVMF_CODE.secboot.4m.fd
  -drive if=pflash,format=raw,file="$VM/OVMF_VARS.4m.fd"

  -chardev socket,id=chrtpm,path="$VM/tpm/sock"
  -tpmdev emulator,id=tpm0,chardev=chrtpm
  -device tpm-tis,tpmdev=tpm0

  -drive file="$VM/win11.qcow2",if=none,id=sys,format=qcow2,cache=none,aio=native,discard=unmap
  -device virtio-blk-pci,drive=sys,bootindex=1

  -device qemu-xhci
  -device usb-tablet
  -vga virtio
  -display gtk,gl=off

  -monitor unix:"$VM/mon.sock",server,nowait
)

case "$mode" in
  install)
    args+=(
      -drive file="$WIN_ISO",media=cdrom,if=none,id=cdwin,readonly=on
      -device ide-cd,drive=cdwin,bus=ide.0,bootindex=0
      -drive file="$VIRTIO_ISO",media=cdrom,if=none,id=cdvirtio,readonly=on
      -device ide-cd,drive=cdvirtio,bus=ide.1
      -drive file="$UNATTEND_ISO",media=cdrom,if=none,id=cdunattend,readonly=on
      -device ide-cd,drive=cdunattend,bus=ide.2
      -nic none
    )
    ;;
  run) args+=(-nic "user,model=virtio-net-pci,restrict=on,$SSH_FWD") ;;
  net) args+=(-nic "user,model=virtio-net-pci,$SSH_FWD") ;;
  *)
    echo "usage: $(basename "$0") install|run|net [extra qemu args...]" >&2
    exit 1
    ;;
esac

exec qemu-system-x86_64 "${args[@]}" "$@"
