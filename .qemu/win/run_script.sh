#!/bin/bash
#==============================================================================
# QEMU Windows VM Launcher — Tiny11 with TPM + USB eID Reader Passthrough
#==============================================================================
# Compatible: Bash 4.2+ | Linux hosts with KVM
# Purpose:    Launch a Windows VM with software TPM, VirtIO, and USB smart-card
#             reader passthrough. Designed for Belgian eID / itsme registration.
# Network:    NAT (user-mode), works with NetworkManager, systemd-networkd, etc.
#==============================================================================

set -euo pipefail

#------------------------------------------------------------------------------
# CONFIGURATION — edit these to match your system
#------------------------------------------------------------------------------
VM_DIR="${HOME}/vm/win"                           # VM working directory
OVMF_CODE="/usr/share/edk2/x64/OVMF_CODE.4m.fd"   # UEFI firmware (read-only)
OVMF_VARS="${VM_DIR}/OVMF_VARS.fd"                # UEFI variables (rw, per-VM)
DISK_IMG="${VM_DIR}/win.qcow2"                    # Main disk image
ISO_WIN="${VM_DIR}/tiny11_25H2_Nov25.iso"         # Windows installer ISO
ISO_VIRTIO="${VM_DIR}/virtio.iso"                 # VirtIO drivers ISO
TPM_DIR="${VM_DIR}/tpm"                           # swtpm state directory
TPM_SOCK="${TPM_DIR}/sock"                        # swtpm UNIX socket path

# USB smart-card reader IDs (Eletrand/Realtek composite reader)
# Change these if you use a different reader. Find yours with: lsusb
USB_VID="0x0bda"
USB_PID="0x0169"

# QEMU resources
QEMU_MEM="4G"
QEMU_SMP="4"

#------------------------------------------------------------------------------
# HELPERS
#------------------------------------------------------------------------------
info()  { printf "[INFO]  %s\n" "$*"; }
warn()  { printf "[WARN]  %s\n" "$*" >&2; }
error() { printf "[ERROR] %s\n" "$*" >&2; }

cleanup() {
    local exit_code=$?

    # Kill swtpm if we started it
    if [[ -n "${SWTPM_PID:-}" ]] && kill -0 "${SWTPM_PID}" 2>/dev/null; then
        info "Stopping swtpm (PID ${SWTPM_PID})..."
        kill "${SWTPM_PID}" 2>/dev/null || true
        wait "${SWTPM_PID}" 2>/dev/null || true
    fi

    # Restart host pcscd if it was running before
    if [[ "${PCSCD_WAS_RUNNING:-false}" == "true" ]]; then
        info "Restarting host pcscd..."
        sudo systemctl start pcscd 2>/dev/null || true
    fi

    # Remove stale socket to avoid "address already in use" on next run
    rm -f "${TPM_SOCK}"

    exit "${exit_code}"
}
trap cleanup EXIT INT TERM

#------------------------------------------------------------------------------
# PREREQUISITE CHECKS
#------------------------------------------------------------------------------
info "Checking prerequisites..."

# Required binaries
for bin in qemu-system-x86_64 swtpm; do
    if ! command -v "${bin}" &>/dev/null; then
        error "'${bin}' not found in PATH."
        error "  Arch:     sudo pacman -S qemu-full swtpm"
        error "  Debian:   sudo apt install qemu-system-x86 swtpm"
        error "  Fedora:   sudo dnf install qemu-kvm swtpm"
        exit 1
    fi
done

# Check user is in kvm group (best-effort warning)
if ! id -nG "${USER}" | grep -qw "kvm"; then
    warn "User '${USER}' is not in the 'kvm' group."
    warn "Fix: sudo usermod -aG kvm ${USER}  # then log out and back in"
fi

# Check OVMF firmware exists
if [[ ! -r "${OVMF_CODE}" ]]; then
    error "OVMF firmware not found: ${OVMF_CODE}"
    error "  Arch:     sudo pacman -S edk2-ovmf"
    error "  Debian:   sudo apt install ovmf"
    error "  Fedora:   sudo dnf install edk2-ovmf"
    exit 1
fi

# Check VM directory exists
if [[ ! -d "${VM_DIR}" ]]; then
    error "VM directory does not exist: ${VM_DIR}"
    error "Create it and place your VM files there (OVMF_VARS.fd, ISOs, etc.)"
    exit 1
fi

# Check required files (except disk — we handle that separately)
for f in "${OVMF_VARS}" "${ISO_WIN}" "${ISO_VIRTIO}"; do
    if [[ ! -f "${f}" ]]; then
        error "Missing required file: ${f}"
        exit 1
    fi
done

# Disk image: create only if missing, never overwrite
if [[ ! -f "${DISK_IMG}" ]]; then
    warn "Disk image not found: ${DISK_IMG}"
    read -r -p "Create a new ${QEMU_MEM%G}0 GB qcow2 disk? [y/N] " reply
    if [[ "${reply}" =~ ^[Yy]$ ]]; then
        info "Creating disk image..."
        qemu-img create -f qcow2 "${DISK_IMG}" 60G
    else
        error "Cannot start VM without a disk image."
        exit 1
    fi
fi

# Create TPM state directory
mkdir -p "${TPM_DIR}"

# Remove stale socket from a previous unclean exit
rm -f "${TPM_SOCK}"

#------------------------------------------------------------------------------
# STOP HOST PC/SC DAEMON
# Releases the USB smartcard reader so QEMU can claim it.
#------------------------------------------------------------------------------
if systemctl is-active --quiet pcscd 2>/dev/null; then
    PCSCD_WAS_RUNNING=true
    info "Stopping host pcscd (releases USB card reader for VM)..."
    sudo systemctl stop pcscd
else
    PCSCD_WAS_RUNNING=false
fi

#------------------------------------------------------------------------------
# START SWTPM EMULATOR
#------------------------------------------------------------------------------
info "Starting swtpm..."
swtpm socket \
    --tpmstate dir="${TPM_DIR}" \
    --ctrl type=unixio,path="${TPM_SOCK}" \
    --log level=20 \
    --flags not-need-init \
    &
SWTPM_PID=$!

# Wait for socket to appear (max 5 seconds)
for i in {1..50}; do
    if [[ -S "${TPM_SOCK}" ]]; then
        info "swtpm ready (PID ${SWTPM_PID})."
        break
    fi
    sleep 0.1
done
if [[ ! -S "${TPM_SOCK}" ]]; then
    error "swtpm failed to create socket within 5 seconds."
    exit 1
fi

#------------------------------------------------------------------------------
# LAUNCH QEMU
#------------------------------------------------------------------------------
info "Launching QEMU..."
info "  Disk:     ${DISK_IMG}"
info "  ISO:      ${ISO_WIN}"
info "  TPM:      ${TPM_SOCK}"
info "  USB dev:  ${USB_VID}:${USB_PID}"
info "  Network:  NAT (user-mode)"

qemu-system-x86_64 \
    -enable-kvm \
    -machine q35,smm=off \
    -cpu host \
    -smp "${QEMU_SMP}" \
    -m "${QEMU_MEM}" \
    -drive if=pflash,format=raw,readonly=on,file="${OVMF_CODE}" \
    -drive if=pflash,format=raw,file="${OVMF_VARS}" \
    -chardev socket,id=chrtpm,path="${TPM_SOCK}" \
    -tpmdev emulator,id=tpm0,chardev=chrtpm \
    -device tpm-tis,tpmdev=tpm0 \
    -drive file="${DISK_IMG}",if=virtio,format=qcow2,cache=none,aio=native,discard=unmap,detect-zeroes=unmap \
    -drive file="${ISO_WIN}",media=cdrom \
    -drive file="${ISO_VIRTIO}",media=cdrom \
    -nic user,model=virtio-net-pci \
    -device qemu-xhci,id=xhci \
    -device usb-host,vendorid="${USB_VID}",productid="${USB_PID}" \
    -device virtio-balloon \
    -vga std \
    -display gtk \
    -boot menu=on \
    "$@"

