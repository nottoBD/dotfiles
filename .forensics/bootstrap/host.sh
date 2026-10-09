#!/usr/bin/env bash
# host.sh — rebuild the DFIR lab on the host from this repo.
# Idempotent: never overwrites an existing disk image, VARS file or TPM state.
#   bash ~/projects/dotfiles/.forensics/bootstrap/host.sh
set -euo pipefail
shopt -u patsub_replacement 2>/dev/null || true   # bash 5.2: keep '&' literal in ${x//a/b}

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VM="$HOME/vm"
WIN="$VM/images/win11-dfir"
SIFT="$VM/images/sift"
DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
WIN_ISO="$VM/iso/Windows11_Client_x64_en-us_26300_9457.iso"
SIFT_OVA="$VM/iso/sift-2026-04-22.ova"

say() { printf '\n== %s\n' "$*"; }
warn() { printf '!! %s\n' "$*" >&2; }

say "packages"
sudo pacman -S --needed qemu-desktop edk2-ovmf swtpm libisoburn socat usbutils

say "layout"
if [[ ! -d "$VM" ]]; then
  sudo btrfs subvolume create "$VM"      # own subvolume: excluded from @home snapper snapshots
  sudo chown "$USER": "$VM"
fi
mkdir -p "$VM/iso" "$VM/images"
lsattr -d "$VM/images" | awk '{print $1}' | grep -q C || chattr +C "$VM/images"   # nodatacow for qcow2

say "launchers -> ~/.local/bin (symlinks into repo)"
mkdir -p "$HOME/.local/bin"
for f in "$REPO"/bin/*.sh; do
  chmod +x "$f"
  ln -sfn "$f" "$HOME/.local/bin/$(basename "$f")"
done

say "ISOs"
[[ -f "$VM/iso/virtio-win.iso" ]] || curl -fL -o "$VM/iso/virtio-win.iso" \
  https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso
[[ -f "$WIN_ISO" ]]  || warn "missing $WIN_ISO -> microsoft.com/software-download/windows11 (multi-edition x64, English)"
[[ -f "$SIFT_OVA" ]] || warn "missing $SIFT_OVA -> sans.org/tools/sift-workstation"
(cd "$VM/iso" && sha256sum -c --ignore-missing "$REPO/iso/SHA256SUMS") || warn "hash check failed or nothing to verify"

say "Windows guest"
mkdir -p "$WIN"
[[ -f "$WIN/OVMF_VARS.4m.fd" ]] || cp /usr/share/edk2/x64/OVMF_VARS.4m.fd "$WIN/"
[[ -f "$WIN/win11.qcow2" ]] || qemu-img create -f qcow2 -o preallocation=metadata "$WIN/win11.qcow2" 80G
if [[ ! -f "$WIN/unattend.iso" ]]; then
  read -rsp "Password for local admin 'analyst': " pw; echo
  pw=${pw//&/&amp;}; pw=${pw//</&lt;}; pw=${pw//>/&gt;}
  tmp=$(mktemp -d)
  xml=$(<"$REPO/guest/windows/autounattend.xml")
  printf '%s\n' "${xml//<Value>changeme<\/Value>/<Value>$pw</Value>}" > "$tmp/autounattend.xml"
  xorriso -as mkisofs -o "$WIN/unattend.iso" -V UNATTEND -J -r "$tmp/autounattend.xml"
  shred -u "$tmp/autounattend.xml"; rmdir "$tmp"
fi
xorriso -as mkisofs -o "$WIN/setup.iso" -V SETUP -J -r "$REPO/guest/windows/setup.ps1" 2>/dev/null

say "SIFT guest"
mkdir -p "$SIFT"
if [[ ! -f "$SIFT/sift.qcow2" && -f "$SIFT_OVA" ]]; then
  mkdir -p "$SIFT/ova"
  tar -xf "$SIFT_OVA" -C "$SIFT/ova"
  if grep -qi 'firmware="efi"' "$SIFT"/ova/*.ovf; then warn "SIFT OVA is EFI: sift-dfir.sh needs pflash"; fi
  qemu-img convert -p -f vmdk -O qcow2 "$SIFT"/ova/*.vmdk "$SIFT/sift.qcow2"
  rm -r "$SIFT/ova"
fi

say "case data"
if [[ -d "$DF" ]]; then
  mkdir -p "$DF/evidence" "$DF/cases"
else
  warn "vault not mounted: $DF (unlock Cryptomator, then: mkdir -p \$DF/{evidence,cases})"
fi

cat <<EOF

== next
Windows (fresh disk): win11-dfir.sh install
  OVMF: press a key at "boot from CD" | Load driver: viostor\\w11\\amd64
  desktop: run virtio-win-guest-tools.exe from the virtio CD, shut down
  win11-dfir.sh net ; host: echo "change ide2-cd0 $WIN/setup.iso" | socat - UNIX-CONNECT:$WIN/mon.sock
  guest admin terminal (Tamper Protection off first):
    \$d=(Get-Volume | ? FileSystemLabel -eq 'SETUP').DriveLetter; powershell -ExecutionPolicy Bypass -File "\${d}:\\setup.ps1"
SIFT: sift-dfir.sh net ; console: sudo apt install -y openssh-server && sudo systemctl enable --now ssh
  scp -P 2223 $REPO/guest/sift/setup.sh sansforensics@127.0.0.1: && ssh -t -p 2223 sansforensics@127.0.0.1 'bash setup.sh'
Baselines: see README.md
EOF
