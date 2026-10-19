#!/usr/bin/env bash
# host.sh — rebuild the DFIR lab on the host from this repo.
# Idempotent: never overwrites an existing disk image, VARS file or TPM state.
#   bash ~/projects/dotfiles/.forensics/bootstrap/host.sh               full build from ISOs/OVAs
#   bash ~/projects/dotfiles/.forensics/bootstrap/host.sh --no-guests   packages, layout, launchers only;
#        the guests then come from another machine: dfir-push.sh <user>@<this host> (run over there)
set -euo pipefail
shopt -u patsub_replacement 2>/dev/null || true   # bash 5.2: keep '&' literal in ${x//a/b}

GUESTS=1
case ${1:-} in
  --no-guests) GUESTS=0 ;;
  "") ;;
  *) echo "usage: $(basename "$0") [--no-guests]" >&2; exit 1 ;;
esac

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VM="$HOME/vm"
WIN="$VM/images/win11-dfir"
SIFT="$VM/images/sift"
VOL2="$VM/images/vol2"
DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
WIN_ISO="$VM/iso/Windows11_Client_x64_en-us_26300_9457.iso"
SIFT_OVA="$VM/iso/sift-2026-04-22.ova"
VOL2_OVA="$VM/iso/mint-19.3-volatility.ova"

say() { printf '\n== %s\n' "$*"; }
warn() { printf '!! %s\n' "$*" >&2; }

say "packages"
sudo pacman -S --needed qemu-desktop edk2-ovmf swtpm libisoburn socat usbutils file unzip pv

say "layout"
if [[ ! -d "$VM" ]]; then
  if [[ $(findmnt -no FSTYPE -T "$HOME") == btrfs ]]; then
    sudo btrfs subvolume create "$VM"    # own subvolume: excluded from @home snapper snapshots
    sudo chown "$USER": "$VM"
  else
    mkdir -p "$VM"
  fi
fi
mkdir -p "$VM/iso" "$VM/images"
if [[ $(findmnt -no FSTYPE -T "$VM/images") == btrfs ]]; then
  lsattr -d "$VM/images" | awk '{print $1}' | grep -q C || chattr +C "$VM/images"   # nodatacow for qcow2
fi

say "launchers -> ~/.local/bin (symlinks into repo)"
mkdir -p "$HOME/.local/bin"
for f in "$REPO"/bin/*.sh; do
  chmod +x "$f"
  ln -sfn "$f" "$HOME/.local/bin/$(basename "$f")"
done
case ":$PATH:" in *":$HOME/.local/bin:"*) ;; *) warn "~/.local/bin is not in PATH" ;; esac

say "ISOs"
# --no-guests: the pinned ISOs come from the other machine (dfir-push.sh), don't fetch a newer virtio build
if (( GUESTS )) && [[ ! -f "$VM/iso/virtio-win.iso" ]]; then
  curl -fL -o "$VM/iso/virtio-win.iso" \
    https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso
fi
[[ -f "$WIN_ISO" ]]  || warn "missing $WIN_ISO -> microsoft.com/software-download/windows11 (multi-edition x64, English)"
[[ -f "$SIFT_OVA" ]] || warn "missing $SIFT_OVA -> sans.org/tools/sift-workstation"
[[ -f "$VOL2_OVA" ]] || warn "missing $VOL2_OVA -> fetch-labs.sh"
(cd "$VM/iso" && sha256sum -c --ignore-missing "$REPO/iso/SHA256SUMS") || warn "hash check failed or nothing to verify"

if (( GUESTS )); then
  say "Windows guest"
  mkdir -p "$WIN"
  [[ -f "$WIN/OVMF_VARS.4m.fd" ]] || cp /usr/share/edk2/x64/OVMF_VARS.4m.fd "$WIN/"
  fresh=0
  if [[ ! -f "$WIN/win11.qcow2" ]]; then
    qemu-img create -f qcow2 -o preallocation=metadata "$WIN/win11.qcow2" 80G
    fresh=1
  fi
  # The answer file (with the analyst password) is only needed to install a fresh disk.
  if (( fresh )) && [[ ! -f "$WIN/unattend.iso" ]]; then
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

  say "Vol2 guest"
  mkdir -p "$VOL2"
  if [[ ! -f "$VOL2/vol2.qcow2" && -f "$VOL2_OVA" ]]; then
    mkdir -p "$VOL2/ova"
    tar -xf "$VOL2_OVA" -C "$VOL2/ova"
    # 1 TiB virtual as created upstream; sparse, ~10 GiB allocated
    qemu-img convert -p -f vmdk -O qcow2 -o preallocation=metadata "$VOL2"/ova/*.vmdk "$VOL2/vol2.qcow2"
    rm -r "$VOL2/ova"
  fi
fi

say "case data"
if [[ -d "$DF" ]]; then
  mkdir -p "$DF/evidence" "$DF/cases"
else
  warn "vault not mounted: $DF (unlock Cryptomator, then: mkdir -p \$DF/{evidence,cases})"
fi

if (( ! GUESTS )); then
  cat <<EOF

== next
On the machine that has the guests (guests stopped):  dfir-push.sh $USER@<this host>
Then here: unlock the vault, dfir-lab.sh start --clean
EOF
  exit 0
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
Vol2: vol2-dfir.sh net ; console (vagrant): sudo apt install -y openssh-server
  scp -P 2224 $REPO/guest/vol2/setup.sh ~/.ssh/id_dfir_lab.pub vagrant@127.0.0.1: && ssh -t -p 2224 vagrant@127.0.0.1 'bash setup.sh'
Baselines: see docs/LAB.md
EOF
