#!/usr/bin/env bash
# dfir-push.sh — copy the DFIR lab to another machine with the same user and the same layout.
#
#   dfir-push.sh devid@192.168.42.2 [--no-iso] [--force]
#
# What goes over (each to the same path on the target):
#   ~/vm/images/sift/sift.qcow2, vol2/vol2.qcow2, win11-dfir/win11.qcow2
#        disks including their internal 'baseline' snapshot (what --clean reverts to)
#   ~/vm/images/win11-dfir/OVMF_VARS*.fd, tpm*/   Windows firmware variables + TPM state (+ baselines)
#   ~/vm/iso/<lab ISOs/OVAs>                       verified on the target against iso/SHA256SUMS
#   ~/.ssh/id_dfir_lab(.pub)                       + known_hosts entries of the three guests
# Not copied: the Cryptomator vault (it syncs by itself; only checked on the target),
# the scripts (they come from git), unattend.iso/setup.iso, logs.
#
# Transport: GNU tar --sparse over one SSH connection. Holes are skipped (vol2.qcow2 is
# 1 TiB apparent, ~7 GiB real) and files land byte-identical, so the baseline snapshot survives.
# (qemu-img convert would drop internal snapshots.)
#
# Before: on the target, the repo is at the same commit (git pull) and
#         'bash ~/projects/dotfiles/.forensics/bootstrap/host.sh --no-guests' has run.
# Guests must be stopped on both machines.
# A file already on the target with the same size and mtime is skipped. A different one is
# only replaced with --force (e.g. after re-taking a baseline here).
set -euo pipefail

die()  { printf 'xx %s\n' "$*" >&2; exit 1; }
say()  { printf '\n== %s\n' "$*"; }
ok()   { printf '   ok %s\n' "$*"; }
info() { printf '   -- %s\n' "$*"; }

DEST=; ISO=1; FORCE=0
for a in "$@"; do
  case $a in
    --no-iso) ISO=0 ;;
    --force)  FORCE=1 ;;
    -*)       die "unknown option $a" ;;
    *)        [[ -z $DEST ]] || die "one target only"; DEST=$a ;;
  esac
done
[[ -n $DEST ]] || die "usage: $(basename "$0") user@host [--no-iso] [--force]"

REPO=$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/.." && pwd)     # .forensics
VM=$HOME/vm
IMG=$VM/images
DF=$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2
ISOS=(Windows11_Client_x64_en-us_26300_9457.iso sift-2026-04-22.ova mint-19.3-volatility.ova virtio-win.iso)
DISKS=(sift/sift.qcow2 vol2/vol2.qcow2 win11-dfir/win11.qcow2)

# One SSH connection for the whole run: one password prompt at most.
CTL="${XDG_RUNTIME_DIR:-/tmp}/dfir-push.$$"
SSH=(ssh -o ControlMaster=auto -o ControlPath="$CTL" -o ControlPersist=60m)
trap 'ssh -o ControlPath="$CTL" -O exit "$DEST" >/dev/null 2>&1 || true' EXIT
rsh() { "${SSH[@]}" "$DEST" "$@"; }            # one command line, run by the target's login shell
rbash() { "${SSH[@]}" "$DEST" bash -s -- "$@"; } # bash script on stdin (works whatever the login shell)

# ---------------------------------------------------------------- preflight
say "preflight"
tar --version 2>/dev/null | head -1 | grep -q 'GNU tar' || die "GNU tar needed here"
[[ -z $(pgrep -f "qemu-system.*$IMG" || true) ]] || die "guests running here: dfir-lab.sh stop"
for d in "${DISKS[@]}"; do
  [[ -f $IMG/$d ]] || die "missing $IMG/$d"
  qemu-img snapshot -l -U "$IMG/$d" | awk '{print $2}' | grep -qx baseline || die "$d: no baseline snapshot"
done
[[ -f $IMG/win11-dfir/OVMF_VARS.baseline.fd && -d $IMG/win11-dfir/tpm.baseline ]] \
  || die "win: OVMF_VARS.baseline.fd / tpm.baseline missing"
[[ -f $HOME/.ssh/id_dfir_lab ]] || die "missing ~/.ssh/id_dfir_lab"
ok "here: guests stopped, three disks with a baseline snapshot"

rsh true || die "cannot ssh to $DEST"
read -r rhome rfree rmem rrun rtools < <(rbash "$IMG" <<'EOF'
img=$1; miss=
for c in tar qemu-img sha256sum git; do command -v "$c" >/dev/null || miss+="$c,"; done
printf '%s %s %s %s %s\n' "$HOME" "$(df -B1 --output=avail "$HOME" | tail -1 | tr -d ' ')" \
  "$(awk '/MemAvailable/ {print int($2/1048576)}' /proc/meminfo)" \
  "$(pgrep -fc "qemu-system.*$img" || true)" "${miss:-none}"
EOF
)
[[ $rhome == "$HOME" ]] || die "target home is $rhome, here $HOME: the layout must match"
[[ $rtools == none ]] || die "missing on target: ${rtools%,} (run bootstrap/host.sh --no-guests there)"
[[ ${rrun:-0} == 0 ]] || die "guests running on the target: dfir-lab.sh stop there"
ok "target $DEST: same home, tools present, guests stopped"

lhead=$(git -C "$REPO" rev-parse HEAD)
rhead=$(rbash "$REPO" <<'EOF'
git -C "$1" rev-parse HEAD 2>/dev/null || echo none
EOF
)
[[ $rhead != none ]] || die "no repo at $REPO on the target: clone it, then run bootstrap/host.sh --no-guests"
if [[ $rhead == "$lhead" ]]; then ok "repo at the same commit ${lhead:0:7}"
else info "repo: target at ${rhead:0:7}, here ${lhead:0:7} -> git pull on the target"; fi
[[ -z $(git -C "$REPO" status --porcelain -- . 2>/dev/null) ]] \
  || info "uncommitted changes in $REPO here: commit + push, then git pull on the target"
rbash <<'EOF' || info "launchers not linked on the target: run bootstrap/host.sh --no-guests there"
[[ -L $HOME/.local/bin/dfir-lab.sh ]]
EOF

# ---------------------------------------------------------------- what to send
WINFILES=()
while IFS= read -r -d '' f; do WINFILES+=("${f#"$IMG"/}"); done < <(
  find "$IMG/win11-dfir" -maxdepth 2 \( -name 'OVMF_VARS*.fd' -o -path '*/tpm*/tpm2-*' \) -type f -print0 | sort -z)
IMGFILES=("${DISKS[@]}" "${WINFILES[@]}")
ISOFILES=()
if (( ISO )); then
  for i in "${ISOS[@]}"; do
    if [[ -f $VM/iso/$i ]]; then ISOFILES+=("$i"); else info "not here, skipped: $VM/iso/$i"; fi
  done
fi
KEYFILES=(id_dfir_lab id_dfir_lab.pub)

# size+mtime of every candidate on the target
declare -A RSTAT=()
all=()
for f in "${IMGFILES[@]}"; do all+=("$IMG/$f"); done
for f in "${ISOFILES[@]}"; do all+=("$VM/iso/$f"); done
for f in "${KEYFILES[@]}"; do all+=("$HOME/.ssh/$f"); done
while read -r p s m; do RSTAT[$p]="$s $m"; done < <(rbash "${all[@]}" <<'EOF'
for p; do [[ -f $p ]] && printf '%s %s\n' "$p" "$(stat -c '%s %Y' "$p")"; done; true
EOF
)

# push BASE FILE...: send the files (paths relative to BASE) to the same BASE on the target
push() {
  local base=$1; shift
  local send=() p l r bytes
  for p in "$@"; do
    l=$(stat -c '%s %Y' "$base/$p")
    r=${RSTAT[$base/$p]:-}
    if [[ -z $r ]]; then
      send+=("$p")
    elif [[ $r == "$l" ]]; then
      info "already on target: $p"
    elif (( FORCE )); then
      send+=("$p"); info "replacing on target: $p"
    else
      die "$p differs on the target (size/mtime): rerun with --force to replace it"
    fi
  done
  (( ${#send[@]} )) || return 0
  bytes=$(cd "$base" && du -cB1 -- "${send[@]}" | tail -1 | cut -f1)      # allocated, not apparent
  (( bytes < rfree )) || die "not enough space on the target: need $(numfmt --to=iec "$bytes"), free $(numfmt --to=iec "$rfree")"
  rfree=$(( rfree - bytes ))
  info "sending $(numfmt --to=iec "$bytes"): ${send[*]}"
  if command -v pv >/dev/null; then
    tar -C "$base" --sparse -cf - -- "${send[@]}" | pv -s "$bytes" | rsh "mkdir -p '$base' && tar -C '$base' -xpf -"
  else
    tar -C "$base" --sparse -cf - -- "${send[@]}" | rsh "mkdir -p '$base' && tar -C '$base' -xpf -"
  fi
  ok "sent ${#send[@]} file(s) to $base"
}

# ---------------------------------------------------------------- transfer
say "VM disks + Windows firmware/TPM state"
push "$IMG" "${IMGFILES[@]}"

if (( ${#ISOFILES[@]} )); then
  say "ISOs / OVAs"
  push "$VM/iso" "${ISOFILES[@]}"
fi

say "SSH key for the guests"
push "$HOME/.ssh" "${KEYFILES[@]}"
rsh "chmod 700 ~/.ssh && chmod 600 ~/.ssh/id_dfir_lab"
kh=$(for port in 2222 2223 2224; do
       ssh-keygen -F "[127.0.0.1]:$port" -f "$HOME/.ssh/known_hosts" 2>/dev/null | grep -v '^#' || true
     done)
if [[ -n $kh ]]; then
  printf '%s\n' "$kh" | rsh 'cat > ~/.ssh/.dfir_known_hosts'
  rbash <<'EOF'
kh=$HOME/.ssh/known_hosts; new=$HOME/.ssh/.dfir_known_hosts; touch "$kh"
n=0; while IFS= read -r l; do grep -qxF "$l" "$kh" || { printf '%s\n' "$l" >> "$kh"; n=$((n+1)); }; done < "$new"
rm -f "$new"; echo "   ok $n guest host key(s) added to known_hosts"
EOF
fi

# ---------------------------------------------------------------- verify
say "verify on the target"
rbash "$IMG" "${DISKS[@]}" <<'EOF'
img=$1; shift; rc=0
for d; do
  if qemu-img check -q "$img/$d" && qemu-img snapshot -l "$img/$d" | awk '{print $2}' | grep -qx baseline; then
    echo "   ok $d: image consistent, baseline snapshot present"
  else
    echo "xx $d: qemu-img check or baseline failed"; rc=1
  fi
done
exit $rc
EOF
(cd "$IMG" && sha256sum -- "${WINFILES[@]}") | rsh "cd '$IMG' && sha256sum --quiet -c -" \
  && ok "Windows VARS + TPM state identical" || die "Windows VARS/TPM hashes differ"
if (( ${#ISOFILES[@]} )); then
  info "hashing ISOs on the target (a few minutes)"
  rsh "cd '$VM/iso' && sha256sum --quiet --ignore-missing -c -" < "$REPO/iso/SHA256SUMS" \
    && ok "ISOs match iso/SHA256SUMS" || die "ISO hash mismatch on the target"
fi

say "vault on the target (not copied: Cryptomator syncs it)"
rbash "$DF" <<'EOF'
df=$1
if [[ ! -d $df ]]; then
  echo "   !! $df not there: unlock the synthesis vault and let it sync"; exit 0
fi
ref=$df/cases/hashes-labs.2026-10-10.txt
if [[ -f $ref ]]; then
  miss=$(cd "$df/evidence/labs" 2>/dev/null && awk '{sub(/^[^ ]+  /,""); print}' "$ref" \
         | while IFS= read -r f; do [[ -f $f ]] || echo "$f"; done | wc -l)
  [[ $miss == 0 ]] && echo "   ok all lab downloads present (hash them there with fetch-labs.sh)" \
                   || echo "   !! $miss lab file(s) missing: sync not finished, or run fetch-labs.sh there"
else
  echo "   !! no frozen hash list in cases/: sync not finished?"
fi
[[ -d $df/cases/vol3-symbols ]] && echo "   ok Vol3 symbol cache present"
true
EOF
(( rmem >= 8 )) || info "target has ${rmem} GiB RAM available: dfir-lab.sh start needs 8 for win + sift"

say "done"
cat <<EOF
   On $DEST:
     dfir-lab.sh start --clean      # vault unlocked first
     plug a test USB key, then: findmnt | grep /run/media   (must print nothing)
   After re-taking a baseline here: dfir-push.sh $DEST --no-iso --force
EOF
