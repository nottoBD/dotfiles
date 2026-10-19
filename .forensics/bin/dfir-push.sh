#!/usr/bin/env bash
# dfir-push.sh — copy the DFIR lab to another machine with the same user and the same layout.
#
#   dfir-push.sh devid@192.168.42.2 [--no-iso] [--force]   everything below
#   dfir-push.sh devid@192.168.42.2 --vault-only           only the large vault files (after a lesson)
#
# What goes over (each to the same path on the target):
#   ~/vm/images/sift/sift.qcow2, vol2/vol2.qcow2, win11-dfir/win11.qcow2
#        disks including their internal 'baseline' snapshot (what --clean reverts to)
#   ~/vm/images/win11-dfir/OVMF_VARS*.fd, tpm*/   Windows firmware variables + TPM state (+ baselines)
#   ~/vm/iso/<lab ISOs/OVAs>                       verified on the target against iso/SHA256SUMS
#   ~/.ssh/id_dfir_lab(.pub)                       + known_hosts entries of the three guests
#   vault files git cannot carry                   see below
# Not copied: the scripts (they come from git), unattend.iso/setup.iso, logs.
#
# Vault: 'synthesis' syncs through git, and gitfit keeps every file whose Cryptomator
# ciphertext is >= 100 MiB (GitHub's limit) out of it. Those files — extracted memory images,
# big lab zips, E01s, memory dumps — exist only where they were made. This script copies
# them into the target's unlocked vault when they are missing there. Smaller files are left
# to git (copying them would collide with the next 'git pull' of the vault); the script only
# lists the ones still missing on the target.
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

DEST=; ISO=1; FORCE=0; VONLY=0
for a in "$@"; do
  case $a in
    --no-iso)     ISO=0 ;;
    --force)      FORCE=1 ;;
    --vault-only) VONLY=1 ;;
    -*)           die "unknown option $a" ;;
    *)            [[ -z $DEST ]] || die "one target only"; DEST=$a ;;
  esac
done
[[ -n $DEST ]] || die "usage: $(basename "$0") user@host [--no-iso] [--force] [--vault-only]"

REPO=$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/.." && pwd)     # .forensics
VM=$HOME/vm
IMG=$VM/images
DF=$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2
ISOS=(Windows11_Client_x64_en-us_26300_9457.iso sift-2026-04-22.ova mint-19.3-volatility.ova virtio-win.iso)
DISKS=(sift/sift.qcow2 vol2/vol2.qcow2 win11-dfir/win11.qcow2)
GITMAX=104857600     # GitHub per-file limit, the threshold gitfit uses on the ciphertext

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
if (( ! VONLY )); then
  for d in "${DISKS[@]}"; do
    [[ -f $IMG/$d ]] || die "missing $IMG/$d"
    qemu-img snapshot -l -U "$IMG/$d" | awk '{print $2}' | grep -qx baseline || die "$d: no baseline snapshot"
  done
  [[ -f $IMG/win11-dfir/OVMF_VARS.baseline.fd && -d $IMG/win11-dfir/tpm.baseline ]] \
    || die "win: OVMF_VARS.baseline.fd / tpm.baseline missing"
  [[ -f $HOME/.ssh/id_dfir_lab ]] || die "missing ~/.ssh/id_dfir_lab"
  ok "here: guests stopped, three disks with a baseline snapshot"
fi

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

# ---------------------------------------------------------------- helpers
declare -A RSTAT=()
# rstat_fill ABSPATH...: size+mtime of those files on the target (absent ones stay unset)
rstat_fill() {
  (( $# )) || return 0
  local p s m
  while read -r p s m; do RSTAT[$p]="$s $m"; done < <(rbash "$@" <<'EOF'
for p; do [[ -f $p ]] && printf '%s %s\n' "$p" "$(stat -c '%s %Y' "$p")"; done; true
EOF
)
}

# push BASE FILE...: send the files (paths relative to BASE) to the same BASE on the target.
# SPARSE=1 (disks, ISOs): tar --sparse skips holes, size counted from allocated blocks.
# SPARSE=0 (vault): the Cryptomator FUSE mount reports 0 allocated blocks for every file, and
#   tar --sparse archives such a file as one big hole (all zeros). So: plain tar, apparent size.
# SENT: the files actually sent, for verification afterwards.
SPARSE=1; SENT=()
push() {
  local base=$1; shift
  local send=() p l r bytes topt=()
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
  SENT=("${send[@]}")
  (( ${#send[@]} )) || return 0
  if (( SPARSE )); then
    topt=(--sparse)
    bytes=$(cd "$base" && du -cB1 -- "${send[@]}" | tail -1 | cut -f1)    # allocated, not apparent
  else
    bytes=$(cd "$base" && stat -c %s -- "${send[@]}" | awk '{s+=$1} END {print s+0}')
  fi
  (( bytes < rfree )) || die "not enough space on the target: need $(numfmt --to=iec "$bytes"), free $(numfmt --to=iec "$rfree")"
  rfree=$(( rfree - bytes ))
  info "sending $(numfmt --to=iec "$bytes"): ${send[*]}"
  if command -v pv >/dev/null; then
    tar -C "$base" "${topt[@]}" -cf - -- "${send[@]}" | pv -s "$bytes" | rsh "mkdir -p '$base' && tar -C '$base' -xpf -"
  else
    tar -C "$base" "${topt[@]}" -cf - -- "${send[@]}" | rsh "mkdir -p '$base' && tar -C '$base' -xpf -"
  fi
  ok "sent ${#send[@]} file(s) to $base"
}

# ---------------------------------------------------------------- guests, ISOs, key
if (( ! VONLY )); then
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

  all=()
  for f in "${IMGFILES[@]}"; do all+=("$IMG/$f"); done
  for f in "${ISOFILES[@]}"; do all+=("$VM/iso/$f"); done
  for f in "${KEYFILES[@]}"; do all+=("$HOME/.ssh/$f"); done
  rstat_fill "${all[@]}"

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
fi

# ---------------------------------------------------------------- vault
say "vault: files too large for its git sync"
if [[ ! -d $DF ]]; then
  info "vault locked here ($DF): skipped"
elif ! rbash "$DF" <<'EOF'
[[ -d $1/evidence && -d $1/cases ]]
EOF
then
  info "vault locked on the target: unlock 'synthesis' there, then: dfir-push.sh $DEST --vault-only"
else
  # Cryptomator ciphertext: 88-byte header + 48 bytes per 32 KiB chunk (upper bound of both cipher modes)
  big=(); small=()
  while IFS= read -r -d '' f; do
    s=$(stat -c %s "$f"); rel=${f#"$DF"/}
    if (( 88 + s + 48 * ((s + 32767) / 32768) >= GITMAX )); then big+=("$rel"); else small+=("$rel"); fi
  done < <(find "$DF" -path "$DF/scripts" -prune -o -type f -print0)

  # Evidence: compared by content, not size/mtime (a zero-filled copy has the same size and mtime).
  if (( ${#big[@]} )); then
    info "hashing ${#big[@]} large vault file(s) on both machines"
    declare -A LH=() RH=()
    while read -r h f; do LH[$f]=$h; done < <(cd "$DF" && sha256sum -- "${big[@]}")
    while read -r h f; do RH[$f]=$h; done < <(rbash "$DF" "${big[@]}" <<'EOF'
cd "$1"; shift; for f; do [[ -f $f ]] && sha256sum -- "$f"; done; true
EOF
)
    need=()
    for f in "${big[@]}"; do
      if [[ ${RH[$f]:-} == "${LH[$f]}" ]]; then info "identical on target: $f"
      elif [[ -n ${RH[$f]:-} ]]; then info "differs on target, resending: $f"; need+=("$f")
      else need+=("$f"); fi
    done
    if (( ${#need[@]} )); then
      SPARSE=0; push "$DF" "${need[@]}"; SPARSE=1
      for f in "${need[@]}"; do printf '%s  %s\n' "${LH[$f]}" "$f"; done \
        | rsh "cd '$DF' && sha256sum --quiet -c -" \
        && ok "sent vault files identical (sha256)" || die "vault file hash mismatch on the target"
    fi
  else
    ok "no file over the git limit"
  fi

  # small files are git's job: only report those the target doesn't have yet
  mapfile -t rfiles < <(rbash "$DF" <<'EOF'
cd "$1" && find . -path ./scripts -prune -o -type f -printf '%P\n'
EOF
)
  declare -A RHAS=(); for f in "${rfiles[@]}"; do RHAS[$f]=1; done
  missing=(); for f in "${small[@]}"; do [[ -n ${RHAS[$f]:-} ]] || missing+=("$f"); done
  if (( ${#missing[@]} )); then
    info "${#missing[@]} smaller file(s) not on the target yet (git's job): run gitfit + commit + push in the vault repo here, git pull there"
    printf '        %s\n' "${missing[@]:0:10}"
    (( ${#missing[@]} > 10 )) && info "… and $(( ${#missing[@]} - 10 )) more"
  else
    ok "every smaller vault file is on the target"
  fi
fi

if (( ! VONLY )); then
  (( rmem >= 8 )) || info "target has ${rmem} GiB RAM available: dfir-lab.sh start needs 8 for win + sift"
  say "done"
  cat <<EOF
   On $DEST:
     dfir-lab.sh start --clean      # vault unlocked first
     plug a test USB key, then: findmnt | grep /run/media   (must print nothing)
   After re-taking a baseline here: dfir-push.sh $DEST --no-iso --force
   After a lesson that produced big evidence (E01, dump): dfir-push.sh $DEST --vault-only
EOF
fi
