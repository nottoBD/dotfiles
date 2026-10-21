#!/usr/bin/env bash
# df-reset.sh — reset the SIFT practice state between exercise runs.
#   1. unmounts every image mounted with -o loop (deepest first)
#   2. unmounts ewfmount/affuse FUSE mounts on the usual mount points
#   3. frees loop devices whose image lives in your home folder
#   4. deletes your work folders ~/df01, ~/df02, …
#   5. optional --history: empties the bash history (no Ctrl-R cheating)
# Never touches /mnt/evidence, /mnt/cases or the /snap mounts.
#
# usage: bash /mnt/cases/df-reset.sh [-y] [--history]
#   -y          don't ask for confirmation
#   --history   also empty ~/.bash_history

set -u
YES=0 HIST=0
for a in "$@"; do
  case $a in
    -y) YES=1 ;;
    --history) HIST=1 ;;
    -h|--help) sed -n '2,13p' "$0"; exit 0 ;;
    *) echo "unknown option: $a (see --help)"; exit 1 ;;
  esac
done

cd / || exit 1                      # never sit inside a mount point we unmount
shopt -s nullglob

PROTECT='^/(mnt/evidence|mnt/cases|snap)(/|$)'
FUSE_POINTS=(/mnt/usb /mnt/e01 /mnt/windows /mnt/ewf /mnt/ewf_mount /mnt/aff)

# 1. loop mounts (images mounted with -o loop), deepest path first
mapfile -t LOOP_MOUNTS < <(findmnt -rn -o TARGET,SOURCE \
  | awk '$2 ~ /^\/dev\/loop/ {print $1}' | grep -Ev "$PROTECT" | sort -r)

# 2. FUSE mounts made by ewfmount / affuse
FUSE_MOUNTS=()
for m in "${FUSE_POINTS[@]}"; do
  t=$(findmnt -rn -o FSTYPE --mountpoint "$m" 2>/dev/null)
  [[ $t == fuse* ]] && FUSE_MOUNTS+=("$m")
done

# 3. loop devices backed by a file in your home (e.g. built with losetup)
mapfile -t LOOP_DEVS < <(losetup -nO NAME,BACK-FILE 2>/dev/null \
  | awk -v h="$HOME/" 'index($2, h) == 1 {print $1}')

# 4. work folders
WORK_DIRS=("$HOME"/df[0-9][0-9])

echo "== df-reset plan"
printf '   unmount (loop) : %s\n' "${LOOP_MOUNTS[@]:-none}"
printf '   unmount (FUSE) : %s\n' "${FUSE_MOUNTS[@]:-none}"
printf '   free loop dev  : %s\n' "${LOOP_DEVS[@]:-none}"
if ((${#WORK_DIRS[@]})); then
  for d in "${WORK_DIRS[@]}"; do printf '   delete folder  : %s (%s)\n' "$d" "$(du -sh "$d" 2>/dev/null | cut -f1)"; done
else
  echo "   delete folder  : none"
fi
((HIST)) && echo "   empty history  : $HOME/.bash_history"

if ((${#LOOP_MOUNTS[@]} + ${#FUSE_MOUNTS[@]} + ${#LOOP_DEVS[@]} + ${#WORK_DIRS[@]} + HIST == 0)); then
  echo "== nothing to reset"; exit 0
fi
if ((!YES)); then
  read -rp "Proceed? [y/N] " r
  [[ $r == [yY]* ]] || { echo "aborted"; exit 1; }
fi

rc=0
for m in "${LOOP_MOUNTS[@]}"; do sudo umount "$m" && echo "   ok unmounted $m" || { echo "   !! $m busy: close programs using it, then rerun"; rc=1; }; done
for m in "${FUSE_MOUNTS[@]}"; do sudo umount "$m" && echo "   ok unmounted $m" || { echo "   !! $m busy: unmount what is mounted from it first"; rc=1; }; done
for l in "${LOOP_DEVS[@]}"; do sudo losetup -d "$l" && echo "   ok freed $l" || rc=1; done
for d in "${WORK_DIRS[@]}"; do
  if findmnt -rn -o TARGET | grep -q "^$d"; then
    echo "   !! $d still has something mounted inside: kept"; rc=1
  else
    rm -rf -- "$d" && echo "   ok deleted $d"
  fi
done
if ((HIST)); then
  : > "$HOME/.bash_history" && echo "   ok emptied ~/.bash_history"
  echo "   -> also run:  history -c   (clears this terminal's in-memory history)"
fi

echo "== done$( ((rc)) && echo ' (with warnings)')"
exit $rc
