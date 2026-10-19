#!/usr/bin/env bash
# fetch-labs.sh — download the course exercise files (cylab.be short links from the slides)
# into $DF/evidence/labs/<chapter>/ and log SHA-256 to $DF/cases/hashes-labs.txt.
# Skips files already present. Vault must be unlocked.
# After downloading, flags any file whose content does not match its extension
# (2026-10-19: treasurehunt_fw_eth1.pcap turned out to be a zip archive).
set -euo pipefail

DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
LABS="$DF/evidence/labs"
[[ -d "$DF" ]] || { echo "vault not mounted: $DF" >&2; exit 1; }
command -v file >/dev/null || { echo "missing: file (pacman -S file)" >&2; exit 1; }

# chapter  code   filename ('-' = use server-provided name)
LIST='
01_disks   fFMqA usb-01.img.zip
01_disks   5Lne8 usb-02.E01
01_disks   LOz9Z usb-03.img.zip
01_disks   kbcRa usb-04.E01
01_disks   iYJtY usb-05.zip
01_disks   Y1seb usb-06.E01
02_windows Q2zQ0 hives-01.zip
02_windows Gwsxl eventlogs-01.zip
02_windows sUWh8 prefetch.zip
02_windows XaVkj -
03_memory  y5UhN -
03_memory  j0BZp -
03_memory  7fRjY -
03_memory  GcsqM -
04_network uHlYB treasurehunt_fw_eth1.pcap
04_network 2FnZC lotsofweb.pcap
'
# Not downloadable: 04.1 lorem.txt (no link in the slides; expected `wc`: 39 1939 13222).

while read -r chap code name; do
  [[ -z "${chap:-}" ]] && continue
  dir="$LABS/$chap"; mkdir -p "$dir"
  url="https://cylab.be/s/$code"
  if [[ "$name" == "-" ]]; then
    [[ -e "$dir/.$code.done" ]] && { echo "skip $chap/$code"; continue; }
    echo "get  $chap/$code"
    (cd "$dir" && curl -fL -OJ "$url") && touch "$dir/.$code.done" || echo "FAIL $url" >&2
  else
    [[ -e "$dir/$name" ]] && { echo "skip $chap/$name"; continue; }
    echo "get  $chap/$name"
    curl -fL -o "$dir/$name" "$url" || { rm -f "$dir/$name"; echo "FAIL $url" >&2; }
  fi
done <<< "$LIST"

# Volatility 2 VM from the memory slides (VirtualBox OVA) -> ~/vm/iso
OVA="$HOME/vm/iso/mint-19.3-volatility.ova"
OVA_URL=https://cylab.be/s/jfp0o
ova_ok() { tar -tf "$1" >/dev/null 2>&1; }      # GNU tar seeks over members: fast, catches truncation

if [[ -e "$OVA" ]] && ova_ok "$OVA"; then
    echo "skip $(basename "$OVA")"
else
    [[ -e "$OVA" ]] && mv "$OVA" "$OVA.part"    # adopt a broken leftover as a resume point
    curl -fL -C - -o "$OVA.part" "$OVA_URL"; rc=$?
    if [[ $rc -eq 33 ]]; then                   # server refused byte ranges → restart clean
        rm -f "$OVA.part"; curl -fL -o "$OVA.part" "$OVA_URL"; rc=$?
    fi
    if [[ $rc -eq 0 ]] && ova_ok "$OVA.part"; then
        mv "$OVA.part" "$OVA"; echo "get  $(basename "$OVA")"
    else
        echo "FAIL $OVA_URL (curl rc=$rc, partial kept: $OVA.part)" >&2
    fi
fi

# Content vs extension. Prints the unzip command when a zip hides behind another name.
typecheck() {
  local f=$1 t
  t=$(file -b "$f")
  case "${f,,}" in
    *.zip)  [[ $t == Zip* ]] ;;
    *.pcap) [[ $t == *pcap* ]] ;;
    *.e01)  [[ $t == *EWF* ]] ;;
    *)      return 0 ;;
  esac
}
bad=0
while IFS= read -r -d '' f; do
  if ! typecheck "$f"; then
    bad=1
    rel=${f#"$LABS"/}
    echo "TYPE $rel: $(file -b "$f")" >&2
    [[ $(file -b "$f") == Zip* ]] && echo "     unzip -n '$f' -x '__MACOSX/*' -d '$(dirname "$f")/extracted'" >&2
  fi
done < <(find "$LABS" -type f ! -name '.*' ! -path '*/extracted/*' -print0)
[[ $bad -eq 0 ]] && echo "types: all files match their extension"

# Hashes of the downloads only (extracted/ has its own list: cases/hashes-labs-extracted.txt)
CUR="$DF/cases/hashes-labs.txt"
REF="$DF/cases/hashes-labs.2026-10-10.txt"      # frozen reference, chmod 444
(cd "$LABS" && find . -type f ! -name '.*' ! -path '*/extracted/*' -print0 | sort -z | xargs -0 -r sha256sum) > "$CUR"
echo "hashes -> $CUR"
if [[ -e "$REF" ]]; then
    (cd "$LABS" && sha256sum --quiet -c "$REF") && echo "verify: all reference hashes OK" \
        || echo "verify: MISMATCH against $REF" >&2
    new=$(comm -13 <(awk '{print $2}' "$REF" | sort) <(awk '{print $2}' "$CUR" | sort))
    [[ -n "$new" ]] && printf 'new (not in reference):\n%s\n' "$new"
fi
