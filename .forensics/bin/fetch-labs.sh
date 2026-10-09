#!/usr/bin/env bash
# fetch-labs.sh — download the course exercise files (cylab.be short links from the slides)
# into $DF/evidence/labs/<chapter>/ and log SHA-256 to $DF/cases/hashes-labs.txt.
# Skips files already present. Vault must be unlocked.
set -euo pipefail

DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
LABS="$DF/evidence/labs"
[[ -d "$DF" ]] || { echo "vault not mounted: $DF" >&2; exit 1; }

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
[[ -e "$OVA" ]] || curl -fL -o "$OVA" https://cylab.be/s/jfp0o || rm -f "$OVA"

(cd "$LABS" && find . -type f ! -name '.*' -print0 | sort -z | xargs -0 -r sha256sum) > "$DF/cases/hashes-labs.txt"
echo "hashes -> $DF/cases/hashes-labs.txt"
