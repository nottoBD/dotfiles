#!/usr/bin/env bash
# dfir-lab.sh — lesson runner for the forensics lab
#
#   dfir-lab.sh start [--net] [--clean] [win] [sift]   boot guests (default: both, isolated), checks, lesson notes
#   dfir-lab.sh stop  [win] [sift]                     ACPI shutdown, wait for exit
#   dfir-lab.sh status
#   dfir-lab.sh ssh   win|sift [cmd...]
#   dfir-lab.sh usb                                    list host USB devices
#   dfir-lab.sh usb   attach <bus> <addr> | detach     hot-plug a USB device into Windows (FTK Imager lab)
#   dfir-lab.sh dump  win|sift                         memory dump -> today's case dir, sha256 logged
#   dfir-lab.sh keys                                   one-time: key-based SSH into both guests
#   dfir-lab.sh baseline [win] [sift]                  re-take baseline snapshots (guests stopped)
#
# --net   : guests get internet (updates, symbol downloads); default is isolated (restrict=on), SSH only
# --clean : revert guests to their baseline snapshot before boot
set -euo pipefail
shopt -u patsub_replacement 2>/dev/null || true

DF="$HOME/.local/share/Cryptomator/mnt/synthesis/digital_forensics_rma_ma2"
IMG="$HOME/vm/images"
KEY="$HOME/.ssh/id_dfir_lab"
KNOWN="$HOME/.ssh/known_hosts_dfir"
TODAY=$(date +%F)
LESSON="$DF/cases/$TODAY"

declare -A NAME=([win]=win11-dfir [sift]=sift)
declare -A LAUNCH=([win]=win11-dfir.sh [sift]=sift-dfir.sh)
declare -A PORT=([win]=2222 [sift]=2223)
declare -A USERN=([win]=analyst [sift]=sansforensics)
declare -A RAM_G=([win]=4 [sift]=4)   # must match RAM= in the launchers
declare -A DISK=([win]=win11.qcow2 [sift]=sift.qcow2)

c()    { printf '\e[1;36m== %s\e[0m\n' "$*"; }
ok()   { printf '   \e[32mok\e[0m %s\n' "$*"; }
warn() { printf '   \e[33m!!\e[0m %s\n' "$*" >&2; }
die()  { printf '\e[31mxx %s\e[0m\n' "$*" >&2; exit 1; }

valid()   { [[ -n "$1" ]] && [[ -n "${NAME[$1]:-}" ]] || die "unknown guest '$1' (win|sift)"; }
pat()     { printf -- '^qemu-system-x86_64 .*-name %s( |$)' "${NAME[$1]}"; }
running() { pgrep -f -- "$(pat "$1")" >/dev/null; }
mon()     { printf '%s\n' "$2" | socat - "UNIX-CONNECT:$IMG/${NAME[$1]}/mon.sock"; }
banner()  { timeout 2 bash -c "exec 3<>/dev/tcp/127.0.0.1/$1 && head -c 4 <&3" 2>/dev/null | grep -q '^SSH-'; }
ssh_opts() { printf '%s\n' -i "$KEY" -p "${PORT[$1]}" -o StrictHostKeyChecking=accept-new \
               -o UserKnownHostsFile="$KNOWN" -o LogLevel=ERROR; }
sshb()    { local g=$1; shift; mapfile -t o < <(ssh_opts "$g")
            ssh -n "${o[@]}" -o BatchMode=yes -o ConnectTimeout=5 "${USERN[$g]}@127.0.0.1" "$@"; }
sshi()    { local g=$1; shift; mapfile -t o < <(ssh_opts "$g")
            ssh "${o[@]}" "${USERN[$g]}@127.0.0.1" "$@"; }
note()    { [[ -f "$LESSON/notes.md" ]] && printf -- '- %s: %s\n' "$(date +%T)" "$*" >> "$LESSON/notes.md" || true; }

set_guests()  { GS=(); for a in "$@"; do valid "$a"; GS+=("$a"); done
                ((${#GS[@]})) || GS=(win sift); }
psenc()       { printf '%s' "$1" | iconv -f UTF-8 -t UTF-16LE | base64 -w0; }

wait_vault() {
  if [[ ! -d "$DF" ]]; then
    warn "vault locked: unlock Cryptomator (waiting up to 2 min)"
    for _ in $(seq 60); do [[ -d "$DF" ]] && break; sleep 2; done
    [[ -d "$DF" ]] || die "vault not mounted: $DF"
  fi
  mkdir -p "$DF/evidence" "$DF/cases"
  ok "vault mounted"
}

preflight() {
  [[ -r /dev/kvm && -w /dev/kvm ]] || die "/dev/kvm not usable"
  for b in qemu-system-x86_64 swtpm socat "${LAUNCH[win]}" "${LAUNCH[sift]}"; do
    command -v "$b" >/dev/null || die "missing: $b"
  done
  local free need=0 avail
  free=$(df --output=avail -BG "$IMG" | tail -1 | tr -dc 0-9)
  (( free >= 20 )) && ok "disk: ${free} GiB free" || warn "disk: only ${free} GiB free on $IMG"
  for g in "$@"; do running "$g" || need=$((need + RAM_G[$g])); done
  avail=$(awk '/MemAvailable/ {print int($2/1048576)}' /proc/meminfo)
  (( avail >= need )) && ok "RAM: ${avail} GiB available, ${need} GiB needed" \
                      || warn "RAM: ${avail} GiB available, ${need} GiB needed — expect swapping"
}

revert() {
  local g=$1 d="$IMG/${NAME[$1]}"
  running "$g" && { warn "$g running: not reverted"; return; }
  qemu-img snapshot -l "$d/${DISK[$g]}" | awk '{print $2}' | grep -qx baseline || die "$g: no baseline snapshot"
  qemu-img snapshot -a baseline "$d/${DISK[$g]}"
  if [[ $g == win ]]; then
    [[ -f "$d/OVMF_VARS.baseline.fd" && -d "$d/tpm.baseline" ]] || die "win: OVMF_VARS.baseline.fd / tpm.baseline missing"
    cp "$d/OVMF_VARS.baseline.fd" "$d/OVMF_VARS.4m.fd"
    rm -rf "$d/tpm"; cp -a "$d/tpm.baseline" "$d/tpm"
  fi
  ok "$g reverted to baseline"
}

lesson_notes() {
  mkdir -p "$LESSON"
  if [[ ! -f "$LESSON/notes.md" ]]; then
    {
      echo "# Forensics lesson $TODAY"
      echo
      echo "- host: $(uname -n) $(uname -r)"
      for g in win sift; do
        echo "- $g baseline: $(qemu-img snapshot -l -U "$IMG/${NAME[$g]}/${DISK[$g]}" 2>/dev/null \
                               | awk '$2=="baseline" {print $(NF-3), $(NF-2)}')"
      done
      echo
      echo "## Evidence received"
      echo
      echo "| file | sha256 | source |"
      echo "|---|---|---|"
      echo
      echo "## Log"
    } > "$LESSON/notes.md"
  fi
  ok "lesson dir: $LESSON"
}

post_checks() {
  local g=$1
  if ! sshb "$g" exit 2>/dev/null; then
    warn "$g: key auth not set up — run: dfir-lab.sh keys"
    return
  fi
  case $g in
    sift)
      sshb sift 'mountpoint -q /mnt/evidence && mountpoint -q /mnt/cases' \
        && ok "sift: /mnt/evidence (ro) + /mnt/cases mounted" \
        || warn "sift: 9p shares not mounted — ssh in and: sudo mount -a"
      ;;
    win)
      local rtp
      rtp=$(sshb win '(Get-MpComputerStatus).RealTimeProtectionEnabled' 2>/dev/null | tr -d '\r') || true
      [[ $rtp == False ]] && ok "win: Defender real-time protection off" \
                          || warn "win: Defender real-time protection is '$rtp' — it will alter evidence"
      ;;
  esac
}

summary() {
  cat <<EOF

$(c ready)
   cases today : $LESSON   (SIFT: /mnt/cases/$TODAY)
   evidence    : $DF/evidence   (SIFT: /mnt/evidence, read-only)
   ssh         : dfir-lab.sh ssh win | dfir-lab.sh ssh sift
   to Windows  : scp -P 2222 -i $KEY <file> analyst@127.0.0.1:C:/Cases/
   USB (FTK)   : dfir-lab.sh usb  →  dfir-lab.sh usb attach <bus> <addr>
   memory dump : dfir-lab.sh dump win|sift
   E01 in SIFT : sudo ewfmount /mnt/evidence/<x>.E01 /mnt/e01 && sudo mmls /mnt/e01/ewf1
   end         : dfir-lab.sh stop
EOF
}

cmd_start() {
  local mode=run clean=0 args=()
  for a in "$@"; do
    case $a in --net) mode=net ;; --clean) clean=1 ;; *) args+=("$a") ;; esac
  done
  set_guests "${args[@]}"

  c "preflight"
  wait_vault
  preflight "${GS[@]}"
  lesson_notes

  c "boot ($mode)"
  local how=$mode; (( clean )) && how+=", clean"
  for g in "${GS[@]}"; do
    if running "$g"; then ok "$g already running"; continue; fi
    (( clean )) && revert "$g"
    local log="$IMG/${NAME[$g]}/qemu.log"
    setsid -f "${LAUNCH[$g]}" "$mode" > "$log" 2>&1 < /dev/null
    sleep 2
    running "$g" || die "$g failed to start — see $log"
    ok "$g started (log: $log)"
    note "start $g ($how)"
  done

  c "waiting for SSH"
  for g in "${GS[@]}"; do
    local t=0
    until banner "${PORT[$g]}"; do
      running "$g" || die "$g exited during boot — see $IMG/${NAME[$g]}/qemu.log"
      (( t >= 300 )) && { warn "$g: no SSH after 300 s"; continue 2; }
      sleep 3; t=$((t + 3))
    done
    ok "$g: sshd up on 127.0.0.1:${PORT[$g]}"
    post_checks "$g"
  done
  summary
}

cmd_stop() {
  set_guests "$@"
  for g in "${GS[@]}"; do
    running "$g" || { ok "$g not running"; continue; }
    mon "$g" system_powerdown >/dev/null
    printf '   %s: shutting down' "$g"
    for _ in $(seq 60); do running "$g" || break; printf '.'; sleep 2; done
    echo
    if running "$g"; then
      warn "$g still up after 120 s — force: echo quit | socat - UNIX-CONNECT:$IMG/${NAME[$g]}/mon.sock"
    else
      ok "$g stopped"; note "stop $g"
    fi
  done
}

cmd_status() {
  [[ -d "$DF" ]] && ok "vault mounted" || warn "vault locked"
  for g in win sift; do
    if running "$g"; then
      banner "${PORT[$g]}" && ok "$g running, sshd up (:${PORT[$g]})" || warn "$g running, sshd not answering"
    else
      printf '   -- %s stopped\n' "$g"
    fi
  done
  printf '   -- disk: %s free on %s\n' "$(df --output=avail -h "$IMG" | tail -1 | tr -d ' ')" "$IMG"
  if [[ -d "$LESSON" ]]; then printf '   -- lesson dir: %s\n' "$LESSON"; fi
}

cmd_keys() {
  [[ -f "$KEY" ]] || ssh-keygen -t ed25519 -N '' -C dfir-lab -f "$KEY"
  local pub; pub=$(<"$KEY.pub")
  if banner 2223; then
    ssh-copy-id -i "$KEY.pub" -p 2223 -o UserKnownHostsFile="$KNOWN" -o StrictHostKeyChecking=accept-new \
      sansforensics@127.0.0.1
  else warn "sift not reachable: skipped"; fi
  if banner 2222; then
    # quotes don't survive Windows sshd -> powershell -c; ship the script as -EncodedCommand
    local ps='$f = "C:\ProgramData\ssh\administrators_authorized_keys"
if (-not (Test-Path $f) -or -not (Select-String -Path $f -SimpleMatch "__PUB__" -Quiet)) { Add-Content -Path $f -Value "__PUB__" }
icacls $f /inheritance:r /grant "Administrators:F" /grant "SYSTEM:F" | Out-Null'
    # -n + -InputFormat None: Windows PowerShell 5.1 otherwise blocks reading the (redirected) stdin
    ssh -n -p 2222 -o UserKnownHostsFile="$KNOWN" -o StrictHostKeyChecking=accept-new -o LogLevel=ERROR analyst@127.0.0.1 \
      "powershell -NoProfile -NonInteractive -InputFormat None -EncodedCommand $(psenc "${ps//__PUB__/$pub}")"
  else warn "win not reachable: skipped"; fi
  for g in win sift; do banner "${PORT[$g]}" && { sshb "$g" exit && ok "$g: key auth works" || warn "$g: key auth failed"; }; done
  echo "   Keys live inside the guests now: run 'dfir-lab.sh stop && dfir-lab.sh baseline' so --clean keeps them."
}

cmd_baseline() {
  set_guests "$@"
  for g in "${GS[@]}"; do
    running "$g" && die "$g is running: dfir-lab.sh stop $g first"
    local d="$IMG/${NAME[$g]}"
    qemu-img snapshot -l "$d/${DISK[$g]}" | awk '{print $2}' | grep -qx baseline \
      && qemu-img snapshot -d baseline "$d/${DISK[$g]}"
    qemu-img snapshot -c baseline "$d/${DISK[$g]}"
    if [[ $g == win ]]; then
      cp "$d/OVMF_VARS.4m.fd" "$d/OVMF_VARS.baseline.fd"
      rm -rf "$d/tpm.baseline"; cp -a "$d/tpm" "$d/tpm.baseline"
    fi
    ok "$g baseline re-taken $(date +%T)"
  done
}

cmd_dump() {
  local g=${1:-}; valid "$g"
  running "$g" || die "$g not running"
  [[ -d "$DF" ]] || die "vault locked"
  mkdir -p "$LESSON/mem"
  local out; out="$LESSON/mem/${NAME[$g]}-$(date +%H%M%S).elf"
  mon "$g" "dump-guest-memory -d $out" >/dev/null
  printf '   dumping %s' "$out"
  while :; do
    local s; s=$(mon "$g" 'info dump')
    grep -q completed <<<"$s" && break
    grep -q failed <<<"$s" && { echo; die "dump failed"; }
    printf '.'; sleep 2
  done
  echo
  (cd "$LESSON" && sha256sum "mem/$(basename "$out")" | tee -a hashes.txt)
  note "memory dump $g -> mem/$(basename "$out")"
  ok "in SIFT: vol -f /mnt/cases/$TODAY/mem/$(basename "$out") <plugin>"
}

cmd_usb() {
  case ${1:-} in
    "") lsusb; echo "   attach: dfir-lab.sh usb attach <Bus> <Device>   (numbers from above)" ;;
    attach)
      local b=$((10#${2:?bus})) a=$((10#${3:?addr}))
      running win || die "win not running"
      sudo chown "$USER" "/dev/bus/usb/$(printf %03d "$b")/$(printf %03d "$a")"
      mon win "device_add usb-host,hostbus=$b,hostaddr=$a,id=usbev" | grep -v '^QEMU\|^(qemu)' || true
      ok "attached bus $b addr $a to win (id usbev)"; note "USB attach bus $b addr $a -> win"
      ;;
    detach)
      mon win "device_del usbev" | grep -v '^QEMU\|^(qemu)' || true
      ok "detached usbev"; note "USB detach"
      ;;
    *) die "usb [attach <bus> <addr> | detach]" ;;
  esac
}

cmd_ssh() { local g=${1:-}; valid "$g"; shift; sshi "$g" "$@"; }

case ${1:-} in
  start)    shift; cmd_start "$@" ;;
  stop)     shift; cmd_stop "$@" ;;
  status)   cmd_status ;;
  ssh)      shift; cmd_ssh "$@" ;;
  usb)      shift; cmd_usb "$@" ;;
  dump)     shift; cmd_dump "$@" ;;
  keys)     cmd_keys ;;
  baseline) shift; cmd_baseline "$@" ;;
  *)        sed -n '2,15p' "$0" | sed 's/^# \{0,1\}//'; exit 1 ;;
esac
