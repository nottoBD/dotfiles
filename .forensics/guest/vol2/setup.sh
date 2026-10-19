#!/usr/bin/env bash
# setup.sh — Vol2 guest (cylab.be "Mint 19.3 Volatility" OVA) for the DFIR lab.
# Rebuild path only: replays what was done by hand on 2026-10-10.
# Run inside the guest as vagrant, with id_dfir_lab.pub next to this script:
#   host: vol2-dfir.sh net ; guest console: sudo apt install -y openssh-server
#   host: scp -P 2224 setup.sh ~/.ssh/id_dfir_lab.pub vagrant@127.0.0.1: && ssh -t -p 2224 vagrant@127.0.0.1 'bash setup.sh'
# Then reboot the guest, and on the host: dfir-lab.sh stop && dfir-lab.sh baseline vol2
set -euo pipefail
cd "$(dirname "$0")"
PUB=id_dfir_lab.pub
[[ -f $PUB ]] || { echo "missing $PUB next to setup.sh" >&2; exit 1; }

echo "== ssh: key only"
install -d -m 700 ~/.ssh
install -m 600 "$PUB" ~/.ssh/authorized_keys
if grep -qE '^#?PasswordAuthentication' /etc/ssh/sshd_config; then
  sudo sed -i -E 's/^#?PasswordAuthentication.*/PasswordAuthentication no/' /etc/ssh/sshd_config
else
  echo 'PasswordAuthentication no' | sudo tee -a /etc/ssh/sshd_config >/dev/null
fi
sudo systemctl restart ssh

echo "== 9p shares: evidence read-only, cases read-write"
sudo mkdir -p /mnt/evidence /mnt/cases
grep -qE '^evidence[[:space:]]' /etc/fstab || \
  echo 'evidence /mnt/evidence 9p trans=virtio,version=9p2000.L,msize=262144,ro,nofail 0 0' | sudo tee -a /etc/fstab >/dev/null
grep -qE '^cases[[:space:]]' /etc/fstab || \
  echo 'cases /mnt/cases 9p trans=virtio,version=9p2000.L,msize=262144,nofail 0 0' | sudo tee -a /etc/fstab >/dev/null

echo "== 9p modules in the initramfs (otherwise systemd mounts before 9pnet_virtio is loaded)"
for m in 9p 9pnet 9pnet_virtio; do
  grep -qx "$m" /etc/initramfs-tools/modules || echo "$m" | sudo tee -a /etc/initramfs-tools/modules >/dev/null
done
sudo update-initramfs -u

echo "== text console at boot (Cinnamon intercepts the ACPI power button, so 'stop' would hang)"
sudo systemctl set-default multi-user.target

echo "== Volatility 2 Linux profile LinuxDebian5010x86 (03.2 victoria-v8)"
P=/opt/volatility/volatility/plugins/overlays/linux
[[ -f $P/Debian5010.zip ]] || sudo wget -q -O "$P/Debian5010.zip" \
  https://raw.githubusercontent.com/volatilityfoundation/profiles/master/Linux/Debian/x86/Debian5010.zip
if vol.py --info 2>/dev/null | grep -q LinuxDebian5010x86; then
  echo "   ok profile LinuxDebian5010x86"
else
  echo "!! profile not visible: check $P/Debian5010.zip" >&2
fi

echo "== done: sudo reboot, then on the host: dfir-lab.sh stop && dfir-lab.sh baseline vol2"
