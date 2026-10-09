#!/usr/bin/env bash
# SIFT guest setup — run over SSH (re-runnable):
#   scp -P 2223 setup.sh sansforensics@127.0.0.1: && ssh -t -p 2223 sansforensics@127.0.0.1 'bash setup.sh'
# Prerequisite (console, once): sudo apt install -y openssh-server && sudo systemctl enable --now ssh
set -euo pipefail

echo "== 9p shares"
sudo mkdir -p /mnt/evidence /mnt/cases
add_fstab() { grep -qxF "$1" /etc/fstab || echo "$1" | sudo tee -a /etc/fstab >/dev/null; }
add_fstab 'evidence /mnt/evidence 9p trans=virtio,version=9p2000.L,msize=262144,ro,nofail 0 0'
add_fstab 'cases /mnt/cases 9p trans=virtio,version=9p2000.L,msize=262144,nofail 0 0'
sudo systemctl daemon-reload
sudo mount -a
df -h /mnt/evidence /mnt/cases
touch /mnt/cases/.w && rm /mnt/cases/.w && echo "cases: rw OK"
touch /mnt/evidence/.w 2>/dev/null && { rm -f /mnt/evidence/.w; echo "evidence: WRITABLE (should be ro)"; } || echo "evidence: ro OK"

echo "== RegRipper (cylab.be/blog/287 fix only if broken)"
rip_out=$(rip.pl -l 2>&1 || true)
if grep -q 'Global symbol "\$plugindir"' <<<"$rip_out"; then
  f=$(readlink -f "$(command -v rip.pl)")
  sudo cp "$f" "$f.back"
  sudo sed -i '66i my $plugindir = File::Spec->catfile($scriptdir, "plugins");' "$f"
fi
rip.pl -l 2>&1 | head -n 2 || true

echo "== tools"
for t in vol vol.py mmls fls icat ewfmount rip.pl evtxinfo tcpdump; do
  printf '%-10s %s\n' "$t" "$(command -v "$t" || echo MISSING)"
done
