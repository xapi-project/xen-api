#!/bin/sh
# docker run -v /dev/log:/dev/log -v $(pwd)/run.sh:/home/opam/run.sh -it --rm cpd
set -eu
DIRS="/var/lib/xcp /var/run/nonpersistent /etc/xensource"
sudo mkdir -p $DIRS
for d in $DIRS; do sudo chown 1000:1000 $d; done
echo "$(uuidgen)/$(uuidgen)/$(uuidgen)" >/etc/xensource/ptoken
sudo touch /etc/xensource-inventory
sudo chown 1000:1000 /etc/xensource-inventory
cat >/etc/xensource-inventory <<EOF
INSTALLATION_UUID=$(uuidgen)
CONTROL_DOMAIN_UUID=$(uuidgen)
MANAGEMENT_INTERFACE=eth0
EOF
workspace/_build/install/default/bin/xapi -daemon false -pidfile /tmp/xapi.pid -nowatchdog
