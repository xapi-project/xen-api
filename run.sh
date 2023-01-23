#!/bin/sh
# docker run -v /dev/log:/dev/log -v $(pwd)/run.sh:/home/opam/run.sh -it --rm cpd
set -eux
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

sudo chown  1000:1000 /etc/xensource/pool.conf
echo master >/etc/xensource/pool.conf
sudo mkdir -p /var/run/message-switch
sudo chown 1000:1000 /var/run/message-switch
sudo mkdir -p /var/xapi
sudo chown 1000:1000 /var/xapi

sudo touch /etc/xapi-networkd.conf
sudo chown 1000:1000 /etc/xapi-networkd.conf
echo network-conf=/etc/xcp/network.conf >/etc/xapi-networkd.conf

sudo mkdir  -p /etc/xcp
sudo touch /etc/xcp/network.conf
sudo chown 1000:1000 /etc/xcp/network.conf
echo bridge >/etc/xcp/network.conf


prefix/sbin/message-switch &
sleep 1
prefix/sbin/forkexecd &
prefix/bin/xapi-networkd&
prefix/bin/squeezed&
export OCAMLRUNPARAM=b
prefix/bin/xapi -daemon false -pidfile /tmp/xapi.pid -nowatchdog -use-switch false
