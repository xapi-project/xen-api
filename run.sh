#!/bin/sh
# docker run -v /dev/log:/dev/log -v $(pwd)/run.sh:/home/opam/run.sh -it --rm cpd
set -eux
DIRS="/var/lib/xcp /var/run/nonpersistent /etc/xensource"
mkdir -p $DIRS
echo "$(uuidgen)/$(uuidgen)/$(uuidgen)" >/etc/xensource/ptoken
MANAGEMENT_INTERFACE=$(basename $(find /sys/class/net -name 'e*' | head -n1))
cat >/etc/xensource-inventory <<EOF
INSTALLATION_UUID=$(uuidgen)
CONTROL_DOMAIN_UUID=$(uuidgen)
MANAGEMENT_INTERFACE=${MANAGEMENT_INTERFACE}
MANAGEMENT_ADDRESS_TYPE=IPv4
EOF

echo master >/etc/xensource/pool.conf

cat >/opt/xensource/libexec/bfs-interfaces <<EOF
#!/bin/sh
echo "${MANAGEMENT_INTERFACE}"
EOF
chmod +x /opt/xensource/libexec/bfs-interfaces

mkdir -p /var/run/message-switch
mkdir -p /var/xapi

echo network-conf=/etc/xcp/network.conf >/etc/xapi-networkd.conf

mkdir  -p /etc/xcp
echo bridge >/etc/xcp/network.conf

prefix/sbin/message-switch &
sleep 1
prefix/sbin/forkexecd &
prefix/bin/xapi-networkd&
prefix/bin/squeezed&
export OCAMLRUNPARAM=b
prefix/bin/xapi -daemon false -pidfile /tmp/xapi.pid -nowatchdog
