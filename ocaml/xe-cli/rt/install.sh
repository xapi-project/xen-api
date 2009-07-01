#!/bin/sh

# script which installs the CLI RT guest agent
export PATH=/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin

SRCDIR=`dirname $0`
cd $SRCDIR

apt-get update -y -q
apt-get install -y --force-yes -q ocaml dosfstools

cd /root

ocamlc -o gtserver unix.cma gtmessages.ml gtcomms.ml gtlinuxops.ml gtserver_linux.ml

echo "exec /root/gtserver &" > /etc/init.d/xapitest 
chmod 755 /etc/init.d/xapitest
/usr/sbin/update-rc.d xapitest defaults 99

rm /etc/udev/rules.d/z25* /etc/udev/rules.d/z45*

rm -f /etc/rc2.d/S99firstboot

poweroff
