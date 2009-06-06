#!/bin/sh

set -e

DIR=`dirname $0`

# runs with the debian xvda block-attached as ${DEVICE}
sleep 5s # wait for the partition node to appear

mkdir -p /tmp/debmount
mount /dev/${DEVICE}1 /tmp/debmount
cp $DIR/cli-rt-domu-shar.sh /tmp/debmount/root
chmod +x /tmp/debmount/root/cli-rt-domu-shar.sh
echo "exec /root/cli-rt-domu-shar.sh" > /tmp/debmount/etc/init.d/firstboot
chmod 755 /tmp/debmount/etc/init.d/firstboot
cd /tmp/debmount/etc/rc2.d
ln -s ../init.d/firstboot S99firstboot
cd /
umount /tmp/debmount
rmdir /tmp/debmount

sleep 5s
