#!/bin/bash
# Script that edits the host_id in /etc/lvm/lvmlocal.conf
#
# Usage: ./set-lvm-hostid.sh <id>
#

ID=${1}
FILE=/etc/lvm/lvmlocal.conf

sed -i '/host_id/s/ = .*$/ = '${ID}'/; /host_id/s/# //' ${FILE}
