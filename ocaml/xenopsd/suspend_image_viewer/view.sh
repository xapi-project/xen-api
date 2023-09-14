#!/bin/sh
# Usage: copy this script and suspend-image-viewer executable to a box
# Given a snapshot called `snap-mem` view its suspend image VDI:
# /opt/xensource/debug/with-vdi $(xe snapshot-list name-label=uefi-mem params=suspend-VDI-uuid --minimal) ./view.sh
./suspend-image-viewer --config /etc/xenopsd.conf --path /dev/$DEVICE
