#!/bin/sh

mkdir /lib/modules/4.19.0+1/xenserver/bnx2fc/5.4.3
touch /lib/modules/4.19.0+1/xenserver/bnx2fc/5.4.3/bnx2fc.ko
rm -rf /lib/modules/4.19.0+1/xenserver/bnx2fc/2.12.20-dell
rm -rf /lib/modules/4.19.0+1/xenserver/dell_laptop

