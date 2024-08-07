#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

set -e

mkdir -p /etc/stunnel/certs
find /etc/stunnel/certs -name '*.pem' | xargs cat > /etc/stunnel/xapi-stunnel-ca-bundle.pem.tmp
mv /etc/stunnel/xapi-stunnel-ca-bundle.pem.tmp /etc/stunnel/xapi-stunnel-ca-bundle.pem
