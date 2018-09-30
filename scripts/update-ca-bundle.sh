#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

set -e

mkdir -p /etc/stunnel
find /etc/stunnel/certs -name '*.pem' | xargs cat > /etc/stunnel/xapi-stunnel-ca-bundle.pem
