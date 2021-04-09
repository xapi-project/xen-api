#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

set -e

ST="/etc/stunnel"

mkdir -p ${ST}/certs
find ${ST}/certs -name '*.pem' | xargs cat > ${ST}/xapi-stunnel-ca-bundle.tmp
mv ${ST}/xapi-stunnel-ca-bundle.tmp ${ST}/xapi-stunnel-ca-bundle.pem

mkdir -p ${ST}/certs-pool
find ${ST}/certs-pool -name '*.pem' | xargs cat > ${ST}/xapi-pool-ca-bundle.tmp
mv ${ST}/xapi-pool-ca-bundle.tmp ${ST}/xapi-pool-ca-bundle.pem
