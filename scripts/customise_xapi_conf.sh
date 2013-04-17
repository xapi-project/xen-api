#!/bin/bash

xapiconf=$1

cat >> ${xapiconf} << EOF
xapissl = ${LIBEXECDIR}/xapissl
pool_config_file = ${ETCDIR}/pool.conf
pool_secret_path = ${ETCDIR}/ptoken
pci-info = ${LIBEXECDIR}/pci-info
EOF

