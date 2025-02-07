#!/usr/bin/env bash
# parse a PEM file for certificate and key and emit them again as a PEM
# to stdout. This is in response to XSI-1781.

set -o errexit
set -o pipefail
if [[ -n "$TRACE" ]]; then set -o xtrace; fi
set -o nounset

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    cat <<EOF
Usage: $0 /etc/xensource/xapi-ssl.pem

Reformat a PEM file such that it matches expectations from
XenServer 8 about its format. The file is emitted to stdout and
should be re-directed to a file.

EOF
fi

PEM="$1"

openssl x509  -outform PEM -in "$PEM" -out "$PEM.cert.$$"
openssl rsa   -outform PEM -in "$PEM" -out "$PEM.key.$$"
cat "$PEM.key.$$" "$PEM.cert.$$"
rm -f "$PEM.cert.$$" "$PEM.key.$$"

