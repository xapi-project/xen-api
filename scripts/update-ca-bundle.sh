#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

set -e

regen_bundle () {
  CERTS_DIR="$1"
  BUNDLE="$2"

  mkdir -p "$CERTS_DIR"
  CERTS=$(find "$CERTS_DIR" -not -name '*.new.pem' -name '*.pem')
  NEW_CERTS=$(find "$CERTS_DIR" -name '*.new.pem')

  rm -f "$BUNDLE.tmp"
  touch "$BUNDLE.tmp"
  for NEW_CERT in $NEW_CERTS; do
    # If cat new cert command fails, do not error and exit, just skip it
    if cat "$NEW_CERT" >> "$BUNDLE.tmp"; then
      echo "" >> "$BUNDLE.tmp"
    fi
  done
  for CERT in $CERTS; do
    cat "$CERT" >> "$BUNDLE.tmp"
    echo ""     >> "$BUNDLE.tmp"
  done
  mv "$BUNDLE.tmp" "$BUNDLE"
}

regen_bundle "/etc/stunnel/certs"      "/etc/stunnel/xapi-stunnel-ca-bundle.pem"
regen_bundle "/etc/stunnel/certs-pool" "/etc/stunnel/xapi-pool-ca-bundle.pem"
