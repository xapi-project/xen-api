#!/bin/bash
set -e

DATE=$(date "+%Y-%m-%d")

if [[ -z "$XAPI_VERSION" ]]; then
  printf "XAPI_VERSION not set" 1>&2
  exit 1
fi
XAPI_VERSION="${XAPI_VERSION#v}"

printf "let date = \"%s\"\n\n" "$DATE"
printf "let version = \"%s\"\n" "$XAPI_VERSION"
