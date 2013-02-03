#!/bin/sh
if [ -z "${BINDIR}" ]
then
  echo Please set BINDIR
  exit 1
fi
install -m 0755 dist/build/sm-cli/sm-cli ${BINDIR}
