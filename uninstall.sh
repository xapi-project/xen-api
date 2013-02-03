#!/bin/sh
if [ -z "${BINDIR}" ]
then
  echo Please set BINDIR
  exit 1
fi
rm -f ${BINDIR}/sm-cli
