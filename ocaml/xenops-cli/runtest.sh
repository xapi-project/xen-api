#!/bin/sh

PIDFILE=xenopsd.pid

if [ ! -e ./ocaml/xenops-cli/xenopstest ]; then
  echo "Run this from the root of the repo, after a build"
  exit 1
fi

if [ -e ${PIDFILE} ]; then
  kill $(cat ${PIDFILE})
fi
rm -f /tmp/xenopsd.log
./ocaml/xenops/xenopsd -config ocaml/xenops-cli/test.conf -daemon
./ocaml/xenops-cli/xenopstest

