#!/bin/sh

PIDFILE=xenopsd.pid

if [ ! -e ./ocaml/xenops-cli/xenopstest ]; then
  echo "Run this from the root of the repo, after a build"
  exit 1
fi

if [ -e ${PIDFILE} ]; then
  kill $(cat ${PIDFILE})
fi
rm -f /var/log/xenopsd/xenopsd.log
./ocaml/xenops/xenopsd -pidfile ${PIDFILE} -simulate -clean -daemon
sleep 5
./ocaml/xenops-cli/xenopstest

