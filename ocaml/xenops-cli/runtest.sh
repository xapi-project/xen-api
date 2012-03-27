#!/bin/sh

PIDFILE=xenopsd.pid

if [ ! -e ./ocaml/xenops-cli/xenopstest ]; then
  echo "Run this from the root of the repo, after a build"
  exit 1
fi

rm -f /tmp/xenopsd.log
./ocaml/xenops/xenopsd -config ocaml/xenops-cli/test.conf > /tmp/xenopsd.log &

# wait for the socket to be found
RETRIES=60
while [ ${RETRIES} -ne 0 ]; do
	./ocaml/xenops-cli/xn list 2> /dev/null > /dev/null
	if [ $? == 0 ]; then
        RETRIES=0
    else
		sleep 1
		echo -n .
		RETRIES=$(( ${RETRIES} - 1 ))
    fi
done

./ocaml/xenops-cli/xenopstest


if [ -e ${PIDFILE} ]; then
  kill $(cat ${PIDFILE})
  rm -f ${PIDFILE}
fi
