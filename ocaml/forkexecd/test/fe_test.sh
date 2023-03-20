#!/bin/sh

export OCAMLRUNPARAM=b
# Use user-writable directories
export TMPDIR=${TMPDIR:-/tmp}
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-$TMPDIR}
export FE_TEST=1

SOCKET=${XDG_RUNTIME_DIR}/xapi/forker/main

ulimit -n 1024
../src/fe_main.exe &
MAIN=$!
cleanup () {
    kill $MAIN
}
trap cleanup EXIT
for _ in $(seq 1 100); do
    test -S ${SOCKET} && break
    sleep 0.1
done
echo "" | ./fe_test.exe 16
