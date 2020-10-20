#!/bin/sh

# Use user-writable directories
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/tmp}
export FE_TEST=1

SOCKET=${XDG_RUNTIME_DIR}/xapi/forker/main

../src/fe_main.exe &
MAIN=$!
cleanup () {
    kill $MAIN
}
trap cleanup EXIT
for _ in $(seq 1 10); do
    test -S ${SOCKET} || sleep 1
done
./fe_test.exe 16
