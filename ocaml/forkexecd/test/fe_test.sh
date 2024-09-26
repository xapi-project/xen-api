#!/bin/sh

# Use user-writable directories
export TMPDIR=${TMPDIR:-/tmp}
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-$TMPDIR}
export FE_TEST=1

SOCKET=${XDG_RUNTIME_DIR}/xapi/forker/main
rm -f "$SOCKET"

LD_PRELOAD="$PWD/syslog.so" \
TEST_VFORK_HELPER="$PWD/../helper/vfork_helper" \
../src/fe_main.exe &
MAIN=$!
cleanup () {
    kill $MAIN
}
trap cleanup EXIT INT
for _ in $(seq 1 10); do
    test -S ${SOCKET} || sleep 1
done
echo "" | LD_PRELOAD="$PWD/syslog.so" \
TEST_VFORK_HELPER="$PWD/../helper/vfork_helper" \
./fe_test.exe 16
