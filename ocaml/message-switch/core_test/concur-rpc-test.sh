#!/bin/bash
set -e

SPATH="${TMPDIR:-/tmp}/sock_p-$$"
SWITCHPATH="${TMPDIR:-/tmp}/switch_p-$$"

trap "cleanup" TERM INT

function cleanup {
  rm -rf "${SWITCHPATH}"
}

rm -rf "${SWITCHPATH}" && mkdir -p "${SWITCHPATH}"

echo Test message switch concurrent processing

echo Checking the switch can start late
test -x ./server_unix_main.exe || exit 1
./server_unix_main.exe -path "$SPATH" &
sleep 1
test -x ../switch/switch_main.exe && test -x ./client_unix_main.exe || exit 1
../switch/switch_main.exe --path "$SPATH" --statedir "${SWITCHPATH}" &
./client_unix_main.exe -path "$SPATH" -secs 5
sleep 2

echo Performance test of Lwt to Lwt
test -x lwt/server_main.exe && test -x lwt/client_main.exe || exit 1
lwt/server_main.exe -path "$SPATH" -concurrent &
lwt/client_main.exe -path "$SPATH" -secs 5
sleep 2

echo Performance test of Async to Lwt
test -x lwt/server_main.exe && test -x async/client_async_main.exe || exit 1
lwt/server_main.exe -path "$SPATH" -concurrent &
async/client_async_main.exe -path "$SPATH" -secs 5
sleep 2

echo Performance test of Async to Async
test -x async/server_async_main.exe && test -x async/client_async_main.exe || exit 1
async/server_async_main.exe -path "$SPATH" -concurrent &
async/client_async_main.exe -path "$SPATH" -secs 5
sleep 2

../cli/main.exe shutdown --path "$SPATH"
sleep 2
