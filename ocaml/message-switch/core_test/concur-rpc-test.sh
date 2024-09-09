#!/bin/bash
set -e

SPATH="${TMPDIR:-/tmp}/sock_p-$$"
SWITCHPATH="${TMPDIR:-/tmp}/switch_p-$$"

trap "cleanup" TERM INT

function cleanup {
  rm -rf "${SWITCHPATH}"
}
SECS=${SECS:-0.1}

rm -rf "${SWITCHPATH}" && mkdir -p "${SWITCHPATH}"

echo Test message switch concurrent processing

echo Checking the switch can start late
./server_unix_main.exe -path "${SPATH}" &
SERVER=$!
sleep "${SECS}"
../switch/switch_main.exe --path "${SPATH}" --statedir "${SWITCHPATH}" &
./client_unix_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Lwt to Lwt
lwt/server_main.exe -path "${SPATH}" -concurrent &
SERVER=$!
lwt/client_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Async to Lwt
lwt/server_main.exe -path "${SPATH}" -concurrent &
SERVER=$!
async/client_async_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Async to Async
async/server_async_main.exe -path "${SPATH}" -concurrent &
SERVER=$!
async/client_async_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

../cli/main.exe shutdown --path "${SPATH}"
