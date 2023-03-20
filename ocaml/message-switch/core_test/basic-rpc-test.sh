#!/bin/bash
set -e

SPATH=${TMPDIR:-/tmp}/sock_s
SWITCHPATH=${TMPDIR:-/tmp}/switch_s
SECS=0.1

rm -rf "${SWITCHPATH}" && mkdir -p "${SWITCHPATH}"

echo Test message switch serial processing

echo Checking the switch can start late
./server_unix_main.exe -path "${SPATH}" &
SERVER=$!
sleep "${SECS}"
../switch/switch_main.exe --path "${SPATH}" --statedir "${SWITCHPATH}" &
./client_unix_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Unix to Unix
./server_unix_main.exe -path "${SPATH}" &
SERVER=$!
./client_unix_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Lwt to Lwt
lwt/server_main.exe -path "${SPATH}" &
SERVER=$!
lwt/client_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Async to Lwt
lwt/server_main.exe -path "${SPATH}" &
SERVER=$!
async/client_async_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

echo Performance test of Async to Async
async/server_async_main.exe -path "${SPATH}" &
SERVER=$!
async/client_async_main.exe -path "${SPATH}" -secs "${SECS}"
wait "${SERVER}"

../cli/main.exe shutdown --path "${SPATH}"
