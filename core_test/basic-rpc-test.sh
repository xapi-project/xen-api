#!/bin/bash
set -e

SPATH=/tmp/sock
SWITCHPATH=/tmp/switch


rm -rf ${SWITCHPATH} && mkdir -p ${SWITCHPATH}

echo Checking the switch can start late
./server_unix_main.exe -path $SPATH &
sleep 1
../switch/switch_main.exe --path $SPATH --statedir ${SWITCHPATH} &
./client_unix_main.exe -path $SPATH -secs 5
sleep 2

echo Performance test of Unix to Unix
./server_unix_main.exe -path $SPATH &
./client_unix_main.exe -path $SPATH -secs 5
sleep 2

echo Performance test of Lwt to Lwt
lwt/server_main.exe -path $SPATH &
lwt/client_main.exe -path $SPATH -secs 5
sleep 2

echo Performance test of Async to Lwt
lwt/server_main.exe -path $SPATH &
async/client_async_main.exe -path $SPATH -secs 5
sleep 2

echo Performance test of Async to Async
async/server_async_main.exe -path $SPATH &
async/client_async_main.exe -path $SPATH -secs 5
sleep 2

../cli/main.exe shutdown --path $SPATH
sleep 2
