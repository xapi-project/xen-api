#!/bin/bash
set -e

SPATH=${TMPDIR:-/tmp}/sock
SWITCHPATH=${TMPDIR:-/tmp}/switch


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

echo Performance test of Lwt to Unix
lwt/server_main.exe -path $SPATH &
./client_unix_main.exe -path $SPATH -secs 5

../cli/main.exe shutdown --path $SPATH
sleep 2
