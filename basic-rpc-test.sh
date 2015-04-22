#!/bin/bash
set -e
set -x

#function finish {
#  killall switch_main.native || true
#  killall server_unix_main.native || true
#  killall server_main.native || true
#}

#trap finish EXIT

rm -f /tmp/switch/*
mkdir -p /tmp/switch
SPATH=/tmp/sock

echo Checking the switch can start late
./server_unix_main.native -path $SPATH &
sleep 1
./switch_main.native --path $SPATH --statedir /tmp/switch &
./client_unix_main.native -path $SPATH -secs 5
sleep 2

echo Performance test of Unix to Unix
./server_unix_main.native -path $SPATH &
./client_unix_main.native -path $SPATH -secs 5
sleep 2

echo Performance test of Lwt to Lwt
./server_main.native -path $SPATH &
./client_main.native -path $SPATH -secs 5
sleep 2

echo Performance test of Async to Lwt
./server_main.native -path $SPATH &
./client_async_main.native -path $SPATH -secs 5
sleep 2

echo Performance test of Async to Async
./server_async_main.native -path $SPATH &
./client_async_main.native -path $SPATH -secs 5
sleep 2

./main.native shutdown --path $SPATH
sleep 2
