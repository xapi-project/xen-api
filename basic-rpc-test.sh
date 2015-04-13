#!/bin/bash
set -e
set -x

#function finish {
#  killall switch_main.native || true
#  killall server_unix_main.native || true
#  killall server_main.native || true
#}

#trap finish EXIT

echo Checking the switch can start late
./server_unix_main.native -port 8081 &
sleep 1
./switch_main.native -port 8081 &
./client_unix_main.native -port 8081 -secs 5
sleep 2

echo Performance test of Unix to Unix
./server_unix_main.native -port 8081 &
./client_unix_main.native -port 8081 -secs 5
sleep 2

echo Performance test of Lwt to Lwt
./server_main.native -port 8081 &
./client_main.native -port 8081 -secs 5
sleep 2

echo Performance test of Async to Lwt
./server_main.native -port 8081 &
./client_async_main.native -port 8081 -secs 5
sleep 2

echo Performance test of Async to Async
./server_async_main.native -port 8081 &
./client_async_main.native -port 8081 -secs 5
sleep 2

./main.native shutdown --port 8081
sleep 2
