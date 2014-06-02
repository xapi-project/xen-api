#!/bin/bash
set -e
set -x

function finish {
  killall switch_main.native || /bin/true
  killall server_unix_main.native || /bin/true
  killall server_main.native || /bin/true
}

trap finish EXIT

echo Checking the switch can start late
./server_unix_main.native -port 8081 &
sleep 1
./switch_main.native -port 8081 -daemon
./client_unix_main.native -port 8081 -secs 5 

echo Performance test of Unix to Unix
./client_unix_main.native -port 8081 -secs 5
killall server_unix_main.native || /bin/true

echo Performance test of Lwt to Lwt
./server_main.native -port 8081 &
./client_main.native -port 8081 -secs 5
