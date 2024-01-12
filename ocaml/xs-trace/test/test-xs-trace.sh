#!/bin/bash
set -eux

PORT=9411
HOST="127.0.0.1"
MAX_WAIT=60

./test_xs_trace.exe &

PID=$!

trap "kill $PID" SIGINT SIGTERM EXIT
wait_counter=0
while [ ! -f "test-server-ready" ] && [ $wait_counter -lt $MAX_WAIT ]; do
    sleep 1
    ((wait_counter+=1))
done

../xs_trace.exe cp test-source.json http://$HOST:$PORT/api/v2/spans

diff -B test-source.json test-http-server.out || exit 1

rm test-http-server.out

../xs_trace.exe cp test-source.ndjson http://$HOST:$PORT/api/v2/spans

diff -B test-source.ndjson test-http-server.out || exit 1

