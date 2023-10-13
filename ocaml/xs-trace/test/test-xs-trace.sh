set -eux

export PORT=9411
export HOST="127.0.0.1"

./test_xs_trace.exe &

PID=$!

../xs_trace.exe cp test-source.json http://$HOST:$PORT/api/v2/spans

if [ $(diff test-source.json test-http-server.out) -ne 0 ]
then
    exit 1
fi

rm test-http-server.out

../xs_trace.exe cp test-source.ndjson http://$HOST:$PORT/api/v2/spans

if [ $(diff test-source.ndjson test-http-server.out) -ne 0 ]
then
    exit 1
fi

rm test-http-server.out

../xs_trace.exe cp test-source.ndjson.zst http://$HOST:$PORT/api/v2/spans

if [ $(diff test-source.ndjson test-http-server.out) -ne 0 ]
then
    exit 1
fi

kill $PID
