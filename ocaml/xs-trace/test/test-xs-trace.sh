set -eux

export PORT=9411
export HOST="127.0.0.1"

./test_xs_trace.exe &

PID=$!

../xs_trace.exe cp test-source.json http://$HOST:$PORT/api/v2/spans

kill $PID

if [ $(diff test-source.json test-http-server.out | grep "<") ]
then
    exit 1
fi