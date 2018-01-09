set -eux

trap 'kill $(jobs -p)' EXIT

./test_server.exe &
sleep 1

./test_client.exe

