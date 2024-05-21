#!/bin/bash
set -ex

SCRIPT_PATH=$(cd "$(dirname "$0")" && pwd)

start_jsonrpc_server() {
    echo "Starting JSONRPC server"
    python3 jsonrpc-server/server.py &
    JSONRPC_SERVER_PID=$!
    sleep 1
}

start_jsonrpc_go_client() {
    echo "Starting JSONRPC Go client"

    cd jsonrpc-client/go
    # ensure that all dependencies are satisfied
    go mod tidy
    # build client.go and run it
    go test -v &
    JSONRPC_GO_CLIENT_PID=$!
}

trap 'kill $JSONRPC_SERVER_PID $JSONRPC_GO_CLIENT_PID 2>/dev/null' EXIT

main() {
    cd "$SCRIPT_PATH"
    start_jsonrpc_server
    start_jsonrpc_go_client

    # Wait for the component test to finish
    wait $JSONRPC_GO_CLIENT_PID

    # Shut down the server to reduce future problems when testing other clients.
    kill $JSONRPC_SERVER_PID
}

main
