# Stage 1: JsonRPC Server
FROM python:alpine AS jsonrpc-server
WORKDIR /app/server
COPY jsonrpc-server/*.py .
RUN pip install aiohttp
CMD ["python", "server.py"]

# Stage 2: Go Client
FROM golang:alpine AS test-go-client
WORKDIR /app/client
ENV GO111MODULE=off
COPY jsonrpc-client/client.go .
RUN go build -o client .
CMD ["./client"]