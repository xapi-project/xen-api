FROM alpine:latest AS build

RUN apk add opam alpine-sdk

RUN opam init -y --disable-sandboxing --comp=4.10.0
RUN opam install depext -y
COPY . /src
RUN opam pin add qcow.dev /src -n
RUN opam depext -i qcow -y
RUN opam pin add qcow-tool.dev /src -n
RUN opam depext -i qcow-tool -y

FROM alpine:latest
COPY --from=build /root/.opam/4.10.0/bin/qcow-tool /qcow-tool
ENTRYPOINT ["/qcow-tool"]

