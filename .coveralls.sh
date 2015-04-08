#!/bin/sh

eval `opam config env`
opam install ocveralls -y
make
BISECT_FILE=_build/coverage ./basic-rpc-test.sh
`opam config var bin`/ocveralls --prefix _build _build/coverage*.out --send
