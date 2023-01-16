#!/bin/sh
set -eu

# container-pool-dev
IMAGE=cpd

# we could also use COPY/ADD for *.opam but sometimes that is very very slow
# with podman, better to just mount it so we get latest
podman build \
    -t "${IMAGE}" \
    -v "${HOME}/.cache/dune:/home/opam/.cache/dune:rw,z" \
    -v "${HOME}/.opam/download-cache:/home/opam/.opam/download-cache:rw,z" \
    -v "$(pwd):/home/opam/xapi:rw,z" \
    -f scripts/container-pool-dev/Containerfile \
    ../../
