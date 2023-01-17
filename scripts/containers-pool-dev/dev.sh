#!/bin/bash
set -eux

dune build
# container-pool-dev
IMAGE=cpd
# perhaps prefer docker if available since it is almost always orders of
# magnitude faster?
CLI=$(command -v podman docker)
USERMAP="--userns-uid-map=0:1:1000 --userns-uid-map=1000:0:1 --userns-uid-map=1001:1001:64536"

cd $(dirname "$0")
export DOCKER_BUILDKIT=1
# we could also use COPY/ADD for *.opam but sometimes that is very very slow
# with podman, better to just mount it so we get latest
# also podman build doesn't have the right flags for userns=keep-id
# (userns=host doesn't work), so use build to build just depext container,
# and use run for the rest
${CLI} build \
    -t "${IMAGE}" \
    -f Containerfile
