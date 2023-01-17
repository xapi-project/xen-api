#!/bin/sh
set -eu

# container-pool-dev
IMAGE=cpd

# we could also use COPY/ADD for *.opam but sometimes that is very very slow
# with podman, better to just mount it so we get latest
# also podman build doesn't have the right flags for userns=keep-id
# (userns=host doesn't work), so use build to build just depext container,
# and use run for the rest
podman build \
    -t "${IMAGE}" \
    -f scripts/container-pool-dev/Containerfile \
    ../../

podman run \
    --name="${IMAGE}-run" \
    --userns=keep-id \
    "${IMAGE}" \
    -v "${HOME}/.cache/dune:/home/opam/.cache/dune:rw,z" \
    -v "${HOME}/.opam/download-cache:/home/opam/.opam/download-cache:rw,z" \
    -v "$(pwd):/home/opam/xapi:rw,z" \
    opam install -y --deps-only xapi
