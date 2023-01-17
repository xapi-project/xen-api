#!/bin/sh
set -eux

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

echo "Running"
# TODO: if changed, restart?

CONTAINER="${IMAGE}-run"
podman inspect -t container "${CONTAINER}" || podman run \
    --name="${CONTAINER}" \
    --userns=keep-id \
    -v "${HOME}/.cache/dune:/home/opam/.cache/dune:rw,z" \
    -v "${HOME}/.opam/download-cache:/home/opam/.opam/download-cache:rw,z" \
    -v "$(pwd):/home/opam/xapi:rw,z" \
    --init \
    "${IMAGE}" sh -c "sleep 1d"&
podman start "${CONTAINER}"
podman exec "${CONTAINER}" opam install -y --deps-only xapi
