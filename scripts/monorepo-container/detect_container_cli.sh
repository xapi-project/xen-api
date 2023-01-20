#!/bin/sh
set -eu
COMMAND="command"
CLI=$("${COMMAND}" -v docker podman | head -n1)
if [ -z "${CLI}" ]; then
    HOSTEXEC=$(${COMMAND} -v distrobox-host-exec)
    CLI="${HOSTEXEC} $($HOSTEXEC command -v docker podman | head -n1)"
fi

JOBS=
case "${CLI}" in
    *"podman"*)
        # Buildkit would automatically build in parallel, podman doesn't
        JOBS=" --jobs $(nproc)"
        # FIXME: this can only be added to build but not run commands
        ;;
esac
echo "export DOCKER_BUILDKIT=1 BUILDKIT_PROGRESS=plain PROGRESS_NO_TRUNC=1"
echo "exec ${CLI} \"\$@\""
