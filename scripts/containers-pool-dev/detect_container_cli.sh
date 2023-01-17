#!/bin/sh
set -eu
COMMAND=/usr/bin/command
CLI=$("${COMMAND}" -v docker podman | head -n1)
if [ -z "${CLI}" ]; then
    HOSTEXEC=$(${COMMAND} -v distrobox-host-exec)
    CLI="${HOSTEXEC} $($HOSTEXEC command -v docker podman | head -n1)"
fi
echo "exec ${CLI} \$@"
