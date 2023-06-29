#!/bin/bash
set -eu
WRAPPER=/usr/lib64/xen/bin/swtpm-wrapper

tpm()
{
    swtpm_ioctl --unix "${DIR}/swtpm-sock" "$@"
}

smoketest_cmds()
{
    tpm -c
    tpm -i
    tpm -e
    tpm -h foobar
    tpm -g
    tpm -v
    tpm --info 3

    echo "About to save all vTPM states"
    # volatilestate must be first, 'tpm -v' must be previous command, see manpage
    for state in volatile permanent savestate; do
        tpm --save "${state}" test."${state}"
    done

    # must be run before load, note that shutdown != stop
    echo "Stopping vTPM"
    tpm --stop
    echo "Loading all vTPM state"
    for state in volatile permanent savestate; do
        tpm --load "${state}" test."${state}"
    done
    echo "(Re)initializing vTPM from loaded state"
    tpm -i

    echo "Stopping vTPM emulator"
    tpm -s
}

DOMID=100000
GID=$(( $(id swtpm_base -u) + ${DOMID} ))

smoketest_wrapper()
{
    URL="$1"
    echo
    echo "=== TESTING state backend ${URL} ==="
    echo
    echo "Initializing vTPM state in ${DIR}"
    "${WRAPPER}" "${DOMID}" "${DIR}" dir://. true

    inotifywait --exclude '.*.pid' -e create "${DIR}"&
    INOTIFY_PID=$!

    echo "Starting vTPM emulator in ${DIR}"
    "${WRAPPER}" "${DOMID}" "${DIR}" "${URL}" false&
    WRAPPER_PID=$!

    echo "Waiting for socket"
    # wait for socket
    wait "${INOTIFY_PID}"
    echo "Got socket, running vTPM commands"

    smoketest_cmds

    echo "Waiting for vTPM emulator to finish"
    wait "${WRAPPER_PID}"

    echo
    echo "=== OK ${URL} ==="
    echo
}


smoketest_url()
{
    DIR=$(mktemp -d)
    trap "rm -rf '${DIR}'" EXIT

    smoketest_wrapper "$1"
}

smoketest_guard()
{
    echo
    echo "=== TESTING xapi-guard<->swtpm communication ==="
    echo
    VM=$(xe vm-create --minimal name-label="vtpm test")
    trap 'xe vm-destroy uuid="${VM}"' EXIT
    xe vtpm-create vm-uuid="${VM}"

    # TODO: change to new API with Pau's branch
    # domid picked here is beyond max, so shouldn't clash
    DIR="/var/lib/xcp/run/swtpm-root-${DOMID}/"

    # cleanup from previous test potentially
    xapiguard_cli vtpm_destroy smoketest "${DOMID}" "${DIR}/xapidepriv"
    rm -rf "${DIR}"

    mkdir -p "${DIR}"
    trap 'rm -rf "${DIR}"' EXIT
    echo "GID: ${GID}"
    xapiguard_cli vtpm_create smoketest "\"${VM}\"" "${GID}" "${DIR}/xapidepriv"
    trap 'xapiguard_cli vtpm_destroy smoketest "${GID}" "${DIR}/xapidepriv"' EXIT

    smoketest_wrapper "unix+http://xapidepriv"

    echo
    echo "=== OK xapi-guard<->swtpm communication ==="
    echo
}
smoketest_xapi()
{
    echo
    echo "=== TESTING xapi vtpm creation on paused VM ==="
    echo
    VM=$(xe vm-create --minimal name-label="vtpm test")
    trap 'xe vm-destroy uuid="${VM}"' EXIT
    xe vm-param-set uuid="${VM}" HVM-boot-params:firmware=uefi
    xe vm-param-set uuid="${VM}" domain-type=hvm

    xe vtpm-create vm-uuid="${VM}"

    xe vm-start uuid="${VM}" paused=true
    #DOMID=$(xe vm-param-get uuid="${VM}" param-name=dom-id)
    #DIR="/var/lib/xcp/run/swtpm-root-${DOMID}/"
    # we can't attach swtpm_ioctl because qemu is already attached

    SNAP=$(xe vm-snapshot uuid="${VM}" new-name-label=vtpm-snapshot)
    xe snapshot-destroy uuid="${SNAP}"
    CHECKPOINT=$(xe vm-checkpoint uuid="${VM}" new-name-label=vtpm-checkpoint)
    xe snapshot-revert snapshot-uuid="${CHECKPOINT}"
    xe snapshot-destroy uuid="${CHECKPOINT}"

    # no need, revert already did this: xe vm-shutdown uuid="${VM}" --force
    echo
    echo "=== OK xapi vtpm ==="
    echo
}

smoketest_url "dir://."

smoketest_guard
# TODO: start small HTTP server that supports GET/PUT/DELETE and use an http
# url

smoketest_xapi
