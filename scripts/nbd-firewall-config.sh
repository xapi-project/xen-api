#!/bin/bash
#
# Copyright (c) Citrix Systems. All rights reserved.

set -e

##############################################################
# Use this script to open/close the specified interfaces
# (IP addresses) for incoming TCP connections to the NBD port.
#
# Usage:
#   ./nbd-firewall-config set [address...]
#
##############################################################

# Require the "set" argument because otherwise there would be a risk
# of someone running the script with no arguments and accidentally
# setting NBD to be available on no addresses.
# This way, omitting "set" (or supplying "--help") causes the script
# to print usage instructions.

OP="$1"
if [ _"${OP}" != _set ]; then
    echo "Usage: $(basename ${0}) set [address...]
will set the firewall to allow incoming TCP connections
to the NBD port on only the specified addresses (if any)." 1>&2
    exit 1
fi
shift 1

set -eu

TMPSET=xapi_nbd_ipset_ephemeral
NBDSET=xapi_nbd_ipset
NBDPORT=10809
SETTYPE=hash:ip

# Rule to accept new NBD connections on the appropriate addresses
NBD_ACCEPT="-p tcp --dport $NBDPORT -m conntrack --ctstate NEW -m set --match-set $NBDSET dst -j ACCEPT"

# Rules to reject NBD packets on disallowed addresses,
# inbound and outbound,
# even if part of an established connection.
NBD_REJECT_IN="-p  tcp --dport $NBDPORT -m set ! --match-set $NBDSET dst -j REJECT"
NBD_REJECT_OUT="-p tcp --sport $NBDPORT -m set ! --match-set $NBDSET src -j REJECT"

function destroy_tmp {
    ipset destroy $TMPSET 2>/dev/null || true
}

destroy_tmp
trap destroy_tmp ERR EXIT

# Same principle as double-buffering a display:
# add the items one by one to a temporary ipset,
# then swap it into use atomically.
ipset create $TMPSET $SETTYPE

while [ "$#" -ne 0 ]; do
    addr="${1}"
    shift 1
    ipset add $TMPSET "${addr}"
done

ipset create -exist $NBDSET $SETTYPE
ipset swap $TMPSET $NBDSET
ipset destroy $TMPSET

# Now create the rules if they do not exist already.
# Note: the ip set must exist before we can add an iptables rule that uses it.
for rule in "$NBD_ACCEPT" "$NBD_REJECT_IN"; do
    if ! iptables --check INPUT $rule 2>/dev/null
    then
        iptables --insert INPUT $rule
    fi
done

if ! iptables --check OUTPUT $NBD_REJECT_OUT 2>/dev/null
then
    iptables --insert OUTPUT $NBD_REJECT_OUT
fi

exit 0
