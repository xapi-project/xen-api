#!/bin/bash
#
# Copyright (c) Citrix Systems. All rights reserved.

##############################################################
# Use this script to open/close the specified interfaces
# for incoming TCP connections to the NBD port.
#
# Usage:
#   ./nbd-firewall-config set [interface...]
#
##############################################################

# Historical note: the original version of this file filtered
# by local IP addresses rather than by network interfaces;
# that version might one day be a useful reference for something.

# Require the "set" argument because otherwise there would be a risk
# of someone running the script with no arguments and accidentally
# setting NBD to be available on no interfaces.
# This way, omitting "set" (or supplying "--help") causes the script
# to print usage instructions.

set -e
OP="$1"
if [ _"${OP}" != _set ]; then
    echo "Usage: $(basename ${0}) set [interface...]
will set the firewall to allow incoming TCP connections
to the NBD port on only the specified interfaces (if any)." 1>&2
    exit 1
fi
shift 1

set -eu
# No automatic cleanup on error because it would depend too
# critically on the stage at which the error occurs.

# If this script has been run already then there will be existing chains and
# rules in place already. Therefore the approach is:
# 1. Make new nbd-specific chains with names ending in "_new".
# 2. Insert rules into main INPUT/OUTPUT chains to use our new chains.
# 3. Remove preexisting rules that use preexisting nbd chains (if present).
# 4. Remove preexisting nbd chains (if present).
# 5. Rename the new chains so they no longer end in "_new".

# Note that chain names must be under 29 characters.
NewChainIn=xapi_nbd_input_chain_new
NbdChainIn=xapi_nbd_input_chain
NewChainOut=xapi_nbd_output_chain_new
NbdChainOut=xapi_nbd_output_chain

# IANA-assigned port: the NBD protocol specifies that it SHOULD be used.
NBDPORT=10809

# Rule fragments for sending NBD packets to NBD chains
# from INPUT and OUTPUT chains.
NBD_INPUT_JUMP_TO="-p tcp --dport $NBDPORT -j "
NBD_OUTPUT_JUMP_TO="-p tcp --sport $NBDPORT -j "

# Same principle as double-buffering a display:
# add the items one by one to temporary chains,
# then swap them into use atomically, or as near as possible.
iptables --new-chain $NewChainIn 2>/dev/null || iptables --flush $NewChainIn
iptables --new-chain $NewChainOut 2>/dev/null || iptables --flush $NewChainOut
while [ "$#" -ne 0 ]; do
    intf="${1}"
    # (Each positional param is forgotten for ever at the end of the loop.)
    shift 1
    iptables -A $NewChainIn -m conntrack --ctstate NEW,ESTABLISHED --in-interface "${intf}" -j ACCEPT
    iptables -A $NewChainOut --out-interface "${intf}" -j RETURN
done
iptables --append $NewChainIn -j REJECT
iptables --append $NewChainOut -j REJECT

function insert_rule_if_absent {
    chain="${1}"
    shift 1
    rule="${*}"
    iptables --check "${chain}" $rule 2>/dev/null || \
        iptables --insert "${chain}" $rule
}

# Start using the new chains.
insert_rule_if_absent INPUT "${NBD_INPUT_JUMP_TO} ${NewChainIn}"
insert_rule_if_absent OUTPUT "${NBD_OUTPUT_JUMP_TO} ${NewChainOut}"

# If old chains are present, stop using them and then delete them.
iptables --delete INPUT $NBD_INPUT_JUMP_TO $NbdChainIn 2>/dev/null || true
iptables --flush "${NbdChainIn}" 2>/dev/null && \
    iptables --delete-chain "${NbdChainIn}"

iptables --delete OUTPUT $NBD_OUTPUT_JUMP_TO $NbdChainOut 2>/dev/null || true
iptables --flush "${NbdChainOut}" 2>/dev/null && \
    iptables --delete-chain "${NbdChainOut}"

# Rename our rules so they no longer appear new.
# (References remain valid, pointing to the renamed chains.)
iptables --rename-chain "${NewChainIn}" "${NbdChainIn}"
iptables --rename-chain "${NewChainOut}" "${NbdChainOut}"
exit 0
