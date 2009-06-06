#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

set -e

# Insert a firewall rule to allow traffic to pass through the guest-installer network

CHAIN=xapi-INPUT
IFACE=$1               # bridge name of guest installer network
OP=$2                  # if == start, then start up the firewall, else stop it.

# Flush any rules that are already there:
iptables -F $CHAIN &> /dev/null || true
iptables -D INPUT -j $CHAIN &> /dev/null || true
iptables -X $CHAIN &> /dev/null || true

# Insert the new rule - anything coming from the
if [[ "${OP}" == "start" ]]; then
    iptables -N $CHAIN
    iptables -I INPUT 1 -j $CHAIN
    iptables -A $CHAIN -i $IFACE -j ACCEPT
fi
