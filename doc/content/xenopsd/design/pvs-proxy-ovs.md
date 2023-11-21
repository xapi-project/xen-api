+++
title = "PVS Proxy OVS Rules"
+++

# Rule Design

The Open vSwitch (OVS) daemon implements a programmable switch.
XenServer uses it to re-direct traffic between three entities:

  * PVS server - identified by its IP address
  * a local VM - identified by its MAC address
  * a local Proxy - identified by its MAC address

VM and PVS server are unaware of the Proxy; xapi configures OVS to
redirect traffic between PVS and VM to pass through the proxy. 

OVS uses rules that match packets. Rules are organised in sets called
tables. A rule can be used to match a packet and to inject it into
another rule set/table table such that a packet can be matched again.

Furthermore, a rule can set registers associated with a packet which that
can be matched in subsequent rules. In that way, a packet can be tagged
such that it will only match specific rules downstream that match the
tag.

Xapi configures 3 rule sets:

## Table 0 - Entry Rules

Rules match UDP traffic between VM/PVS, Proxy/VM, and PVS/VM where the
PVS server is identified by its IP and all other components by their MAC
address. All packets are tagged with the direction they are going and
re-submitted into Table 101 which handles ports.

## Table 101 - Port Rules

Rules match UDP traffic going to a specific port of the PVS server and
re-submit it into Table 102.

## Table 102 - Exit Rules

These rules implement the redirection: 

* Rules matching packets coming from VM to PVS are directed to the Proxy.
* Rules matching packets coming from PVS to VM are directed to the Proxy.
* Rules matching packets coming from the Proxy are already addressed
  properly (to the VM) are handled normally.



