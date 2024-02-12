+++
title = "Daemons"
weight = 20
+++

The Toolstack consists of a set of co-operating daemons:

xapi
: manages clusters of hosts, co-ordinating access to shared storage and networking.

xenopsd
: a low-level "domain manager" which takes care of creating, suspending,
resuming, migrating, rebooting domains by interacting with Xen via libxc and
libxl.

xcp-rrdd
: a performance counter monitoring daemon which aggregates "datasources" defined
  via a plugin API and records history for each. There are various rrdd-plugin daemons:
  - xcp-rrdd-gpumon
  - xcp-rrdd-iostat
  - xcp-rrdd-squeezed
  - xcp-rrdd-xenpm

xcp-networkd
: a host network manager which takes care of configuring interfaces, bridges
  and OpenVSwitch instances

squeezed
: a daemon in charge of VM memory management

xapi-storage-script
: for storage manipulation over SMAPIv3

message-switch
: exchanges messages between the daemons on a host

xapi-guard
: forwards uefi and vtpm persistence calls from domains to xapi

v6d
: controls which features are enabled.

forkexecd
: a helper daemon that assists the above daemons with executing binaries and scripts

xhad
: The High-Availability daemon

perfmon
: a daemon which monitors performance counters and sends "alerts"
  if values exceed some pre-defined threshold

mpathalert
: a daemon which monitors "storage paths" and sends "alerts"
  if paths fail and need repair

wsproxy
: handles access to VM consoles
