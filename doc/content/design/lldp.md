---
title: LLDP support
layout: default
design_doc: true
revision: 1
status: draft
---

## Overview

[LLDP](https://en.wikipedia.org/wiki/Link_Layer_Discovery_Protocol) is a link-layer discovery protocol used by network devices to advertise their identity, capabilities, and neighbor information on a local network.
This design adds supported LLDP capability to XAPI and networkd so that a host can advertise its identity and selected system information to directly connected switches, and can also retrieve selected LLDP neighbor information from those switches.
The primary use case is host-to-switch verification.

The following are introduced by this design:

- pool-wide and per-PIF LLDP configuration through XenAPI
- LLDP advertisement on physical NICs
- retrieval of selected received LLDP neighbor fields through `PIF_metrics`
- protection against enabling LLDP by default on NIC drivers known to conflict with storage-related functionality

The implementation uses XAPI for configuration, networkd for per-host application of configuration, and [`lldpd`](https://github.com/lldpd/lldpd) as the LLDP agent in dom0 user space.
XAPI stores LLDP configuration in the database and exposes LLDP neighbor data through XenAPI.
networkd configures the LLDP agent to apply LLDP configuration for individual physical NICs and queries the LLDP agent for received LLDP TLVs.
`lldpd` runs as a daemon process in dom0 user space. It receives and sends LLDPDUs via PF_PACKET + SOCK_RAW sockets. It is actively maintained by upstream as the time being.

## XAPI database changes

The following fields are added to the XAPI database.

### `pool.lldp_enabled`

Type: `bool`

When `true`, LLDP is enabled on the NIC associated with each managed physical PIF on every host in the pool.
When `false`, LLDP is disabled on the NIC associated with each managed physical PIF on every host in the pool.

This setting does not apply to other types of PIFs, such as non-managed PIFs, bond PIFs, VLAN PIFs, tunnel PIFs, or SR-IOV PIFs.

`PIF.lldp_mode` determines the final effective state on individul PIF.

LLDP receiving and advertising are always enabled or disabled together.

Default after update/RPU from a version/release without LLDP support to a version/release with LLDP support: `false`

Default after fresh install: `true`

### `PIF.lldp_mode`

Type: `enum pif_lldp_mode`

Values:

- `default`: follow `pool.lldp_enabled`;
- `enabled`: LLDP is enabled on the NIC associated with the managed physical PIF;
- `disabled`: LLDP is disabled on the NIC associated with the managed physical PIF.

This setting does not apply to other types of PIFs, such as non-managed PIFs, bond PIFs, VLAN PIFs, tunnel PIFs, or SR-IOV PIFs.

Default after update/RPU from a version/release without LLDP support to a version/release with LLDP support: `default`.

Default after fresh install: `default`.

### `pool.lldp_multicast_address`

Type: `enum lldp_multicast_address`

Values:

- `nearestbridge`: `01:80:C2:00:00:0E`
- `nearestnontpmrbridge`: `01:80:C2:00:00:03`
- `nearestcustomerbridge`: `01:80:C2:00:00:00`

This value controls the multicast MAC address used for LLDP transmission.
After a change, it is applied when `pool.set_lldp_enabled` or `PIF.set_lldp_mode` is called with `force=true`.
This value is not considered to change often. Changing it does not trigger any application action for simplicity.

Default after update/RPU from a version/release without LLDP support to a version/release with LLDP support: `nearestbridge`.

Default after fresh install: `nearestbridge`.

### `PIF_metrics.lldp_neighbor`

Type: `map(string, string)`

Stores the received LLDP TLVs from the corresponding PIF.

Default: empty

## XenAPI changes

The following new APIs are added.

### `pool.set_lldp_enabled`

Parameters:

- `self`: the pool reference;
- `value`: `true` or `false`;
- `force`: `bool`, default `false`.

Behavior:

- if `force=false` and `value = pool.lldp_enabled`, do nothing;
- otherwise, set `pool.lldp_enabled` to `value`, apply LLDP configuration to every physical PIF in the pool by calling `PIF.plug` to each host;
- return a map of failed PIFs and error strings.

### `PIF.set_lldp_mode`

Parameters:

- `self`: the PIF reference;
- `value`: `default`, `enabled`, or `disabled`;
- `force`: `bool`, default `false`.

Behavior:

- if `force=false` and `value= PIF.lldp_mode`, do nothing;
- otherwise set `PIF.lldp_mode` to `value`, apply LLDP configuration to the physical NIC represented by the PIF by calling `PIF.plug` to the host.


## The networkd database

The `interface_config_t` record in the networkd database is extended with LLDP configuration. networkd can configure LLDP independently using its own database when XAPI is unavailable, for example during host boot.
The default enabled setting is `false` to minimize impact without high-level configuration from XAPI or the user. This database can be updated as XAPI pushes configurtions to networkd through networkd calls.

```ocaml
type lldp_multicast_address =
  | Nearestbridge
  | Nearestnontpmrbridge
  | Nearestcustomerbridge
[@@deriving rpcty]

type lldp = {
  force: bool [@default false] (* Override the blocking list when true. See below. *)
; chassis_id: string [@default ""]
; system_name: string [@default ""]
; system_description: string [@default ""]
; enabled: bool [@default false];
; address: lldp_multicast_address list [@default [Nearestbridge]];
}
[@@deriving rpcty]

type interface_config_t = {
  ...
  lldp: lldp option [@default None];
}
```

## Safety

Some NICs share hardware with storage functions such as CNA and hardware FCoE. Enabling LLDP on these NICs may affect those storage functions.
This is especially important when the storage function provides the boot disk of a host.
For safety, LLDP is disabled by default on NICs which are managed by drivers in a known blocking list when enablement comes from the pool-level default.
Users may still enable LLDP on such NICs through per-PIF XenAPI after confirming that it is safe in practice.
The blocking mechanism is based on the NIC driver. Other approaches, such as PCI bus/device location or PCI device ID, are either unreliable or too complex.

### Blocking list

networkd uses a per-host NIC-driver blocking list which is persisted under files under `/etc/xensource/lldp-nic-driver-blocklist.d/` with content like:

```text
bnx2x
enic
qede
```
It's a directory to allow other components like host install drop custom drivers to avoid impact on installation.

### Configuration matrix

The effective LLDP state on a NIC is determined by `pool.lldp_enabled`, `PIF.lldp_mode`, and whether the NIC driver is in the blocking list used by networkd.

| `pool.lldp_enabled` | `PIF.lldp_mode` | parameter passed to networkd | NIC driver in blocking list | effective LLDP state |
| --- | --- | --- | --- | --- |
| `true` | `default` | `true` | `no` | `enabled` |
| `true` | `default` | `true` | `yes` | `disabled` |
| `false` | `default` | `false` | `*` | `disabled` |
| `*` | `enabled` | `true` | `*` | `enabled` |
| `*` | `disabled` | `false` | `*` | `disabled` |

### Advertised LLDP TLVs

When LLDP is enabled on a physical NIC, the LLDP agent advertises the following TLVs.

| TLV | Value |
| --- | --- |
| Chassis ID | Host UUID (shared by all individual interfaces) |
| Port ID | Interface MAC address |
| Port Description | Interface name |
| System Name | XAPI host.name_label |
| System Description | XAPI host.name_description |
| Management Address | Management IP address |
| System Capabilities | Bridge |
| TTL | Default value from the LLDP agent |

Some advertised values follow the default behavior of `lldpd`, while others are configured by networkd when preparing the LLDP agent configuration.

### Report received LLDP TLVs

networkd periodically queries statistics for individual NICs and writes them to the in-memory file `/dev/shm/network_stats`. The file format is defined in `ocaml/xapi-idl/network/network_stats.ml` and is extended with a new field, `lldp_neighbor`.

```ocaml
type lldp_rx = {
  system_name: string option;
  port_id: string option;
  port_description: string option;
}
[@@deriving rpcty]

type iface_stats = {
  ...
  lldp_neighbor: lldp_rx option;
}
```
networkd queries `lldpd` for the LLDP TLVs recevied on individual NICs and writes them into `/dev/shm/network_stats`.
Monitor_dbcalls.monitor_dbcall_thread in XAPI reads the in-memory file `/dev/shm/network_stats` periodically, and exposes the data through `PIF_metrics.lldp_neighbor` by storing them in XenAPI map form.

## Scenarios

### Fresh install

During the first host boot, networkd does not enable LLDP on physical interfaces because the default value of `lldp.enabled` in `interface_config_t` is `false`.
After XAPI starts, `network-init` scans physical interfaces (`PIF.scan`), creates PIF objects, and brings them up through `Nm.bring_pif_up`.
During this process, XAPI pushes its built-in configuration to networkd to enable LLDP on individual NICs. networkd may still keep LLDP disabled on some NICs based on the built-in blocking list.

### Update or RPU

After update or RPU from a version/release without LLDP support to a version/release with LLDP support, the default values of the new fields (`pool.lldp_enabled` and `PIF.lldp_mode`) cause networkd to keep LLDP disabled on all NICs.

### Pool join

The `PIF.lldp_mode` of PIFs on the joining host has the default value. The joining host shares the pool-level `pool.lldp_enabled` setting.
These configurations are pushed to networkd via `PIF.scan` during the first-boot `network-init` service on the joining host.

### Pool eject

`pool.lldp_enabled`, `PIF.lldp_mode`, and the networkd database revert to the values used just like after fresh install.
