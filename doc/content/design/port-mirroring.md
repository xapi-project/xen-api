---
title: Port Mirroring
layout: default
design_doc: true
revision: 1
status: proposed
---

## 1. Background

Port mirroring duplicates traffic from a source to a destination so it can be inspected by an analyzer, packet sniffer, or security appliance, without disrupting the original traffic flow.

Three variations exist, depending on where the source and the monitoring device sit:

| Variant                               | Source / Destination location                                                              |
| ------------------------------------- | ------------------------------------------------------------------------------------------ |
| **Local SPAN**                        | Same physical switch                                                                       |
| **RSPAN** (Remote SPAN)               | Different switch, same Layer 2 network, carried over a dedicated RSPAN VLAN                |
| **ERSPAN** (Encapsulated Remote SPAN) | Crosses Layer 3 boundaries; traffic is GRE-encapsulated and routed to a remote destination |

### 1.1 What Open vSwitch (OVS) supports today

Per the `Mirror` table in `ovs-vswitchd.conf.db(5)`:

- Up to 32 mirrors per bridge (we might want to XAPI to enforce it to be independent of the backend/version).
- Flow alterations via OpenFlow rules are visible (OVS ≥ 2.5).
- Configurable snaplen (truncation size of mirrored packets).
- Packet selection by: all packets, destination ports, source ports, VLANs, or an OpenFlow-style filter (OVS ≥ 3.7).
- Destination selection by: a physical/internal/GRE/ERSPAN port (SPAN/ERSPAN), or a VLAN (RSPAN — requires `flood_vlans` configuration on every bridge carrying that VLAN, and can disrupt networks containing unmanaged switches).

### 1.2 Implication for XAPI

XAPI currently drives networking exclusively through OVS, so this is the only backend the design needs to support today.
That said, the object model deliberately targets a **conservative subset** of what OVS exposes — SPAN (local mirroring) and ERSPAN, without VLAN-based selection or RSPAN, rather than the full breadth of OVS's mirroring options (see §3, Non-Goals).
This keeps the feature simple to reason about and leaves room to accommodation.

## 2. Goals

- Allow an administrator to mirror traffic to/from one or more VIFs on a given Network to a remote ERSPAN collector.
- Support both ERSPAN Type II and ERSPAN Type III encapsulation.
- Keep the design pool-aware: mirrored VIFs may live on any host in the pool, and a VM may migrate while being mirrored.
- Provide a `Mirror` object lifecycle (`create` / `enable` / `disable` / `destroy`) and mutable output/target parameters via XenAPI and `xe` CLI.

## 3. Non-Goals (for this iteration at least)

- **"All packets" (whole-network) selection** : not supported, because it conflicts with XAPI's VLAN "fake bridge" design: a single OVS bridge can carry several XAPI Networks (VLANs), making it hard to scope a mirror to just one Network.
- **VLAN-based packet selection** : not implemented for this iteration. Emulating it may be reconsidered later, but it is not a priority.
- **RSPAN (VLAN-based output)** : excluded. It is OVS-only, and it can disrupt networks that contain unmanaged switches.
- **Local mirroring to an existing VIF or to a dom0 interface** : deferred; see §7 "Deferred decisions" below.
- **Filter-based selection (OVS `filter` attribute / OpenFlow-style filter)** : interesting feature but requires recent OVS ≥ 3.4.0.

## 4. Data Model

### 4.1 `Mirror` class

A new persistent class, `Mirror`, one instance per monitored Network/direction pair.

| Field           | Qualifier      | Type                        | Default        | Notes                                                                                                                                 |
| --------------- | -------------- | --------------------------- | -------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `uuid`          | RO             | string                      | -              |                                                                                                                                       |
| `enabled`       | RO (StaticRO)  | bool                        | `false`        | Whether mirroring is currently active                                                                                                 |
| `device`        | RO (DynamicRO) | string                      | computed       | Interface name, e.g. `xapi_mirror0`                                                                                                   |
| `network`       | RO (StaticRO)  | `Ref(Network)`              | constructor    | Network being monitored                                                                                                               |
| `direction`     | RO (StaticRO)  | enum `mirror_direction`     | `from-vifs`    | See below                                                                                                                             |
| `mirrored_vifs` | RW             | `Set(Ref VIF)`              | empty          | VIFs, within `network`, being mirrored                                                                                                |
| `output_format` | RO (StaticRO)  | enum `mirror_output_format` | `erspan-type2` | ERSPAN Type II or III                                                                                                                 |
| `output_ip`     | RW             | string                      | constructor    | ERSPAN collector address                                                                                                              |
| `output_id`     | RW             | int                         | constructor    | ERSPAN monitor id (OVS property `erspan_idx` for v1/Type II, `erspan_hwid` for v2/Type III)                                           |
| `other_config`  | RW             | `Map(String,String)`        | empty          | Escape hatch: `snaplen`, `psk`/`remote_cert`/`remote_name` (IPsec), `key` (tunnel pairing), `ttl`, etc. all not essentials parameters |

`mirror_direction`:

- `from-vifs` : traffic arriving from a VIF (applied to the network's ingress pipeline)
- `to-vifs` : traffic forwarded to a VIF (applied to the network's egress pipeline)

note: if both directions are needed, user is expected to create two separate `Mirror` objects.

`mirror_output_format`:

- `erspan-type2` : ERSPAN Type II
- `erspan-type3` : ERSPAN Type III

### 4.2 `VIF` class addition

- `mirrored_by`: `DynamicRO Set(Ref Mirror)` : back-reference, maintained automatically as the inverse of `Mirror.mirrored_vifs`.

### 4.3 New error

- `VIF_NOT_IN_NETWORK` (`vif`, `network`) — raised when a VIF added to `mirrored_vifs` does not belong to the Mirror's `network`.

## 5. API Surface

### 5.1 Messages on `Mirror`

| Method                 | Params                                                            | Purpose                                           |
| ---------------------- | ----------------------------------------------------------------- | ------------------------------------------------- |
| `create`               | `network`, `direction`, `output_format`, `output_ip`, `output_id` | Create (but not enable) a Mirror; returns its ref |
| `destroy`              | `self`                                                            | Disable (if enabled) and remove the Mirror        |
| `enable`               | `self`                                                            | Activate mirroring on every host in the pool      |
| `disable`              | `self`                                                            | Deactivate mirroring on every host in the pool    |
| `set_output_ip`        | `self`, `value`                                                   | Update the ERSPAN collector address               |
| `set_output_id`        | `self`, `value`                                                   | Update the ERSPAN monitor id                      |
| `set_mirrored_vifs`    | `self`, `value`                                                   | Replace the whole mirrored-VIF set                |
| `add_mirrored_vifs`    | `self`, `value`                                                   | Add one VIF to the mirrored set                   |
| `remove_mirrored_vifs` | `self`, `value`                                                   | Remove one VIF from the mirrored set              |

Possible errors surfaced from `create`: `openvswitch_not_active`, `network_unmanaged`, `invalid_ip_address_specified`, `invalid_value`.

### 5.2 CLI (`xe`)

- `mirror-create network-uuid=… output-ip=… output-id=… [direction=…] [output-format=…]`
- `mirror-destroy uuid=…`
- `mirror-enable uuid=…`
- `mirror-disable uuid=…`
- `mirror-param-*` style field access via the generic record framework (`uuid`,
  `enabled`, `device`, `network`, `direction`, `output-format`, `output-ip`,
  `output-id`, `mirrored-vifs`, `other-config`).
- `vif-param-*` : new read-only `vif-param-get param-name=mirrored-by`.

### 5.3 Networkd

- `add_mirror` : reads existing OVS properties, update-or-create mirror configuration based on commands (set/clear properties)
- `remove_mirror` : explicit mirror cleanup

## 6. Architecture / Call Flow

```txt
xe / XenAPI client
      │
      ▼

Message_forwarding.Mirror  (pool-level fan-out)
      │  - create/set_output_*/set_mirrored_vifs are applied via Local + forwarded
      │  - enable/disable/destroy fan out to ALL live hosts (forward_to_all),
      │    ignoring unreachable ones, since ERSPAN mirrors are configured
      │    per-host (see §6.3)
      ▼

Xapi_mirror  (ocaml/xapi/xapi_mirror.ml — per-host business logic)
      │  - validates (vswitch active, network managed, VIF-in-network, id ranges)
      │  - builds OVS property "commands" (set/clear pairs)
      │  - maintains DB state, guarded by a local mutex
      ▼

Network.Net.Bridge.add_mirror / remove_mirror  (xapi-idl RPC to networkd)
      │
      ▼

Networkd Bridge.add_mirror / remove_mirror  (ocaml/networkd/bin/network_server.ml)
      │  - reads existing OVS properties (idempotent update-or-create)
      │  - merges-in new commands
      ▼

Network_utils.Ovs.Mirror  (ocaml/networkd/lib/network_utils.ml)
      - resolves `xs-vif-uuid` external-ids to OVS Port/Interface uuids for
        select_src_port / select_dst_port
      - translates properties into `ovs-vsctl` transactions:
        add-port / set Interface type=erspan / create Mirror / set Bridge mirrors=…
      - `get_opt` reads back current Mirror+Interface state for idempotent updates
```

### 6.1 Why per-host ERSPAN ports

Because ERSPAN encapsulates over IP, the output port can be created on _every_ host in the pool that might carry a mirrored VIF,
all pointing at the same remote ERSPAN collector.
This sidesteps the locality problem that plain SPAN/RSPAN would create (see §7).

### 6.2 VIF migration handling

`Xapi_mirror.refresh` is called from `Xapi_xenops` whenever a VIF's xenopsd state changes (currently attached / active).
If the VIF belongs to one or more enabled Mirrors,
`refresh` re-applies the mirror configuration on the host,
so a mirror stays correctly wired to a VIF that has just migrated in,
without requiring the admin to intervene.

### 6.3 `enable` / `disable` / `destroy` fan-out

`enable` and `disable` are forwarded to **every** host in the pool, skipping hosts that cannot be contacted,
because the ERSPAN device must exist consistently on each host that could end up running a mirrored VIF.
`destroy` first calls `disable` (pool-wide) and then removes the DB object on the master.

### 6.4 Networkd property model

`add_mirror` is generic and idempotent: it looks up the mirror's current OVS properties (if any),
merges in the caller's `(key, value option)` commands (`Some v` = set, `None` = clear),
and issues one batched `ovs-vsctl` transaction split across the `Interface` table
(ERSPAN options: `remote_ip`, `erspan_dir`, `erspan_ver`, `erspan_idx`/`erspan_hwid`)
and the `Mirror` table (`select_src_port`, `select_dst_port`, `output_port`, `snaplen`).
This allows `set_output_ip`, `set_output_id`, and `set_mirrored_vifs` to each push a
narrow, targeted update without needing to resend the full mirror config.

## 7. Design Decisions & Open Questions

Carried over from initial proof-of-concept work, still to be finalized:

1. **Local mirroring to an existing VIF** (output = another VIF, not ERSPAN)
   - Valid use case, but pins the source VM to the same host as the destination VIF, affecting migration.
   - Breaks if the mirrored VMs later end up on different hosts.
   - Could be emulated via ERSPAN by using the destination VIF's IP as the ERSPAN remote address
     (the collector will see ERSPAN/GRE instead of plain packets),
     possibly avoids implementing it as a first-class output type at all.

2. **Local mirroring to dom0** (output = local interface in dom0)
   - Mainly a debugging use case, requiring local root on the XCP-ng host.
   - Same multi-host limitation as above.
   - Alternative: ship a standalone CLI/debug tool instead of adding this to XAPI's object model.

3. **Generic tunnel-endpoint interface tracking**
   - OVS can create interfaces for tunneling (GRE, ip6gre, VXLAN, Geneve,
     GTP-U, bareudp, SRv6), mirroring (GRE, ip6gre, ERSPAN), patch pairs, and
     internal ports.
   - Two schema approaches were weighed: a dedicated `Mirror` class (chosen
     for this iteration) vs. a generic `Mirror` + `GenericInterface` pairing
     that could be reused for tunnel endpoints. The generic approach was
     judged over-engineered for the current, ERSPAN-focused use case; VXLAN
     tunnel endpoints would be the only other likely consumer, and can be
     revisited separately if/when needed.

4. **ERSPAN direction enforcement**
   - ERSPAN v2 (Type III) carries an explicit ingress/egress marker in the header; ERSPAN v1 (Type II) does not.
   - Best practice is two separate ERSPAN flows (distinct ids) when both ingress and egress need mirroring.
     The design enforces this by making a `Mirror` single-direction (`from-vifs` xor `to-vifs`) rather than allowing "both".

## 8. Compatibility / Versioning Notes

- Schema minor version bumped;
  `Mirror` and its fields/messages, plus `VIF.mirrored_by`, are tagged with next lifecycle version.
- `Mirror` added to `Uuidx` object-kind variants and to the DB-persisted / relations tables (`Mirror.mirrored_vifs` ↔ `VIF.mirrored_by`).
- New networkd IDL calls `Bridge.add_mirror` / `Bridge.remove_mirror` were added alongside existing `Bridge.*` port-management calls.

## 9. Future Work

- Revisit VLAN-based selection emulation if a real need arises.
- Add `mirrored_filter` support in `Mirror` class once the OVS version used reaches ≥ 3.4.0.
- Decide on local-VIF and local-dom0 mirroring (§7, items 1–2), including whether either warrants VM-migration constraints in XAPI.

## 10. References

- OVS `Mirror` table: <https://www.openvswitch.org/support/dist-docs/ovs-vswitchd.conf.db.5.html>
