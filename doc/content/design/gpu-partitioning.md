---
title: GPU partitioning
layout: default
design_doc: true
revision: 1
status: proposed
revision_history:
- revision_number: 1
  description: Initial version
---

## Overview

Some physical GPUs can be divided by their driver into several smaller GPUs, each with
its own slice of compute units and framebuffer, and with the isolation enforced by the
card rather than by software. NVIDIA calls its implementation MIG (Multi-Instance GPU);
AMD has an equivalent with a different shape. This document uses the neutral term
**partition**, as does the API: NVIDIA's partitioning has one axis and AMD's has two
independently settable ones, so a field named after either could not describe the other.

Partitioning is not vGPU: with vGPU several VMs time-share a whole card, whereas a
partitioned card is divided up first and each piece consumed like a smaller card.
The two compose, and that is the configuration described here — a vGPU on a partition.

Xapi models each piece of a divided card as a first-class object, counts those
pieces when deciding whether a VM fits, keeps the division across reboots, and
notices when a card has stopped working. Previously capacity was a whole card's
notional size divided by the requested vGPU type's, so a partitioned card advertised
capacity it did not have and the datapath refused VMs xapi had admitted.

## Datamodel

A partition is a new class, `GPU_partition`, one row per piece of a divided card.
Rows are created from observation, never authored — xapi reads the card's division
and records it — so there is no public constructor; an administrator changes them
by asking the card to be divided differently.

| Mutability | Name | Type | Description |
| ---------: | ---- | ---- | ----------- |
| RO  | uuid            | String         | Unique identifier |
| RO  | PGPU            | Ref PGPU       | The card this is a piece of |
| RO  | profile         | String         | The device-published profile it was created with |
| RO  | vendor_slot_id  | Int            | The vendor's own index for this piece |
| SRO | resident_VGPUs  | Set (Ref VGPU) | VGPUs currently on this partition |
| SRO | scheduled_VGPUs | Set (Ref VGPU) | VGPUs reserved but not yet running |

`vendor_slot_id` is a small integer the vendor's own CLI prints, and is public because
it is the join key between the two: given a partition in `xe`, an administrator can find
the same piece in `nvidia-smi`. It is never hashed into a synthetic uuid, and is not
stable across repartitioning, so a client should not record it as a durable name.

`VGPU` gains two references, mirroring the `resident_on` and
`scheduled_to_be_resident_on` pair it already carries for cards. There are two
because a reservation and an occupancy are different facts, and the allocator sums
both. It also records the card's layout generation as it stood when the binding was
made, so that the binding can be checked against the card it names.

| Mutability | Name | Type | Description |
| ---------: | ---- | ---- | ----------- |
| RO | resident_on_partition                 | Ref GPU_partition | The partition this VGPU runs on |
| RO | scheduled_to_be_resident_on_partition | Ref GPU_partition | The partition reserved for it |
| RO | partition_layout_generation           | Int               | The card's generation when this VGPU was bound |

`PGPU` gains the card's own capability and geometry; per-piece identity lives on
the partition, and pool-wide interchangeability remains `GPU_group`'s.

| Mutability | Name | Type | Description |
| ---------: | ---- | ---- | ----------- |
| SRO | partitions                   | Set (Ref GPU_partition) | The pieces this card is divided into |
| DRO | partition_mode               | enum partition_mode     | Whether and how the card is divided |
| SRO | supported_partition_profiles | Set String              | Profiles the card could be divided into |
| SRO | remaining_partition_profiles | Set String              | Profiles that still fit in what is left |
| RO  | partition_layout_generation  | Int                     | Bumped whenever the division changes |
| DRO | requires_reset               | Bool                    | The card is wedged and needs a reset |

`partition_mode` is derived, on the precedent of `PGPU.dom0_access`, which carries
current state and a pending reboot change in one field so the two cannot contradict.
The profile lists are stored and refreshed by discovery, since their source is the
driver; the remaining list is not derivable from the supported one, because creating
one large partition constrains what fits beside it. `partition_layout_generation` is
a staleness witness: repartitioning replaces every piece of a card at once, so a
per-card counter is what makes a stale partition reference detectable.

Three relations join the new class to the existing ones; in each case xapi maintains
the second element from the reference on the first, so nothing writes the set side:

```ocaml
((_gpu_partition, "PGPU"), (_pgpu, "partitions"))
((_vgpu, "resident_on_partition"), (_gpu_partition, "resident_VGPUs"))
((_vgpu, "scheduled_to_be_resident_on_partition"), (_gpu_partition, "scheduled_VGPUs"))
```

The `partition_mode` enum has six values. `disabled` and `not_supported` stay distinct
because "capable but off" and "cannot" are different answers, which is why this is an
enum and not a boolean. `unknown` is permanent rather than transitional: the probe has
a cannot-read outcome, and such a card needs a value to report for as long as it lasts.

| Value | Meaning |
| ----- | ------- |
| `not_supported`     | The card cannot be divided |
| `disabled`          | Capable, not divided |
| `enable_on_reboot`  | Division requested, takes effect at next reboot |
| `enabled`           | The card is divided |
| `disable_on_reboot` | Undivision requested, takes effect at next reboot |
| `unknown`           | The card's division could not be read |

Partitioning is driven by messages rather than a settable field, so RBAC roles have
something to attach to: `PGPU.enable_partitioning` and `PGPU.disable_partitioning`
mirror the existing `dom0_access` pair, alongside messages to create and destroy
individual partitions and to refresh a card's geometry. Free capacity on a partition
is likewise a message, like `PGPU.get_remaining_capacity`: the value is derived, and a
stored copy would be a second version of a truth the allocator already holds.

## Discovering the geometry

We read an NVIDIA card's division by using NVML (NVIDIA Management Library) in
`xcp-rrdd-gpumon` and import the data into Xapi with the gpumon IDL. The possible responses we can receive are:
- Card is divided, together with the pieces it is divided into
- The card is capable but not divided
- The division could not be read, together with an error message (e.g. busy driver)
- The card has no partition axis at all

Geometry changes for two reasons: an operator asked, or the host rebooted. Neither
is frequent, so the recorded division is invalidated by xapi's own write path as it
happens, backed by a long-period poll that catches changes made outside XenServer,
and an administrator who cannot wait can ask for a refresh. Geometry is therefore
fresh immediately after any change XenServer made, and otherwise correct within the
backstop interval.

A division does not survive a host reboot on current hardware, so xapi re-applies it
during startup from what it recorded. On a host partitioned by hand before it was upgraded
there is no recorded intent and the division would otherwise be lost; instead xapi
adopts what it finds, marks it inferred, and never destroys a division it has no
record of asking for.

## Placing a VM

When a VM with a partition-backed vGPU starts, the allocator picks a specific piece of
a specific card and records it on the VGPU, alongside the card-level reference it
already sets. The reservation is written first and becomes an occupancy when the VM
runs, which is why both references exist.

Suspend keeps the partition. This is deliberately asymmetric: a VM entering a suspended
or halted state releases its *card* slot as it always has, and card-level accounting is
unchanged, but the partition binding persists, so a resumed VM returns to the same piece
of the same card. Clone, copy and snapshot-revert clear the references: a copy has been
allocated nothing, and a reverted snapshot's partition may no longer exist.

A binding is honoured only if it passes three tests: the reference is still valid,
the partition is on this host, and `VGPU.partition_layout_generation` still matches
the `PGPU`'s. Otherwise it is treated as absent, and the database garbage collector
clears the reference. The generation is the test that does the real work here.
Partition rows are matched by card, profile and ordinal rather than destroyed and
recreated, so a row can outlive the repartitioning that invalidated it: the
reference stays valid and the host is unchanged, but the piece of silicon behind it
is a different one. Comparing generations is what distinguishes a binding made
before the card was repartitioned from one made after. Migrating a VM moves the
booking to the destination and releases the piece held on the source.

## Capacity and refusals

Remaining capacity for a partition-backed vGPU type is the number of free
partitions that can host it, computed from the card's division and its occupancy.
The two stay separate: the division is fixed until the card is repartitioned, while
occupancy moves as VMs start and stop. Slot cost is not uniform — the same profile
can cost one slot on one card and three on another — so the arithmetic is per-SKU.
Where the geometry is unknown, capacity is zero and the start is refused.

A refusal carries its reason to the caller. The set of reasons a GPU start can fail is
closed and each maps to a registered API error, so a refusal cannot degrade into a
generic failure; when several hosts decline, the most informative reason is reported.

## Health and recovery

A wedged card is detected from the geometry report alone, by comparing the profiles
that should be available against what the card admits to, and the verdict is
surfaced as `PGPU.requires_reset`. Admission consults it, so a wedged card stops
receiving new VMs. The verdict is recomputed when used rather than cached: a stale
"wedged" turns a transient fault into permanently lost capacity, and a stale
"healthy" makes every placement on that card fail.

Recovery is automatic, bounded, gated on the card being idle, and visible to the
operator while it runs: it stops the monitoring daemons, resets the card, and
re-applies the recorded division. When the last VM using a tenant's partitions goes
away those partitions are released and the card returns to a clean state, proved by
comparing a fingerprint of its actual state before and after rather than by
observing that the next VM started.

## Metrics, audit and pools

Each partition has RRD datasources on its host, named from the card's PCI address
and the partition's ordinal, so the names a host can emit stay few and repartitioning
does not accumulate dead datasources. Partition operations are recorded as tracing
spans, with the subject captured before the VM lifecycle clears its references, so
a VM-stop record still names what it was using.

Evacuation accounts for partitions as it plans, so a host that cannot be evacuated for
want of GPU capacity says so up front rather than failing as each VM starts. HA planning
is aware of partition-backed VMs, and is conservative about a card whose division cannot
be guaranteed after a restart.

## The CLI

`GPU_partition` is registered with `gen_cmds`, so it gets the standard generated
family — `gpu-partition-list`, `gpu-partition-param-list`, `gpu-partition-param-get`
and the rest. Everything on the class is read-only, so the `param-set`, `-add`,
`-remove` and `-clear` forms exist but reject every field; the card is changed
through the messages below, not by setting a field on a partition.

The default columns for `gpu-partition-list` are `uuid`, `pgpu-uuid`, `profile` and
`vendor-slot-id` — enough to identify a piece and match it against the vendor tool
without passing `params=all`. The two VGPU sets are available but off by default,
since they are usually read from the other end.

| Command | Description |
| ------- | ----------- |
| `xe gpu-partition-list [pgpu-uuid=<uuid>]`   | The partitions on a card, or all of them |
| `xe pgpu-enable-partitioning uuid=<uuid>`    | Divide the card |
| `xe pgpu-disable-partitioning uuid=<uuid>`   | Return the card to undivided |
| `xe pgpu-create-partition uuid=<uuid> profile=<p>` | Create one partition |
| `xe pgpu-destroy-partition uuid=<uuid>`      | Release one piece |
| `xe pgpu-refresh-geometry uuid=<uuid>`       | Re-read the division from the card now |
| `xe pgpu-get-remaining-capacity uuid=<uuid> vgpu-type-uuid=<uuid>` | The free partitions of that type |

`xe pgpu-list` gains `partition-mode`, `supported-partition-profiles`,
`remaining-partition-profiles`, `partition-layout-generation`, `requires-reset` and
`partition-uuids`. Only `partition-mode` and `requires-reset` join the default
columns; the profile lists can be long, and the generation counter is for
diagnosis rather than routine reading. `xe vgpu-list` gains
`resident-on-partition-uuid`, `scheduled-to-be-resident-on-partition-uuid` and
`partition-layout-generation`, the last for diagnosing a binding that has gone stale.

The states are rendered so they cannot be confused with each other. A card with no
pieces lists nothing and reports `partition-mode: disabled`; a card whose division
could not be read reports `partition-mode: unknown` and lists whatever rows were
last known, each marked stale, rather than showing an empty list that would read as
"this card has no partitions". Since the commands that change a division take
effect at the next reboot on current hardware, `enable_on_reboot` and
`disable_on_reboot` are visible in the same field, so an administrator can see a
pending change without consulting anything else.

Because repartitioning renumbers the pieces, `vendor-slot-id` is displayed but scripts
should key on the partition's `uuid`, which is stable for as long as the piece
exists.
