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

Some GPUs can be split into several smaller GPUs, each with its own compute
units and framebuffer, isolated by the hardware. NVIDIA calls this MIG
(Multi-Instance GPU); AMD has a similar feature. This document and the API call
each piece a partition.

This is not the same as vGPU, where several VMs time-share a whole card. The two
can be combined, and this design covers running a vGPU on a partition.

Currently xapi computes a card's capacity for a vGPU type as the card's size
divided by the type's size, without looking at how the card is partitioned. On
a partitioned card the result can be higher than the free partitions allow, so
xapi admits VMs that then fail to start. With this design xapi:

- models each partition as an object
- counts free partitions when deciding whether a VM fits
- lets an administrator partition a card through the API and `xe`
- re-applies the partitioning after a host reboot
- detects when a card needs a reset

Cards that aren't partitioned keep today's behaviour, including on hosts that
have both kinds of card.

## Datamodel

### GPU_partition

A new class, with one object per partition. xapi creates and destroys these
objects when it reads a card's partitioning, so there is no public constructor.
An administrator changes them by repartitioning the card.

A partition is not modelled as a `PGPU`. A `PGPU` is one physical card, and its
fields (`PCI`, `GPU_group`, supported and enabled vGPU types, dom0 access)
describe the card, not a piece of it.

| Mutability | Name | Type | Description |
| ---------: | ---- | ---- | ----------- |
| RO  | uuid            | String         | Unique identifier |
| RO  | PGPU            | Ref PGPU       | The card this partition is on |
| RO  | profile         | String         | The vendor profile the partition was created with |
| RO  | vendor_slot_id  | Int            | The vendor's index for this partition |
| SRO | resident_VGPUs  | Set (Ref VGPU) | VGPUs running on this partition |
| SRO | scheduled_VGPUs | Set (Ref VGPU) | VGPUs reserved on this partition but not yet running |

`vendor_slot_id` is the index printed by the vendor's tools, e.g. `nvidia-smi`,
so an administrator can match a partition in `xe` to the vendor's output. It
can change when the card is repartitioned, so clients should not store it as a
long-lived name.

Each row also records, internally, the layout generation at which xapi last
confirmed it. A row is stale when that differs from the card's
`partition_layout_generation`.

### VGPU

Two new fields, matching the existing `resident_on` and
`scheduled_to_be_resident_on`:

| Mutability | Name | Type | Description |
| ---------: | ---- | ---- | ----------- |
| RO | resident_on_partition                 | Ref GPU_partition | The partition this VGPU runs on |
| RO | scheduled_to_be_resident_on_partition | Ref GPU_partition | The partition reserved for it |

### PGPU

| Mutability | Name | Type | Description |
| ---------: | ---- | ---- | ----------- |
| SRO | partitions                   | Set (Ref GPU_partition) | The card's partitions |
| DRO | partition_mode               | enum partition_mode     | Whether the card is partitioned |
| SRO | supported_partition_profiles | Set String              | Profiles the card supports |
| SRO | remaining_partition_profiles | Set String              | Profiles that fit in the unpartitioned space |
| RO  | partition_layout_generation  | Int                     | Incremented whenever the card's partitioning changes |

- `partition_mode` is computed, not stored. Like the `pci_dom0_access` enum
  returned by `PCI.get_dom0_access_status`, it shows both the current state and
  any change pending a reboot.
- The two profile lists are read from the driver and refreshed with the
  partitioning. `remaining_partition_profiles` can't be computed from
  `supported_partition_profiles`, because a large partition limits what fits
  next to it.

`PGPU` also needs a field saying whether the card is wedged (see Health and
recovery). Its name and type are not decided yet. The check has three outcomes
(healthy, wedged, unknown), so it will be an enum rather than a Bool.

The new relations are below. In each case xapi maintains the set from the
reference.

```ocaml
((_gpu_partition, "PGPU"), (_pgpu, "partitions"))
((_vgpu, "resident_on_partition"), (_gpu_partition, "resident_VGPUs"))
((_vgpu, "scheduled_to_be_resident_on_partition"), (_gpu_partition, "scheduled_VGPUs"))
```

### partition_mode

| Value | Meaning |
| ----- | ------- |
| `not_supported`     | The card can't be partitioned |
| `unknown`           | The card's partitioning couldn't be read |
| `disabled`          | The card can be partitioned, but isn't |
| `enable_on_reboot`  | Partitioning will be enabled at the next reboot |
| `enabled`           | The card is partitioned |
| `disable_on_reboot` | Partitioning will be disabled at the next reboot |

The first two describe what xapi knows about the card's capability; the other
four describe its partitioning. They are one enum so that a client reads a
single field, as with `pci_dom0_access`. A card stays `unknown` for as long as
its partitioning can't be read, not only during startup.

On NVIDIA cards, enabling or disabling partitioning takes effect immediately.
The `_on_reboot` values are for vendors whose change only takes effect after a
reboot.

### Messages

- `PGPU.enable_partitioning` and `PGPU.disable_partitioning`
- create and destroy a single partition
- refresh a card's partitioning
- get a partition's free capacity, like `PGPU.get_remaining_capacity`
- reset a wedged card

Only the first two names are decided. Enabling, disabling and refreshing are
`_R_POOL_OP`, like the other `PGPU` messages.

Partitioning is changed through messages rather than by setting a field
because each change runs a hardware sequence on the card and can be refused,
for example when a partition is in use.

The messages are on `PGPU`, not `PCI`. The dom0 access messages moved from
`PGPU` to `PCI` because hiding a device from dom0 applies to any PCI device.
Partitioning applies only to GPUs, and the partitions belong to the `PGPU`.

Free capacity is computed when asked for, not stored.

The messages that change partitioning ship behind a feature flag, off by
default, until capacity accounting counts partitions.

## Reading and changing the partitioning

xapi reads a card's partitioning from NVML, through `xcp-rrdd-gpumon` and the
gpumon IDL. The result is one of:

- partitioned, with the list of partitions
- can be partitioned, but isn't
- couldn't be read, with a reason
- can't be partitioned

A failed read must not be treated as "not partitioned". Otherwise xapi would
delete every partition object on a card whose driver was only busy. Instead xapi
keeps the existing rows and marks them stale.

xapi matches what it reads against its rows by card, profile and placement (the
partition's start offset on the card). Rows that still match are kept, new
partitions get new rows, and rows with no match are destroyed. So reading the
same layout again, for example after a reboot, keeps the same rows.

Changes are made by xapi itself, through the messages above. It stops
`xcp-rrdd-gpumon` and `nvidia-vgpud`, runs the vendor tools, and restarts them,
holding a per-card lock throughout. Enabling partitioning resets the card.
Destroying a partition destroys what runs on it, so xapi refuses to destroy an
occupied partition and names the VMs using it. Disabling partitioning destroys
every partition on the card.

xapi re-reads a card's partitioning:

- after xapi changes it
- on a long-period poll, to catch changes made outside xapi
- when an administrator asks for a refresh

So the partitioning is up to date immediately after a change made through xapi,
and otherwise within the poll interval. The poll can't run while vGPU VMs are
starting, because gpumon is stopped then, so that time adds to the interval. If
a requested refresh can't read the card, it fails with the reason.

On current NVIDIA hardware, neither the partitions nor partitioning mode survive
a host reboot. xapi records the partitioning it applied and re-applies it after
a reboot. It detects a reboot from the host's boot time, so a toolstack restart
doesn't trigger it. Re-applying runs in the background and doesn't stop the
host from becoming ready.

A host partitioned by hand before upgrading has no record. xapi adopts what it
finds, creates rows for it, records it as its own, and raises a host message.
Adoption is on by default and can be switched off. If xapi can't represent the
layout, it adopts nothing, raises an alert and leaves the card unmanaged. xapi
never removes partitioning that it didn't create.

## Starting a VM

Allocation works as it does now: a VGPU is bound to a `GPU_group` and a
`VGPU_type`, and xapi picks a card in the group. There is no group class for
partitions. The group and type already identify "a free partition that can host
this type, anywhere in the group".

On the chosen card the vGPU is placed on a partition. With current NVIDIA
drivers the driver picks the partition and xapi reads the choice back. xapi can
only pick the partition itself if the vendor provides a way to do so. Where it
can, it picks the most occupied partition that still has room, to keep empty
partitions free for types that need a whole one.

xapi sets `scheduled_to_be_resident_on_partition` alongside the existing PGPU
reference. Once the VM is running, this becomes `resident_on_partition`.

- Suspend and shutdown release the PGPU slot as before, so card-level accounting
  is unchanged. Suspend keeps `resident_on_partition`, so a resumed VM goes back
  to the same partition. Shutdown clears it. A checkpoint clears nothing.
- Clone and copy clear the partition references.
- Snapshot revert places the VM again, and fails if there is no room. Revert can
  run on a running VM, so it can't leave the VM without a partition.
- Migration reserves a partition on the destination, then releases the one on
  the source. If the migration is aborted, the destination reservation is
  released last.

A partition reference is only valid if the partition still exists, is on this
host, and isn't stale. When it isn't valid:

- placing a VM ignores it and chooses again
- resuming a VM fails with an error saying why
- capacity doesn't count it as occupied

If a resuming VM's partition is valid but another VM is using it, the resume
fails with an error naming the partition and that VM. The database garbage
collector clears invalid references without destroying the VGPU.

## Capacity and errors

Remaining capacity for a partition-backed vGPU type is the number of free vGPU
slots on partitions that can host it. A partition can hold more than one vGPU,
and its first vGPU fixes the type for the rest. The number of slots a profile
uses differs between card models, so this is calculated per model.

If a card's partitioning can't be read, its capacity is unknown, not zero. Zero
would mean "no room" and refuse every VM because of a monitoring failure.

Each reason a GPU start can fail has its own API error, so the caller never
gets a generic failure. When several cards refuse a VM, the error from the card
that came closest to accepting it is reported. A vGPU type conflict ranks above
insufficient capacity, the reverse of the current order in
`xapi_gpu_group.ml`. The same rule applies wherever refusals are combined.

## Health and recovery

xapi decides a card is wedged by checking each partition it intends the card to
have but which doesn't exist. If that partition's profile isn't among the ones
the card says it can still create, the card is wedged. A fully partitioned card
is healthy. If the card's report can't be read, the result is unknown, not
wedged.

The check runs when a VM is admitted, with a cache of a few seconds that is
cleared by anything that could change the result. A stale "wedged" result
would permanently remove capacity, and a stale "healthy" result would make
every placement on the card fail. New VMs are not placed on a wedged card, and
evacuation doesn't pick it as a destination. Unknown neither refuses nor admits
on health grounds.

Recovery is an operator-visible operation on the card. It ships behind a
feature flag, off by default.

- It is refused while any VGPU is resident on or scheduled to the card. The
  error lists the VMs. Recovery never shuts down or moves VMs itself.
- It makes one attempt. If that fails, xapi stops, marks the card unusable for
  new VMs, and raises an alert naming the card.
- It stops `xcp-rrdd-gpumon` and `nvidia-vgpud`, resets the card, re-applies the
  partitioning xapi intends the card to have, and restarts the toolstack.

Both daemons are stopped for the whole host. It isn't known yet whether one card
can be recovered without disturbing the others on a host with several cards.

When a VM hands back a partition and nothing else owns it, xapi releases the
partition before the card is offered to another VM. A periodic pass catches
releases interrupted by a crash. If a release fails, the card is marked
unhealthy, not the VM. A card leaves partitioned mode only if xapi's recorded
partitioning says it should.

## Metrics, tracing and pools

Each partition has RRD datasources on its host, named by the card's PCI address
and the partition's placement. A rebuild that recreates the same layout keeps
the same names. When a partition goes away, xapi retires its datasources
explicitly, which also deletes their history. The datasources report partition
count and occupancy. NVML doesn't report per-partition utilisation on current
drivers.

Partition operations are recorded as tracing spans, including refused and failed
ones. A span carries the partition's uuid and its `vendor_slot_id` at that
moment. The partition is captured before VM shutdown clears the references, so
the span for a VM stop still shows which partition it used. Spans go to a
rotating debug log, so they are not an audit trail.

Host evacuation takes partitions into account, so a host that can't be
evacuated for lack of GPU capacity is reported before any VM is moved. VMs that
can't be placed are reported, not moved elsewhere.

HA plans against the partitioning a host is meant to have. If re-applying it
after a reboot fails, the host reports reduced capacity. A VM that HA can't
protect gets a different error from one that can't start for lack of capacity.

## CLI

`GPU_partition` is registered with `gen_cmds`, so it gets the standard generated
commands: `gpu-partition-list`, `gpu-partition-param-list`,
`gpu-partition-param-get` and the rest. Every field is read-only, so the
`param-set`, `-add`, `-remove` and `-clear` forms reject every field.

The default columns for `gpu-partition-list` are `uuid`, `pgpu-uuid`, `profile`
and `vendor-slot-id`, enough to identify a partition and match it against the
vendor tool. The two VGPU sets are available but not shown by default. Sets are
shown as uuids that can be passed to another command.

| Command | Description |
| ------- | ----------- |
| `xe gpu-partition-list [pgpu-uuid=<uuid>]` | The partitions on a card, or all of them |
| `xe pgpu-enable-partitioning uuid=<uuid>`  | Enable partitioning on the card |
| `xe pgpu-disable-partitioning uuid=<uuid>` | Disable partitioning, destroying every partition on the card |

Commands to create and destroy a single partition, refresh a card and reset a
wedged card will follow the messages, once those are named. Destroying a
partition destroys only that partition, and is refused while it is in use.

`xe pgpu-list` gains `partition-mode`, `supported-partition-profiles`,
`remaining-partition-profiles`, `partition-layout-generation` and
`partition-uuids`. Only `partition-mode` is shown by default. `xe vgpu-list`
gains `resident-on-partition-uuid` and
`scheduled-to-be-resident-on-partition-uuid`.

These cases are shown differently, so that a script can tell them apart:

| Case | `partition-mode` | Partitions listed |
| ---- | ---------------- | ----------------- |
| Partitioned          | `enabled`       | The partitions |
| Enabled, none created | `enabled`      | None |
| Can't be read        | `unknown`       | The last known partitions, marked stale |
| Can't be partitioned | `not_supported` | None |

The documentation must say that "can't be read" doesn't mean the partitions are
gone.

`vendor-slot-id` is displayed, but scripts should use the partition's `uuid`.
