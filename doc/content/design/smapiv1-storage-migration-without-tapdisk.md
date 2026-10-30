---
title: SMAPIv1 storage migration without tapdisk
layout: default
design_doc: true
revision: 1
status: draft
revision_history:
- revision_number: 1
  description: Initial draft
---

## Overview

Outbound storage migration (SXM) from an SMAPIv1 SR is built around tapdisk
and the VHD format. Xapi finds the tapdisk serving a VDI with `tap-ctl`, hands
it a connected socket to mirror into, turns the mirror on and off through
`VDI.snapshot` with a magic `sm_config` key, polls `tap-ctl stats` for
failures and in-flight I/O, reads VHD block allocation tables behind tapdisk's
back to copy the base, and finally re-parents the mirrored leaf onto the
copied base with `VDI.compose`.

None of that applies to a VDI served by qemu-dp, which is how XenServer is
setting up its SMAPIv1 LVM and file SRs to serve qcow2 VDIs. There is no
tapdisk to find, no VHD to read and no VHD parent to compose onto.

SMAPIv3 SRs hit the same wall earlier and solved it differently: the storage
backend runs the mirror itself. Xapi only provides the transport and asks the
backend to start and report on a mirror job (`Data.mirror`, `Data.stat`). The
snapshot-aware extension of that path on `feature/sxm-v3`
([SMAPIv3 snapshot-aware storage migration](../xapi/storage/sxm/sxm-v3-with-snapshot.md))
reproduces a VM's whole snapshot tree using nothing more than those two calls
plus ordinary VDI operations on the destination.

This document names xapi's two migration implementations by who runs the
mirror, not by SMAPI version: the **tapdisk mirror** (today
`Storage_smapiv1_migrate`) and the **backend mirror** (today
`Storage_smapiv3_migrate`). A VDI on an SMAPIv1 SR that takes the backend
mirror is still an SMAPIv1 VDI: xapi coordinates its migration exactly as it
does an SMAPIv3 VDI's, and reaches the backend through new SMAPIv1 calls.

This design lets an SMAPIv1 SR use the backend mirror, VDI by VDI:

- Add SMAPIv1 operations `vdi_mirror`, `vdi_mirror_stat` and
  `vdi_mirror_cancel`, shaped after SMAPIv3's `Data.mirror`, `Data.stat` and
  `Data.cancel`; `vdi_import_activate`, shaped after SMAPIv3's
  `Datapath.import_activate`; and `vdi_mirror_supported`, which says whether
  the backend mirrors a given VDI itself. They are gated by a new, published
  SR capability `VDI_MIRROR_NBD`. An SM declares it to opt in; an SM built
  from the same toolstack source that does not declare it is unaffected.
- Implement the existing SMAPIv2 `DATA.mirror`, `DATA.stat` and
  `DATA.import_activate`, and a new `DATA.mirror_supported`, for SMAPIv1 SRs
  on top of them; and add a SMAPIv2 `DATA.cancel` for both SMAPIv1 and
  SMAPIv3 SRs, which today have no way to stop a backend mirror.
- Choose the migration implementation per VDI rather than per SR, before
  anything is created on the destination: a VDI whose SR declares
  `VDI_MIRROR_NBD` and whose backend answers that it mirrors it takes the
  backend mirror; anything else, including VHD VDIs on the same SR, keeps the
  tapdisk mirror.
- Make the destination side independent of tapdisk, so that VDIs the backend
  serves itself can receive either kind of mirror.

The first implementation is XenServer's SM, for the qcow2 VDIs it serves
through qemu-dp; it is maintained outside the `xapi-project/sm` tree. VHD
VDIs, and any SM that does not declare the new capability (including one that
serves qcow2 through tapdisk), keep the current behaviour unchanged.

## Current behaviour

### SMAPIv1

The steps below are for a live mirror. The receive-side logic runs on the
source host and drives the destination over SMAPIv2.

1. Destination: create the leaf VDI, snapshot it to obtain a dummy VDI (so the
   leaf "becomes a differencing disk"), attach and activate the leaf, and
   create or clone a parent VDI from the most similar destination VDI.
2. Source: attach the VDI to a mirror datapath, find its tapdisk from the
   attach result (`tap-ctl`, or the `nbd<pid>.<minor>` socket name), and pass
   an HTTP connection to the destination to tapdisk's `nbdclient<pid>` socket
   with `SCM_RIGHTS`. On the destination that connection is handed to the
   leaf's tapdisk `nbdserver<pid>.<minor>` socket the same way.
3. Source: `VDI.snapshot` with `sm_config` `mirror=nbd:<dp>`. SM turns this
   into `tap-ctl unpause -2 nbd:<dp>`, so every write after the snapshot is
   mirrored, and blocks leaf-coalesce on the VDI.
4. Source: a watchdog polls `tap-ctl stats` for `nbd_mirror_failed`.
5. Source: copy the snapshot into the destination parent with `sparse_dd`,
   which finds the VHD file behind tapdisk and reads the VHD block allocation
   tables of the chain (and of the similar base, for a delta).
6. Source: at deactivate, poll `tap-ctl stats` until `reqs_outstanding = 0`.
7. Destination: `VDI.compose parent leaf`, destroy the dummy.

Stopping or failing a mirror uses another `VDI.snapshot` with `mirror=null`.
The tapdisk device is persisted in xapi's mirror state so it can still be
polled after a xapi restart.

| Tapdisk or VHD dependency | Where | What replaces it |
|---|---|---|
| Find the tapdisk serving a datapath | `tapdisk_of_attach_info` | The backend finds its own datapath |
| Pass the mirror socket to tapdisk with `SCM_RIGHTS` | `mirror_pass_fds` | `vdi_mirror` is given an NBD URI to connect to |
| `VDI.snapshot` with `mirror=nbd:<dp>` / `mirror=null` | `mirror_snapshot`, `stop_internal` | `vdi_mirror` / `vdi_mirror_cancel` |
| `tap-ctl stats`: `nbd_mirror_failed`, `reqs_outstanding` | `mirror_checker`, `pre_deactivate_hook` | `vdi_mirror_stat`; deactivate finishes the job |
| Destination leaf made a VHD child by a dummy snapshot | `receive_start_common` | Not needed when the backend mirrors, which copies all data; unchanged when it receives a tapdisk mirror |
| `VDI.compose` of leaf onto copied base | `receive_finalize_common` | Not needed when the backend mirrors; `vdi_compose` when it receives a tapdisk mirror (see [Requirements on an implementing SM](#requirements-on-an-implementing-sm)) |
| `sparse_dd` reading the VHD BAT behind tapdisk | `Copy.copy_into_vdi` | The backend's mirror job copies the data |
| `similar_content` walking `vhd-parent` | `storage_smapiv1.ml` | Snapshot tree reproduced as in SMAPIv3 |
| Tapdisk NBD server socket names on the destination | `get_nbd_server_common` | NBD URI from the attach result; `vdi_import_activate` |
| `Tapctl.tapdev` persisted in mirror send state | `storage_migrate_helper.ml` | Opaque backend operation id |

### SMAPIv3

Xapi creates the destination leaf, attaches and activates it, and obtains its
NBD export from the attach result. On the source it runs an NBD proxy that
listens on a local Unix socket and forwards each connection over HTTP(S) to the
destination host, where it is joined to the destination datapath's NBD server.
It then calls `Data.mirror(uri, domain, remote)` with an `nbd+unix` URI for
that proxy socket, and polls `Data.stat` until the job reports the destination
in sync. The job copies the existing content and forwards new writes. On
`feature/sxm-v3`, read-only snapshot VDIs are copied with the same call
(attached read-only, mirrored once into a clone of the destination copy of
their parent, then snapshotted on the destination), which reproduces the
snapshot tree without any knowledge of the backend's internal chain.

## Proposal

### Principles

- The backend that serves a VDI is the only component that knows how to read
  it and how to forward its writes. Xapi provides the transport and the
  orchestration, not the data path.
- The SMAPIv1 operations mirror the SMAPIv3 `Data` interface so that both SMAPI
  versions share one migration implementation in xapi.
- Xapi decides which path a VDI takes by asking the source backend, before it
  creates anything on the destination. It never discovers the answer by
  trying, and it never interprets the backend's metadata to work it out: what
  a backend can mirror is the backend's policy.

The same principle applies to VDI export and import, which are outside this
design: for a VDI the backend serves over NBD, xapi should learn which extents
hold data from the datapath (`NBD_CMD_BLOCK_STATUS` with `base:allocation`,
and `qemu:allocation-depth` for a delta), not by parsing the image file.

### Capability

`VDI_MIRROR_NBD` is a new SR capability, published with the other SM
capabilities in `smint` so that any SM can declare it. Declaring it means the
SR's backend implements the operations below: `vdi_mirror_supported` for any
of its VDIs, and, for the VDIs it mirrors itself, `vdi_mirror`,
`vdi_mirror_stat` and `vdi_mirror_cancel` as a source and
`vdi_import_activate` and `vdi_compose` as a destination of the tapdisk mirror.

The capability says only that the operations exist; which VDIs they cover is
the backend's answer to `vdi_mirror_supported`. So the capability needs no
version, no list of formats and no per-VDI metadata for xapi to interpret,
and supporting a further format is a change to the backend alone. It is still
needed: an SM answers an operation it does not know with an error that xapi
cannot tell apart from a failure, so xapi must not ask an SM that has not
declared it. Its absence means xapi never calls the operations. Two products
built from the same toolstack source can therefore differ: one declares the
capability and gets the new path, the other does not and keeps the existing
one.

The capability is a statement about the backend, not about any format: an SM
that serves qcow2 through tapdisk, and relies on the existing tapdisk mirror,
simply does not declare it.

The snapshot copy activates each snapshot read-only. That needs nothing new
from an SMAPIv1 SR: SMAPIv2 `VDI.activate_readonly` is `activate3` for
SMAPIv1, and the backend sees the read-only attach as the `rdonly` option to
`vdi_activate`, as it does today.

### SMAPIv1 operations

All five are VDI operations and are not master-only. `vdi_mirror_supported`
runs on any host where the SR is attached; the others run on the host where
the VDI is attached. The usual `sr_ref`, `sr_uuid`, `vdi_ref`, `vdi_uuid` and
`device_config` fields are passed as for other VDI calls.

#### `vdi_mirror_supported`

- `args`: `[]`
- Result: a boolean, true if the backend mirrors this VDI itself.
- Preconditions: none; the VDI need not be attached.
- Semantics: a property of the VDI, not of its current state. The answer for
  a VDI does not change while it exists, and is the same for every VDI in one
  disk's snapshot tree. Conditions that pass, such as a merge in progress, do
  not make it false: `vdi_mirror` defers through those (see below). A backend
  answers from its own knowledge of the VDI, typically its image format.
- Errors: an SR backend error only if the VDI cannot be found or read.

#### `vdi_mirror`

- `args`: `[remote]`, an NBD URI of the form
  `nbd+unix:///<export>?socket=<path>`. The backend connects to it as an NBD
  client.
- Result: a string, the operation id.
- Preconditions: the VDI is attached and activated on this host, read-write
  (live mirror) or read-only (one-shot copy of a snapshot).
- Semantics, matching the SMAPIv3 `Data.mirror` implementation in
  `xapi-storage-plugins`:
  - The job copies **one level**: the data the VDI holds over its parent in
    xapi's snapshot tree. The caller guarantees the target already holds the
    content of that parent: a blank VDI if there is none, otherwise the
    destination copy of the parent snapshot.
    - For an active VDI, that is its own writable layer.
    - For a snapshot VDI, it is the frozen node the snapshot was taken from,
      which is where a snapshot's data lives when the snapshot itself is an
      empty leaf. (`xapi-storage-plugins` mirrors the snapshot's parent volume
      with `sync: top` for exactly this reason.)
  - A range the level holds as zeroes is part of the level and must reach the
    target as zeroes, not be skipped as unallocated. The case that matters is
    a guest discard in an overlay over data its parent holds: a qcow2 overlay
    records it as a zero cluster precisely so the parent's data does not show
    through, and skipping it would let the destination read the copied
    parent's stale data where the source reads zeroes.
  - If the backend's chain has more than one layer between the VDI and that
    parent, for example a node left by a deleted snapshot that background
    garbage collection has not yet merged, the job must not copy a partial
    level. It either waits until the chain is collapsed, reporting
    `complete = false` meanwhile, or copies every layer down to the parent.
    The SMAPIv3 plugins defer the job until garbage collection has finished.
  - The call returns as soon as the job is started or deferred.
  - If the VDI is active read-write, once the level is copied the job keeps
    forwarding every write to the target until the VDI is deactivated or the
    job is cancelled.
  - If the VDI is active read-only, the job ends once the copy is complete.
  - While a job exists the backend must not change the VDI's chain under it
    (for qcow2, live merges in the chain; for VHD, leaf-coalesce, as today).
- Errors:
  - Fault 38 (not implemented in backend) for a VDI for which
    `vdi_mirror_supported` is false. xapi does not call it for such VDIs; the
    error is a safety net and must be raised before making any change.
  - Any other failure as an SR backend error.

#### `vdi_mirror_stat`

- `args`: `[operation]`
- Result: a struct `{failed: bool; complete: bool; progress: double}`, where a
  negative `progress` means unknown.
  - `complete`: for a live mirror, the target is in sync and every subsequent
    write is being forwarded; for a one-shot copy, the copy has finished.
  - `failed`: the job has stopped and the target cannot be relied on.
- The result of a job that has ended (completed, failed, or finished by
  deactivate) remains available until `vdi_mirror_cancel` is called for it or
  the host reboots, so xapi can read the final status after the VDI has been
  deactivated.
- Errors: an unknown operation id is an SR backend error.

#### `vdi_mirror_cancel`

- `args`: `[operation]`
- Result: unit.
- Semantics: stop the job if it is still running, wait until it has stopped,
  and forget its result. The target's content is undefined afterwards.
  Idempotent: cancelling an unknown or already-cancelled id succeeds.

#### `vdi_import_activate`

The destination half of a tapdisk mirror into a VDI the backend serves
itself.

- `args`: `[]`
- Result: a string, the path of a Unix socket that accepts a connected NBD
  client socket passed with `SCM_RIGHTS`, and serves the VDI read-write to it.
  The client is tapdisk's NBD client, which asks for the export name
  `tapdisk_client`.
- Preconditions: the VDI is attached and activated read-write on this host.
- Semantics: as SMAPIv3's `Datapath.import_activate`. Xapi calls it once per
  inbound mirror and passes the mirror connection to the returned socket.
  Until the VDI is deactivated the backend must keep merges off its chain, as
  for an outbound job.
- Errors: fault 38 for a VDI the backend does not serve itself, as for
  `vdi_mirror`.

#### Deactivate

`vdi_deactivate` of a VDI with a live mirror job must complete the job before
returning: stop accepting new guest I/O, wait until every write that reached
the source has also reached the target, then end the job and record its final
status for `vdi_mirror_stat`. This replaces the `reqs_outstanding` polling in
`pre_deactivate_hook`. Deactivate must not fail because the mirror failed;
xapi learns that from `vdi_mirror_stat`.

### Storage interface (SMAPIv2)

Two new SMAPIv2 calls are needed:

- `DATA.mirror_supported dbg sr vdi`, returning a boolean. For SMAPIv3 SRs it
  returns true without asking the plugin, because a SMAPIv3 backend always
  mirrors its own VDIs.
- `DATA.cancel dbg sr vdi vm operation`, stopping a backend mirror. For
  SMAPIv3 SRs it calls the plugin's `Data.cancel`, which exists in the plugin
  interface but which xapi has never called; the SMAPIv3 SXM work needs it
  too.

For SMAPIv1 SRs, `storage_smapiv1.ml` implements:

- `DATA.mirror_supported dbg sr vdi`: false if the SR does not declare
  `VDI_MIRROR_NBD`, otherwise the result of `vdi_mirror_supported`.
- `DATA.mirror dbg sr vdi image_format vm url`: call `vdi_mirror` with
  `remote = url`, returning `Mirror` of the operation id. A backend fault 38
  becomes `Storage_error (Unimplemented "DATA.mirror")`.
- `DATA.stat dbg sr vdi vm operation`: call `vdi_mirror_stat`.
- `DATA.cancel dbg sr vdi vm operation`: call `vdi_mirror_cancel`.

Xapi's mirror stop and failure paths call `DATA.cancel` for every VDI that
took the backend mirror, whichever SMAPI version its SR uses, so there is one
cancel path for both.

`DATA.get_nbd_server` for SMAPIv1 SRs stops guessing tapdisk socket names
when the datapath's attach result has an `Nbd` implementation, and returns
that socket instead, as SMAPIv3 SRs already do. The tapdisk
socket names remain the fallback for attach results without one.

`DATA.import_activate` for SMAPIv1 SRs does the same: when the attach result
has no tapdisk it calls `vdi_import_activate` and returns that socket, where
today it fails with "No tapdisk attach info found".

### Xapi

#### Choosing the implementation

`Storage_migrate.choose_backend` currently picks the tapdisk or backend mirror
from the source SR's SMAPI version. It becomes per VDI:

1. A source VDI on an SMAPIv3 SR takes the backend mirror, as today.
2. A source VDI on an SMAPIv1 SR takes the backend mirror if
   `DATA.mirror_supported` returns true for it.
3. Any other source VDI takes the tapdisk mirror, as today.

The decision costs one call to the source backend per disk, before
`receive_start3` creates anything on the destination, so choosing the tapdisk
mirror costs nothing there. It is made once per disk, on its active VDI, and
applies to that disk's snapshot tree as well: the backend's answer is the same
for every VDI in the tree, and migrating a tree half one way and half the
other would not reproduce it.

`Unimplemented` from `DATA.mirror` on a VDI routed by (2) means the backend
broke its declaration; it fails the migration, with the normal cleanup,
rather than triggering a fallback.

#### Supported directions

For an SMAPIv1 SR whose backend serves qcow2 itself:

| Source | Destination | Mirror | Destination needs |
|---|---|---|---|
| SMAPIv1 qcow2 | SMAPIv1 qcow2 | backend | NBD server from the attach result |
| SMAPIv1 qcow2 | SMAPIv3 | backend | nothing new |
| SMAPIv1 qcow2 | SMAPIv1 VHD | refused | |
| SMAPIv1 VHD | SMAPIv1 qcow2 | tapdisk | `vdi_import_activate`, `vdi_compose` |
| SMAPIv3 | SMAPIv1 qcow2 | backend | NBD server from the attach result |

A disk taking the backend mirror from an SMAPIv1 SR may migrate to the same
image format or a newer one, never an older one. Xapi refuses qcow2 to VHD
up front, alongside `assert_vdi_format_is_supported`, before anything is
created on the destination: a VHD cannot hold a disk larger than 2 TiB, and
the reverse direction is not worth the cost of supporting it. Migrations from
SMAPIv3 SRs keep their current rules.

A destination needs nothing new to receive a tapdisk mirror unless its VDI
is served outside tapdisk. No query is needed there either, and none is
possible before the destination VDI exists: `DATA.import_activate` sees from
the attach result whether a tapdisk serves the VDI, and a backend that serves
a VDI itself implements `vdi_import_activate` and `vdi_compose` for it under
the same capability.

#### What is reused unchanged

Everything the backend mirror already does for transport and orchestration:
`receive_start3`, the source NBD proxy and the destination
`import_nbd_proxy` handler, `wait_for_mirror`, the snapshot-tree walk,
`update_snapshot_info`, `receive_finalize3` and `receive_cancel2`.

#### What changes

- The checks guarding the backend mirror (`assert_migratable` and similar)
  apply to every VDI that takes it, not just to VDIs on SMAPIv3 SRs.
- `MigrateRemote.receive_finalize3` and `receive_cancel2` choose their
  implementation from the source SR, like `choose_backend` today. With the
  choice made per VDI, a VDI on an SMAPIv1 SR taking the backend mirror would
  be finalised by the tapdisk mirror, which has no record of it. The chosen
  implementation is recorded in the mirror's state when it starts, and
  finalize and cancel use that record.
- After the source VDI is deactivated, before `receive_finalize3`, xapi reads
  the final `DATA.stat` and fails the migration if the mirror failed. The
  backend mirror currently finalises without checking, because SMAPIv3
  `Data.stat` stops working once the domain is paused; the SMAPIv1 operations
  keep the result available, and SMAPIv3 plugins should be asked to do the
  same.
- Mirror send state for these VDIs stores the operation id, never a tapdisk.
- `DATA.mirror_supported`, for the dispatch above.
- `DATA.cancel` on the mirror stop and failure paths, which today leave a
  backend mirror running.
- The format-direction check in "Supported directions".

### Snapshots

The snapshot-tree walk on `feature/sxm-v3` mirrors each read-only snapshot VDI
into a clone of the destination copy of its parent, then snapshots the result.
Because `vdi_mirror` copies one level (see its semantics above), each step
sends only the data that snapshot added, and the live leaf finally sends only
what was written since the newest snapshot. The tree is reproduced on the
wire as the deltas it is made of, with no change to the walk and no extra
arguments.

This relies on xapi's snapshot tree and the backend's chain agreeing level for
level, which is why a backend must not copy a partial level while garbage
collection has an unmerged node in between.

### Destination

The destination format is chosen as today, from the destination SR and the
requested format, subject to the direction rule above. What the destination
VDI must provide depends on which mirror the source takes.

- **Backend mirror.** The destination VDI must be attachable read-write with
  an attach result that offers an NBD server, which VDIs served by tapdisk
  (through its NBD server), VDIs served by qemu-dp and SMAPIv3 VDIs all do.
  `DATA.get_nbd_server` takes it from the attach result.
- **Tapdisk mirror.** Xapi's `nbd_handler` gets a socket from
  `DATA.import_activate` and passes the mirror connection to it with
  `SCM_RIGHTS`; at the end, `receive_finalize` composes the mirrored leaf onto
  the copied base with `VDI.compose`. (The base itself is copied through
  `nbdproxy/import`, which uses the NBD server above.) For a VDI served by
  tapdisk both steps work as today. For a VDI the backend serves itself,
  `DATA.import_activate` calls `vdi_import_activate`, and the backend must
  implement `vdi_compose` for the format.

## Requirements on an implementing SM

This design changes xapi only. No change to the `xapi-project/sm` tree is
required: xapi calls the operations only on an SR that declares the
capability. XenServer's implementation lives in XenServer's own SM.

An SM that declares `VDI_MIRROR_NBD` must:

- implement `vdi_mirror_supported` for every VDI on the SR, answering true
  exactly for the VDIs it mirrors itself, with an answer that does not change
  while the VDI exists;
- implement `vdi_mirror`, `vdi_mirror_stat`, `vdi_mirror_cancel`,
  `vdi_import_activate` and `vdi_compose` for every VDI it answers true for;
- copy exactly one level, as the SMAPIv3 plugins do: the VDI's own writable
  layer, or for a snapshot the frozen layer it was taken from;
- defer a job, starting it from a later `vdi_mirror_stat`, while a layer
  between that one and the parent in xapi's tree is awaiting a merge;
- keep merges and coalesces off the VDI's chain for the job's lifetime, and
  not let anything that preempts background work cancel a migration mirror;
- keep the final status readable after the job ends, until cancelled or the
  host reboots;
- on deactivate, after the guest has stopped issuing I/O, finish a live job
  gracefully, record its result, and not fail because the mirror failed;
- run the mirror from a process that can connect to xapi's NBD proxy socket
  under `/var/run/nbdproxy/export/`;
- copy ranges the level holds as zeroes, discards over parent data included,
  so that they read back as zeroes on the target over the copied parent;
- for `vdi_import_activate`, accept the passed client socket, serve it the
  export `tapdisk_client`, and keep merges off the receiving VDI's chain until
  it is deactivated;
- keep that connection usable across a pause of the source tapdisk. On pause
  tapdisk sends `NBD_CMD_DISC` and, after unpause, carries on over the same
  connection, which the NBD protocol does not allow: a conforming server
  closes the connection on `NBD_CMD_DISC`.

As an illustration, a qemu-dp datapath maps these directly onto QMP:
`blockdev-add` an NBD client node for `remote` and `blockdev-mirror` with
`sync: top` for `vdi_mirror`; `query-block-jobs` for `vdi_mirror_stat`
(ready is `complete`, error or abort is `failed`, offset over length is
`progress`); `block-job-cancel` with `force` then `blockdev-del` for
`vdi_mirror_cancel`; and `block-job-cancel` without `force`, the graceful end
of a ready mirror, on deactivate. Unlike qemu-dm, qemu-dp is not
deprivileged: it runs as root because the storage it serves may be readable
only by root, so it can reach the proxy socket. For `vdi_import_activate`,
the client socket cannot simply be handed to qemu-dp (for example with QMP
`getfd` and `add_client`), because qemu-dp closes it on tapdisk's
`NBD_CMD_DISC`. The SMAPIv3 plugins in `xapi-storage-plugins` use a small
helper, `smapiv1-nbd-proxy`, that receives the socket, relays it to qemu-dp's
NBD server, on which they add an export named `tapdisk_client`, and when
qemu-dp drops its side reconnects to it while keeping tapdisk's side open.

## Upgrade and compatibility

- An SM without `VDI_MIRROR_NBD`: xapi never calls the new operations.
- An older xapi with a new SM: never calls them either, so VDIs that only the
  new path can migrate (for XenServer, qcow2 VDIs served by qemu-dp) cannot be
  storage-migrated, which is the case today.
- Mixed pools during a rolling upgrade: the capability is merged pool-wide as
  for other SM features, so the new path is used only once every host has it.
- Cross-pool, backend mirror: the destination needs nothing new. Its VDI must
  offer an NBD server on attach, which every current datapath does.
- Cross-pool, tapdisk mirror into a VDI served outside tapdisk: needs a
  destination with this design. An older destination host fails at
  `DATA.import_activate`, as it does today.

## Testing

- xapi unit tests: per-VDI dispatch on the capability and
  `DATA.mirror_supported` (no destination call for a VDI that keeps the
  tapdisk mirror, and no `vdi_mirror_supported` call to an SR without the
  capability), finalize and cancel following the recorded choice,
  refusal of qcow2 to VHD before any destination call, final-status check
  after deactivate, `DATA.get_nbd_server` and `DATA.import_activate` without
  a tapdisk.
- Unit tests in each implementing SM for the five operations and
  `vdi_compose`, `vdi_mirror_supported` for each format the SR serves, node
  selection for active and snapshot VDIs, deferral while a merge is pending,
  the merge fence and deactivate draining.
- System tests (for XenServer, XenRT), on LVM over iSCSI, local LVM and NFS,
  each with qcow2 VDIs served by qemu-dp:
  - live and halted VMs, intra-pool and cross-pool;
  - VMs with snapshot trees, including reverted branches, and with a
    snapshot deleted shortly before migrating (merge pending);
  - mixed VHD and qcow2 disks on the same SR;
  - an SM without the capability, which must keep today's behaviour;
  - cancellation and induced mirror failure (destination space, network),
    checking the destination is left clean;
  - each direction in "Supported directions", including the refused one;
  - bytes sent for a snapshot tree, against the sum of its levels;
  - a guest discard in the active level over data its snapshot holds, then a
    migration, then a comparison of the discarded range on both sides: it
    must read back as zeroes on the destination too.

## Open questions

1. **Final status on SMAPIv3.** SMAPIv3 `Data.stat` fails once the domain is
   paused, so the v3 path finalises without knowing the mirror's last state.
   This design requires SMAPIv1 backends to keep it and proposes the same for
   SMAPIv3 plugins; that needs agreeing with the `feature/sxm-v3` work.
2. **A per-VDI query or a published format.** The alternative is for xapi to
   read the VDI's `image-format` from `sm_config` against a versioned
   capability listing formats. Asking the backend instead keeps the policy in
   the backend, avoids the metadata and the scan-time backfill that would
   need, and lets a backend add a format without a xapi change, at the cost
   of one call per disk at migration start. If xapi gains a first-class per-VDI
   image format for other reasons, the query can still stand: whether a
   backend mirrors a VDI is not determined by format alone.
3. **Halted VMs.** The leaf is activated in dom0 for the mirror; the fix for
   the equivalent SMAPIv3 case (xapi-project/xen-api#7334) should apply
   unchanged, but needs confirming for SMAPIv1 datapaths.
4. **One capability or two.** `VDI_MIRROR_NBD` covers both directions, so an
   SM cannot offer outbound migration for a format until it can also receive
   a tapdisk mirror into it. Splitting it into outbound and inbound
   capabilities, as `VDI_MIRROR` and `VDI_MIRROR_IN` are, would let the two
   ship separately at the cost of another check.
5. **Module names.** Renaming `Storage_smapiv1_migrate` and
   `Storage_smapiv3_migrate` after the mirror each runs would make the code
   match this document; it is not required.
