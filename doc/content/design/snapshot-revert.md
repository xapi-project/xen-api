---
title: Better VM revert
layout: default
design_doc: true
revision: 2
status: confirmed
---

## Overview

XenAPI allows users to rollback the state of a VM to a previous state, which is
stored in a snapshot, using the call `VM.revert`. Because there is no
`VDI.revert` call, `VM.revert` uses `VDI.clone` on the snapshot to duplicate
the contents of that disk and then use the new clone as the storage for the VM.

Because `VDI.clone` creates new VDI refs and uuids, some problematic
behaviours arise:

- Clients such as 
  [Apache CloudStack](http://cloudstack.apache.org) need to include complex
  logic to keep track of the disks they are actively managing
- Because the snapshot is cloned and the original vdi is deleted, VDI
  references to the VDI become invalid, like `VDI.snapshot_of`. This means
  that the database has to be combed through to change these references. 
  Because the database doesn't support transactions this operation is not atomic
  and can produce inconsistent database states.

Additionally, some filesystems support snapshots natively, doing the clone
procedure is much costlier than allowing the filesystem to do the revert.

We will fix these problems by:

- introducing the new feature `VDI_REVERT` in SM interface (`xapi_smint`). This
  allows backends to advertise that they support the new functionality
- defining a new storage operation `VDI.revert` in storage_interface, which is
  gated by the feature `VDI_REVERT`
- proxying the storage operation to SMAPIv3 and SMAPv1 backends accordingly
- adding `VDI.revert` to xapi_vdi which will call the storage operation if the
  backend advertises it, and fallback to the previous method that uses
  `VDI.clone` if it doesn't advertise it, or issues are detected at runtime
  that prevent it
- changing the Xapi implementation of `VM.revert` to use `VDI.revert`
- implement `vdi_revert` for common storage types, including File and LVM-based
  SRs
- adding unit and quick tests to xapi to test that `VM.revert` does not regress

## Current VM.revert behaviour

The code that reverts the state of storage is located in 
[update_vifs_vbds_vgpus_and_vusbs](https://github.com/xapi-project/xen-api/blob/bc0ba4e9dc8dc4b85b7cbdbf3e0ba5915b4ad76d/ocaml/xapi/xapi_vm_snapshot.ml#L211).
The steps it does is:
1. destroys the VM's VBDs (both disks and CDs)
2. destroys the VM's VDI (disks only), referenced by the snapshot's VDIs using
   `snapshot_of`; as well as the suspend VDI.
3. clones the snapshot's VDIs (disks and CDs), if one clone fails none remain.
4. searches the database for all `snapshot_of` references to the deleted VDIs
   and replaces them with the referenced of the newly cloned snapshots.
5. clones the snapshot's resume VDI
6. creates copies of all the cloned VBDs and associates them with the cloned VDIs
7. assigns the new resume VDI to the VM

## XenAPI design

### API

The function `VDI.revert` will be added, with arguments:

- in: `snapshot: Ref(VDI)`: the snapshot to which we want to revert
- in: `driver_params: Map(String,String)`: optional extra parameters
- out: `Ref(VDI)` reference to the new VDI with the reverted contents

The function will extract the reference of VDI whose contents need to be
replaced. This is the snapshot's `snapshot_of` field, then it will call the
storage function function `VDI.revert` to have its contents replaced with the
snapshot's. The VDI object will not be modified, and the reference returned is
the VDI's original reference.
If anything impedes the successful finish of an in-place revert, like the SM
backend does not advertising the feature `VDI_REVERT`, not implement the
feature, or the `snapshot_of` reference is invalid; an exception will be
raised.

### Xapi Storage

The function `VDI.revert` is added, with the following arguments:

- in: `dbg`: the task identifier, useful for tracing
- in: `sr`: SR where the new VDI must be created
- in: `snapshot_info`: metadata of the snapshot, the contents of which must be
       made available in the VDI indicated by the `snapshot_of` field

#### SMAPIv1

The function `vdi_revert` is defined with the following arguments:

- in: `sr_uuid`: the UUID of the SR containing both the VDI and the snapshot
- in: `vdi_uuid`: the UUID of the snapshot whose contents must be duplicated
- in: `target_uuid`: the UUID of the target whose contents must be replaced

The function will replace the contents of the `target_uuid` VDI with the
contents of the `vdi_uuid` VDI without changing the identify of the target
(i.e. name-label, uuid and location are guaranteed to remain the same).
The `vdi_uuid` is preserved by this operation. The operation is obvoiusly
idempotent.

#### SMAPIv3

In an analogous way to SMAPIv1, the function `Volume.revert` is defined with the
following arguments:

- in: `dbg`: the task identifier, useful for tracing
- in: `sr`: the UUID of the SR containing both the VDI and the snapshot
- in: `snapshot`: the UUID of the snapshot whose contents must be duplicated
- in: `vdi`: the UUID of the VDI whose contents must be replaced

### Xapi

- add the capability `VDI_REVERT` so backends can advertise it
- use `VDI.revert` in the `VM.revert` after the VDIs have been destroyed, and
  before the snapshot's VDIs have been cloned. If any of the reverts fail
  because a `Not_implemented` exception is thrown, or the `snapshot_of`
  contains an invalid reference, add the affected VDIs to the list to be cloned
  and recovered, using the existing method
- expose a new `xe vdi-revert` CLI command

## SM changes

We will modify

- SRCommand.py and VDI.py to add a new `vdi_revert` function which throws
  a 'not implemented' exception
- FileSR.py to implement `VDI.revert` using a variant of the existing
  snapshot/clone machinery
- EXTSR.py and NFSSR.py to advertise the `VDI_REVERT` capability
- LVHDSR.py to implement `VDI.revert` using a variant of the existing
  snapshot/clone machinery
- LVHDoISCSISR.py and LVHDoHBASR.py to advertise the `VDI_REVERT` capability

# Prototype code from the previous proposal

Prototype code exists here:

- [xapi-project/xcp-idl#37](https://github.com/xapi-project/xcp-idl/pull/37) by @johnelse
- [xapi-project/xen-api#2058](https://github.com/xapi-project/xen-api/pull/2058) mainly by @johnelse but with 2 extra patches from me
- [Definition of SMAPIv1 vdi_revert](https://github.com/djs55/sm/commit/cbc28755c9c4300ed067abc089081f58f821f504)
- [Hacky implementation for EXT/NFS](https://github.com/djs55/sm/commit/eb31d6205ccd707152a5b59c9a733fd48db5316b)
