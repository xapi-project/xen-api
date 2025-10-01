---
title: Better VM revert
layout: default
design_doc: true
revision: 2
status: confirmed
---

## Overview

Currently there is a XenAPI `VM.revert` which reverts a VM to the state it was
in when a VM-level snapshot was taken. Because there is no `VDI.revert`,
`VM.revert` uses `VDI.clone` on the snapshot to change the state of the disks.

Because `VDI.clone` creates new VDI refs and uuids, some problematic
behaviours arise:

- Clients such as 
  [Apache CloudStack](http://cloudstack.apache.org) need to include complex
  logic to keep track of the disks they are actively managing
- Because the snapshot is cloned and the original vdi is deleted, VDI
  references to the VDI become invalid, like `VDI.snapshot_of`. This means
  there the database has to be combed through to change these references. 
  Because the database doesn't support transaction this operation is not atomic
  and can produce inconsistent database states.

We will fix these problems by:

- defining a new storage operation `VDI.revert` in storage_interface and
  calling this from `VM.revert`
- proxying the storage operation to SMAPIv3 and SMAPv1 backends accordingly
- changing the Xapi implementation of `VM.revert` to first try the
  `VDI.revert`, and fall back to `VDI.clone` if that fails
- implement `vdi_revert` for common storage types, including File and LVM-based
  SRs
- adding unit and quick tests to xapi to test that `VM.revert` does not regress

## XenAPI design

### API

The function `VDI.revert` will be added, with arguments:

- in: `snapshot: Ref(VDI)`: the snapshot to which we want to revert
- in: `driver_params: Map(String,String)`: optional extra parameters
- out: `Ref(VDI)` the new VDI

The function will look up the VDI which this is a `snapshot_of`, check that
the value is not invalid nor null, and use that VDI reference to call SM to
change the VDI to have the same contents as the snapshot. The snapshot object
will not be modified, and the reference returned is the reference in
`snapshot_of`.
If anything impedes the successful finish of an in-place revert, the previous
method of using `VDI.clone` is used as fallback, and the reference returned
refers to the fresh VDI created by the `VDI.clone` fallback.

### Xapi Storage

The function `VDI.revert` is added, with the following arguments:

- in: `dbg`: the task identifier, useful for tracing
- in: `sr`: SR where the new VDI must be created
- in: `snapshot_info`: metadata of the snapshot, the contents of which must be
       made available in the VDI indicated by the `snapshot_of` field
- out: `vdi_info`: metadata of the resulting VDI

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
- 
### Xapi

- use `VDI.revert` in the `VM.revert` code-path and fall back to `VDI.clone` if
  a `Not_implemented` exception is thrown, or the snapshot_of contains an
  invalid reference
- expose a new `xe vdi-revert` CLI command
- implement the `VDI.revert` 

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
