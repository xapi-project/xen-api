---
title: Improving snapshot revert behaviour
layout: default
design_doc: true
revision: 1
status: confirmed
---

Currently there is a XenAPI `VM.revert` which reverts a "VM" to the state it
was in when a VM-level snapshot was taken. There is no `VDI.revert` so
`VM.revert` uses `VDI.clone` to change the state of the disks.

The use of `VDI.clone` has the side-effect of changing VDI refs and uuids.
This causes the following problems:

- It is difficult for clients
  such as [Apache CloudStack](http://cloudstack.apache.org) to keep track
  of the disks it is actively managing
- VDI snapshot metadata (`VDI.snapshot_of` et al) has to be carefully
  fixed up since all the old refs are now dangling

We will fix these problems by:

1. adding a `VDI.revert` to the SMAPIv2 and calling this from `VM.revert`
2. defining a new SMAPIv1 operation `vdi_revert` and a corresponding capability
   `VDI_REVERT`
3. the Xapi implementation of `VDI.revert` will first try the `vdi_revert`,
   and fall back to `VDI.clone` if that fails
4. implement `vdi_revert` for common storage types, including File and LVM-based
   SRs.

XenAPI changes
--------------

We will add the function `VDI.revert` with arguments:

- in: `snapshot: Ref(VDI)`: the snapshot to which we want to revert
- in: `driver_params: Map(String,String)`: optional extra parameters
- out: `Ref(VDI)` the new VDI

The function will look up the VDI which this is a `snapshot_of`, and change
the VDI to have the same contents as the snapshot. The snapshot will not be
modified. If the implementation is able to revert in-place, then the reference
returned will be the VDI this is a `snapshot_of`; otherwise it is a reference
to a fresh VDI (created by the `VDI.clone` fallback path)

References:

- @johnelse's [pull request](https://github.com/xapi-project/xen-api/pull/1963)
  which implements this

SMAPIv1 changes
---------------

We will define the function `vdi_revert` with arguments:

- in: `sr_uuid`: the UUID of the SR containing both the VDI and the snapshot
- in: `vdi_uuid`: the UUID of the snapshot whose contents should be duplicated
- in: `target_uuid`: the UUID of the target whose contents should be replaced

The function will replace the contents of the `target_uuid` VDI with the
contents of the `vdi_uuid` VDI without changing the identify of the target
(i.e. name-label, uuid and location are guaranteed to remain the same).
The `vdi_uuid` is preserved by this operation. The operation is obvoiusly
idempotent.

Xapi changes
------------

Xapi will

- use `VDI.revert` in the `VM.revert` code-path
- expose a new `xe vdi-revert` CLI command
- implement the `VDI.revert` by calling the SMAPIv1 function and falling back
  to `VDI.clone` if a `Not_implemented` exception is thrown

References:

- @johnelse's [pull request](https://github.com/xapi-project/xen-api/pull/1963)

SM changes
----------

We will modify

- SRCommand.py and VDI.py to add a new `vdi_revert` function which throws
  a 'not implemented' exception
- FileSR.py to implement `VDI.revert` using a variant of the existing
  snapshot/clone machinery
- EXTSR.py and NFSSR.py to advertise the `VDI_REVERT` capability
- LVHDSR.py to implement `VDI.revert` using a variant of the existing
  snapshot/clone machinery
- LVHDoISCSISR.py and LVHDoHBASR.py to advertise the `VDI_REVERT` capability

Prototype code
==============

Prototype code exists here:

- [xapi-project/xcp-idl#37](https://github.com/xapi-project/xcp-idl/pull/37) by @johnelse
- [xapi-project/xen-api#2058](https://github.com/xapi-project/xen-api/pull/2058) mainly by @johnelse but with 2 extra patches from me
- [Definition of SMAPIv1 vdi_revert](https://github.com/djs55/sm/commit/cbc28755c9c4300ed067abc089081f58f821f504)
- [Hacky implementation for EXT/NFS](https://github.com/djs55/sm/commit/eb31d6205ccd707152a5b59c9a733fd48db5316b)
