---
title: Snapshots
layout: xenapi
xenapi_tag: snapshots
---

Snapshots represent the state of a VM, or a disk (VDI) at a point in time.
They can be used for:

- backups (hourly, daily, weekly etc)
- experiments (take snapshot, try something, revert back again)
- golden images (install OS, get it just right, clone it 1000s of times)

Read more about [Snapshots: the High-Level Feature](../../toolstack/features/snapshots/index).

Taking a VDI snapshot
=====================

To take a snapshot of a single disk (VDI):

```
snapshot_vdi <- VDI.snapshot(session_id, vdi, driver_params)
```

where `vdi` is the reference to the disk to be snapshotted, and `driver_params`
is a list of string pairs providing optional backend implementation-specific hints.
The snapshot operation should be quick (i.e. it should never be implemented as
a slow disk copy) and the resulting VDI will have

Field name     | Description
---------------|------------------------------------------------------
is_a_snapshot  | a flag, set to true, indicating the disk is a snapshot
snapshot_of    | a reference to the disk the snapshot was created from
snapshot_time  | the time the snapshot was taken

The resulting snapshot should be considered read-only. Depending on the backend
implementation it may be technically possible to write to the snapshot, but clients
must not do this. To create a writable disk from a snapshot, see "restoring from
a snapshot" below.

Note that the storage backend is free to implement this in different ways. We
do not assume the presence of a .vhd-formatted storage repository. Clients
must never assume anything about the backend implementation without checking
first with the maintainers of the backend implementation.

Restoring to a VDI snapshot
===========================

To restore from a VDI snapshot first

```
new_vdi <- VDI.clone(session_id, snapshot_vdi, driver_params)
```

where `snapshot_vdi` is a reference to the snapshot VDI, and `driver_params`
is a list of string pairs providing optional backend implementation-specific hints.
The clone operation should be quick (i.e. it should never be implemented as
a slow disk copy) and the resulting VDI will have

Field name     | Description
---------------|------------------------------------------------------------
is_a_snapshot  | a flag, set to false, indicating the disk is not a snapshot
snapshot_of    | an invalid reference
snapshot_time  | an invalid time

The resulting disk is writable and can be used by the client as normal.

Note that the "restored" VDI will have a different `VDI.uuid` and reference to
the original VDI.

Taking a VM snapshot
====================

A VM snapshot is a copy of the VM metadata and a snapshot of all the associated
VDIs at around the same point in time. To take a VM snapshot:

```
snapshot_vm <- VM.snapshot(session_id, vm, new_name)
```

where `vm` is a reference to the existing VM and `new_name` will be the `name_label`
of the resulting VM (snapshot) object. The resulting VM will have

Field name     | Description
---------------|------------------------------------------------------
is_a_snapshot  | a flag, set to true, indicating the VM is a snapshot
snapshot_of    | a reference to the VM the snapshot was created from
snapshot_time  | the time the snapshot was taken

Note that each disk is snapshotted one-by-one and not at the same time.

Restoring to a VM snapshot
==========================

A VM snapshot can be reverted to a snapshot using

```
VM.revert(session_id, snapshot_ref)
```

where `snapshot_ref` is a reference to the snapshot VM. Each VDI associated with
the VM before the snapshot will be destroyed and each VDI associated with the
snapshot will be cloned (see "Reverting to a disk snapshot" above) and associated
with the VM. The resulting VM will have

Field name     | Description
---------------|----------------------------------------------------------
is_a_snapshot  | a flag, set to false, indicating the VM is not a snapshot
snapshot_of    | an invalid reference
snapshot_time  | an invalid time

Note that the `VM.uuid` and reference are preserved, but the `VDI.uuid` and
VDI references are not.

Downloading a disk or snapshot
==============================

Disks can be downloaded in either raw or vhd format using an HTTP 1.0 GET
request as follows:

```
GET /export_raw_vdi?session_id=%s&task_id=%s&vdi=%s&format=%s[&base=%s] HTTP/1.0\r\n
Connection: close\r\n
\r\n
\r\n
```

where

- `session_id` is a currently logged-in session
- `task_id` is a `Task` reference which will be used to monitor the
  progress of this task and receive errors from it
- `vdi` is the reference of the `VDI` into which the data will be
  imported
- `format` is either `vhd` or `raw`
- (optional) `base` is the reference of a `VDI` which has already been
  exported and this export should only contain the blocks which have changed
  since then.

Note that the vhd format allows the disk to be sparse i.e. only contain allocated
blocks. This helps reduce the size of the download.

The xapi-project/xen-api repo has a
[python download example](https://github.com/xapi-project/xen-api/blob/19afd3dfe8883814e525ce7ce39c8c959ce3c924/scripts/examples/python/exportimport.py#L32)

Uploading a disk or snapshot
=============================

Disks can be uploaded in either raw or vhd format using an HTTP 1.0 PUT
request as follows:

```
PUT /import_raw_vdi?session_id=%s&task_id=%s&vdi=%s&format=%s HTTP/1.0\r\n
Connection: close\r\n
\r\n
\r\n
```

where

- `session_id` is a currently logged-in session
- `task_id` is a `Task` reference which will be used to monitor the
  progress of this task and receive errors from it
- `vdi` is the reference of the `VDI` into which the data will be
  imported
- `format` is either `vhd` or `raw`

Note that you must create the disk (with the correct size) before importing
data to it. The disk doesn't have to be empty, in fact if restoring from a
series of incremental downloads it makes sense to upload them all to the
same disk in order.

Example: incremental backup with xe
===================================

This section will show how easy it is to build an incremental backup
tool using these APIs. For simplicity we will use the `xe` commands
rather than raw XMLRPC and HTTP.

For a VDI with uuid $VDI, take a snapshot:

```sh
FULL=$(xe vdi-snapshot uuid=$VDI)
```

Next perform a full backup into a file "full.vhd", in vhd format:

```sh
xe vdi-export uuid=$FULL filename=full.vhd format=vhd  --progress
```

If the SR was using the vhd format internally (this is the default)
then the full backup will be sparse and will only contain blocks if they
have been written to.

After some time has passed and the VDI has been written to, take another
snapshot:

```sh
DELTA=$(xe vdi-snapshot uuid=$VDI)
```

Now we can backup only the disk blocks which have changed between the original
snapshot $FULL and the next snapshot $DELTA into a file called "delta.vhd":

```sh
xe vdi-export uuid=$DELTA filename=delta.vhd format=vhd base=$FULL --progress
```

We now have 2 files on the local system:

- "full.vhd": a complete backup of the first snapshot
- "delta.vhd": an incremental backup of the second snapshot, relative to
  the first

For example:

```sh
test $ ls -lh *.vhd
-rw------- 1 dscott xendev 213M Aug 15 10:39 delta.vhd
-rw------- 1 dscott xendev 8.0G Aug 15 10:39 full.vhd
```

To restore the original snapshot you must create an empty disk with the
correct size. To find the size of a .vhd file use ```qemu-img``` as follows:

```sh
test $ qemu-img info delta.vhd
image: delta.vhd
file format: vpc
virtual size: 24G (25769705472 bytes)
disk size: 212M
```

Here the size is 25769705472 bytes.
Create a fresh VDI in SR $SR to restore the backup as follows:

```sh
SIZE=25769705472
RESTORE=$(xe vdi-create name-label=restored virtual-size=$SIZE sr-uuid=$SR type=user)
```

then import "full.vhd" into it:

```sh
xe vdi-import uuid=$RESTORE filename=full.vhd format=vhd --progress
```

Once "full.vhd" has been imported, the incremental backup can be restored
on top:

```sh
xe vdi-import uuid=$RESTORE filename=delta.vhd format=vhd --progress
```

Note there is no need to supply a "base" parameter when importing; Xapi will
treat the "vhd differencing disk" as a set of blocks and import them. It
is up to you to check you are importing them to the right place.

Now the VDI $RESTORE should have the same contents as $DELTA.
