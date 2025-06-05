---
Title: Storage migration
---

- [Overview](#overview)
- [SXM Multiplexing](#sxm-multiplexing)
  - [Motivation](#motivation)
    - [But we have storage\_mux.ml](#but-we-have-storage_muxml)
    - [Thought experiments on an alternative design](#thought-experiments-on-an-alternative-design)
  - [Design](#design)
- [SMAPIv1 migration](#smapiv1-migration)
  - [Preparation](#preparation)
  - [Establishing mirror](#establishing-mirror)
    - [Mirror](#mirror)
    - [Snapshot](#snapshot)
    - [Copy and compose](#copy-and-compose)
    - [Finish](#finish)
- [SMAPIv3 migration](#smapiv3-migration)
  - [Preparation](#preparation-1)
  - [Establishing mirror](#establishing-mirror-1)
    - [Limitations](#limitations)
  - [Finish](#finish-1)
- [Error Handling](#error-handling)
  - [Preparation (SMAPIv1 and SMAPIv3)](#preparation-smapiv1-and-smapiv3)
  - [Snapshot and mirror failure (SMAPIv1)](#snapshot-and-mirror-failure-smapiv1)
  - [Mirror failure (SMAPIv3)](#mirror-failure-smapiv3)
  - [Copy failure (SMAPIv1)](#copy-failure-smapiv1)
- [SMAPIv1 Migration implementation detail](#smapiv1-migration-implementation-detail)
  - [Receiving SXM](#receiving-sxm)
  - [Xapi code](#xapi-code)
  - [Storage code](#storage-code)
    - [Copying a VDI](#copying-a-vdi)
    - [Mirroring a VDI](#mirroring-a-vdi)
    - [Code walkthrough](#code-walkthrough)
      - [DATA.copy](#datacopy)
      - [DATA.copy\_into](#datacopy_into)
      - [DATA.MIRROR.start](#datamirrorstart)


## Overview

The core idea of storage migration is surprisingly simple: We have VDIs attached to a VM, 
and we wish to migrate these VDIs from one SR to another. This necessarily requires 
us to copy the data stored in these VDIs over to the new SR, which can be a long-running 
process if there are gigabytes or even terabytes of them. We wish to minimise the 
down time of this process to allow the VM to keep running as much as possible.

At a very high level, the SXM process generally only consists of two stages: preparation 
and mirroring. The preparation is about getting the receiving host ready for the
mirroring operation, while the mirroring itself can be further divided into two 
more operations: 1. sending new writes to both sides; 2.copying existing data from 
source to destination. The exact detail of how to set up a mirror differs significantly
between SMAPIv1 and SMAPIv3, but both of them will have to perform the above two
operations.
Once the mirroring is established, it is a matter of checking the status of the 
mirroring and carry on with the follwoing VM migration.

The reality is more complex than what we had hoped for. For example, in SMAPIv1,
the mirror establishment is quite an involved process and is itself divided into
several stages, which will be discussed in more detail later on.


## SXM Multiplexing

This section is about the design idea behind the additional layer of mutiplexing specifically
for Storage Xen Motion (SXM) from SRs using SMAPIv3. It is recommended that you have read the 
[introduction doc](_index.md) for the storage layer first to understand how storage
multiplexing is done between SMAPIv2 and SMAPI{v1, v3} before reading this.


### Motivation

The existing SXM code was designed to work only with SMAPIv1 SRs, and therefore
does not take into account the dramatic difference in the ways SXM is done between
SMAPIv1 and SMAPIv3. The exact difference will be covered later on in this doc, for this section
it is sufficient to assume that they have two ways of doing migration. Therefore,
we need different code paths for migration from SMAPIv1 and SMAPIv3.    

#### But we have storage_mux.ml

Indeed, storage_mux.ml is responsible for multiplexing and forwarding requests to
the correct storage backend, based on the SR type that the caller specifies. And
in fact, for inbound SXM to SMAPIv3 (i.e. migrating into a SMAPIv3 SR, GFS2 for example),
storage_mux is doing the heavy lifting of multiplexing between different storage
backends. Every time a `Remote.` call is invoked, this will go through the SMAPIv2
layer to the remote host and get multiplexed on the destination host, based on
whether we are migrating into a SMAPIv1 or SMAPIv3 SR (see the diagram below). 
And the inbound SXM is implemented
by implementing the existing SMAPIv2 -> SMAPIv3 calls (see `import_activate` for example)
which may not have been implemented before.

![mux for inbound](sxm_mux_inbound.svg)

While this works fine for inbound SXM, it does not work for outbound SXM. A typical SXM
consists of four combinations, the source sr type (v1/v3) and the destiantion sr
type (v1/v3), any of the four combinations is possible. We have already covered the
destination multiplexing (v1/v3) by utilising storage_mux, and at this point we
have run out of multiplexer for multiplexing on the source. In other words, we
can only mutiplex once for each SMAPIv2 call, and we can either use that chance for
either the source or the destination, and we have already used it for the latter.


#### Thought experiments on an alternative design

To make it even more concrete, let us consider an example: the mirroring logic in
SXM is different based on the source SR type of the SXM call. You might imagine
defining a function like `MIRROR.start v3_sr v1_sr` that will be multiplexed
by the storage_mux based on the source SR type, and forwarded to storage_smapiv3_migrate,
or even just xapi-storage-script, which is indeed quite possible. 
Now at this point we have already done the multiplexing, but we still wish to 
multiplex operations on destination SRs, for example, we might want to attach a 
VDI belonging to a SMAPIv1 SR on the remote host. But as we have already done the
multiplexing and is now inside xapi-storage-script, we have lost any chance of doing
any further multiplexing :(

### Design

The idea of this new design is to introduce an additional multiplexing layer that
is specific for multiplexing calls based on the source SR type. For example, in
the diagram below the `send_start src_sr dest_sr` will take both the src SR and the
destination SR as parameters, and suppose the mirroring logic is different for different
types of source SRs (i.e. SMAPIv1 or SMAPIv3), the storage migration code will
necessarily choose the right code path based on the source SR type. And this is
exactly what is done in this additional multiplexing layer. The respective logic
for doing {v1,v3}-specifi mirroring, for example, will stay in storage_smapi{v1,v3}_migrate.ml

![mux for outbound](sxm_mux_outbound.svg)

Note that later on storage_smapi{v1,v3}_migrate.ml will still have the flexibility
to call remote SMAPIv2 functions, such as `Remote.VDI.attach dest_sr vdi`, and
it will be handled just as before.

## SMAPIv1 migration

This section is about migration from SMAPIv1 SRs to SMAPIv1 or SMAPIv3 SRs, since
the migration is driven by the source host, it is usally the source host that
determines most of the logic during a storage migration.

First we take a look at an overview diagram of what happens during SMAPIv1 SXM:
the diagram is labelled with S1, S2 ... which indicates different stages of the migration.
We will talk about each stage in more detail below.

![overview-v1](sxm-overview-v1.svg)

### Preparation

Before we can start our migration process, there are a number of preparations 
needed to prepare for the following mirror. For SMAPIv1 this involves:

1. Create a new VDI (called leaf) that will be used as the receiving VDI for all the new writes
2. Create a dummy snapshot of the VDI above to make sure it is a differencing disk and can be composed later on
3. Create a VDI (called parent) that will be used to receive the existing content of the disk (the snapshot)

Note that the leaf VDI needs to be attached and activated on the destination host (to a non-exsiting `mirror_vm`) 
since it will later on accept writes to mirror what is written on the source host. 

The parent VDI may be created in two different ways: 1. If there is a "similar VDI",
clone it on the destination host and use it as the parent VDI; 2. If there is no
such VDI, create a new blank VDI. The similarity here is defined by the distances
between different VDIs in the VHD tree, which is exploiting the internal representation
of the storage layer, hence we will not go into too much detail about this here.

Once these preparations are done, a `mirror_receive_result` data structure is then
passed back to the source host that will contain all the necessary information about
these new VDIs, etc.

### Establishing mirror

At a high level, mirror establishment for SMAPIv1 works as follows:

1. Take a snapshot of a VDI that is attached to VM1. This gives us an immutable 
copy of the current state of the VDI, with all the data up until the point we took 
the snapshot. This is illustrated in the diagram as a VDI and its snapshot connecting 
to a shared parent, which stores the shared content for the snapshot and the writable 
VDI from which we took the snapshot (snapshot)
2. Mirror the writable VDI to the server hosts: this means that all writes that goes to the 
client VDI will also be written to the mirrored VDI on the remote host (mirror)
3. Copy the immutable snapshot from our local host to the remote (copy)
4. Compose the mirror and the snapshot to form a single VDI 
5. Destroy the snapshot on the local host (cleanup)

#### Mirror

The mirroring process for SMAPIv1 is rather unconventional, so it is worth
documenting how this works. Instead of a conventional client server architecture,
where the source client connects to the destination server directly through the
NBD protocol in tapdisk, the connection is established in xapi and then passed
onto tapdisk. It was done in this rather unusual way mainly due to authentication
issues. Because it is xapi that is creating the connection, tapdisk does not need
to be concerned about authentication of the connection, thus simplifying the storage
component. This is reasonable as the storage component should focus on handling
storage requests rather than worrying about network security.

The diagram below illustrates this prcess. First, xapi on the source host will
initiate an https request to the remote xapi. This request contains the necessary
information about the VDI to be mirrored, and the SR that contains it, etc. This
information is then passed onto the https handler on the destination host (called
`nbd_handler`) which then processes this information. Now the unusual step is that
both the source and the destination xapi will pass this connection onto tapdisk,
by sending the fd representing the socket connection to the tapdisk process. On
the source this would be nbd client process of tapdisk, and on the destination
this would be the nbd server process of the tapdisk. After this step, we can consider
a client-server connection is established between two tapdisks on the client and
server, as if the tapdisk on the source host makes a request to the tapdisk on the
destination host and initiates the connection. On the diagram, this is indicated 
by the dashed lines between the tapdisk processes. Logically, we can view this as
xapi creates the connection, and then passes this connection down into tapdisk.

![mirror](sxm-mirror-v1.svg)

#### Snapshot

The next step would be create a snapshot of the VDI. This is easily done as a
`VDI.snapshot` operation. If the VDI was in VHD format, then internally this would
create two children for, one for the snapshot, which only contains the metadata
information and tends to be small, the other for the writable VDI where all the
new writes will go to. The shared base copy contains the shared blocks.

![snapshot](sxm-snapshot-v1.svg)

#### Copy and compose

Once the snapshot is created, we can then copy the snapshot from the source
to the destination. This step is done by `sparse_dd` using the nbd protocol. This
is also the step that takes the most time to complete.

`sparse_dd` is a process forked by xapi that does the copying of the disk blocks.
`sparse_dd` can supports a number of protocols, including nbd. In this case, `sparse_dd`
will initiate an https put request to the destination host, with a url of the form
`<address>/services/SM/nbdproxy/<sr>/<vdi>`. This https request then
gets handled by the https handler on the destination host B, which will then spawn
a handler thread. This handler will find the 
"generic" nbd server[^2] of either tapdisk or qemu-dp, depending on the destination 
SR type, and then start proxying data between the https connection socket and the 
socket connected to the nbd server. 

[^2]: The server is generic because it does not accept fd passing, and I call those
"special" nbd server/fd receiver.

![sxm new copy](sxm-new-copy-v1.svg)

Once copying is done, the snapshot and mirrored VDI can be then composed into a
single VDI.

#### Finish

At this point the VDI is synchronised to the new host! Mirror is still working at this point
though because that will not be destroyed until the VM itself has been migrated
as well. Some cleanups are done at this point, such as deleting the snapshot
that is taken on the source, destroying the mirror datapath, etc.

The end results look like the following. Note that VM2 is in dashed line as it
is not yet created yet. The next steps would be to migrate the VM1 itself to the
destination as well, but this is part of the VM migration process and will not
be covered here.

![final](sxm-final-v1.svg)


## SMAPIv3 migration

This section covers the mechanism of migrations *from* SRs using SMAPIv3 (to
SMAPIv1 or SMAPIv3). Although the core ideas are the same, SMAPIv3 has a rather 
different mechanism for mirroring: 1. it does not require xapi to take snapshot
of the VDI anymore, since the mirror itself will take care of replicating the
existing data to the destination; 2. there is no fd passing for connection establishment anymore, and instead proxies are used for connection setup. 

### Preparation

The preparation work for SMAPIv3 is greatly simplified by the fact that the mirror
at the storage layer will copy the existing data in the VDI to the destination.
This means that snapshot of the source VDI is not required anymore. So we are left
with only one thing:

1. Create a VDI used for mirroring the data of the source VDI

For this reason, the implementation logic for SMAPIv3 preparation is also shorter,
as the complexity is now handled by the storage layer, which is where it is supposed
to be handled.

### Establishing mirror

The other significant difference is that the storage backend for SMAPIv3 `qemu-dp` 
SRs no longer accepts fds, so xapi needs to proxy the data between two nbd client
and nbd server.

SMAPIv3 provides the `Data.mirror uri domain remote` which needs three parameters: 
`uri` for accessing the local disk, `doamin` for the domain slice on which mirroring 
should happen, and most importantly for this design, a `remote` url which represents 
the remote nbd server to which the blocks of data can be sent to. 

This function itself, when called by xapi and forwarded to the storage layer's qemu-dp
nbd client, will initiate a nbd connection to the nbd server pointed to by `remote`.
This works fine when the storage migration happens entirely within a local host, 
where qemu-dp's nbd client and nbd server can communicate over unix domain sockets. 
However, it does not work for inter-host migrations as qemu-dp's nbd server is not 
exposed publicly over the network (just as tapdisk's nbd server). Therefore a proxying
service on the source host is needed for forwarding the nbd connection from the 
source host to the destination host. And it would be the responsiblity of
xapi to manage this proxy service.

The following diagram illustrates the mirroring process of a single VDI:

![sxm mirror](sxm-mirror-v3.svg)

The first step for xapi is then to set up a nbd proxy thread that will be listening 
on a local unix domain socket with path `/var/run/nbdproxy/export/<domain>` where 
domain is the `domain` parameter mentioned above in `Data.mirror`. The nbd proxy 
thread will accept nbd connections (or rather any connections, it does not 
speak/care about nbd protocol at all) and sends an https put request 
to the remote xapi. The proxy itself will then forward the data exactly as it is 
to the remote side through the https connection.

Once the proxy is set up, xapi will call `Data.mirror`, which 
will be forwarded to the xapi-storage-script and is further forwarded to the qemu-dp. 
This call contains, among other parameters, the destination NBD server url (`remote`)
to be connected. In this case the destination nbd server is exactly the domain
socket to which the proxy thread is listening. Therefore the `remote` parameter
will be of the form `nbd+unix:///<export>?socket=<socket>` where the export is provided
by the destination nbd server that represents the VDI prepared on the destination 
host, and the socket will be the path of the unix domain socket where the proxy 
thread (which we just created) is listening at.

When this connection is set up, the proxy process will talk to the remote xapi via
https requests, and on the remote side, an https handler will proxy this request to
the appropriate nbd server of either tapdisk or qemu-dp, using exactly the same
[import proxy](#copy-and-compose) as mentioned before.

Note that this proxying service is tightly integrated with outbound SXM of SMAPIv3
SRs. This is to make it simple to focus on the migration itself.

Although there is no need to explicitly copy the VDI anymore, we still need to
transfer the data and wait for it finish. For this we use `Data.stat` call provided
by the storage backend to query the status of the mirror, and wait for it to finish
as needed.

#### Limitations

This way of establishing the connection simplifies the implementation of the migration
for SMAPIv3, but it also has limitations:

One proxy per live VDI migration is needed, which can potentially consume lots of resources in dom0, and we should measure the impact of this before we switch to using more resource-efficient ways such as wire guard that allows establishing a single connection between multiple hosts.


### Finish

As there is no need to copy a VDI, there is also no need to compose or delete the
snapshot. The cleanup procedure would therefore just involve destroy the datapath
that was used for receiving writes for the mirrored VDI.

## Error Handling

Storage migration is a long-running process, and is prone to failures in each
step. Hence it is important specifying what errors could be raised at each step
and their significance. This is beneficial both for the user and for triaging.

There are two general cleanup functions in SXM: `MIRROR.receive_cancel` and
`MIRROR.stop`. The former is for cleaning up whatever has been created by `MIRROR.receive_start`
on the destination host (such as VDIs for receiving mirrored data). The latter is 
a more comprehensive function that attempts to "undo" all the side effects that
was done during the SXM, and also calls `receive_cancel` as part of its operations.

Currently error handling was done by building up a list of cleanup functions in
the `on_fail` list ref as the function executes. For example, if the `receive_start`
has been completed successfully, add `receive_cancel` to the list of cleanup functions.
And whenever an exception is encountered, just execute whatever has been added
to the `on_fail` list ref. This is convenient, but does entangle all the error
handling logic with the core SXM logic itself, making the code rather than hard
to understand and maintain.

The idea to fix this is to introduce explicit "stages" during the SXM and define
explicitly what error handling should be done if it fails at a certain stage. This
helps separate the error handling logic into the `with` part of a `try with` block,
which is where they are supposed to be. Since we need to accommodate the existing
SMAPIv1 migration (which has more stages than SMAPIv3), the following stages are
introduced: preparation (v1,v3), snapshot(v1), mirror(v1, v3), copy(v1). Note that
each stage also roughly corresponds to a helper function that is called within `Storage_migrate.start`,
which is the wrapper function that initiates storage migration. And each helper
functions themselves would also have error handling logic within themselves as
needed (e.g. see `Storage_smapiv1_migrate.receive_start`) to deal with exceptions 
that happen within each helper functions.

### Preparation (SMAPIv1 and SMAPIv3)

The preparation stage generally corresponds to what is done in `receive_start`, and
this function itself will handle exceptions when there are partial failures within
the function itself, such as an exception after the receiving VDI is created.
It will use the old-style `on_fail` function but only with a limited scope.

There is nothing to be done at a higher level (i.e within `MIRROR.start` which
calls `receive_start`) if preparation has failed.

### Snapshot and mirror failure (SMAPIv1)

For SMAPIv1, the mirror is done in a bit cumbersome way. The end goal is to establish
connections between two tapdisk processes on the source and destination hosts.
To achieve this goal, xapi will do two main jobs: 1. create a connection between two
hosts and pass the connection to tapdisk; 2. create a snapshot as a starting point
of the mirroring process. 

Therefore handling of failures at these two stages are similar: clean up what was
done in the preparation stage by calling `receive_cancel`, and that is almost it. 
Again, we will leave whatever is needed for partial failure handling within those 
functions themselves and only clean up at a stage-level in `storage_migrate.ml`

Note that `receive_cancel` is a multiplexed function for SMAPIv1 and SMAPIv3, which
means different clean up logic will be executed depending on what type of SR we
are migrating from.

### Mirror failure (SMAPIv3)

The `Data.stat` call in SMAPIv3 returns a data structure that includes the current
progress of the mirror job, whether it has completed syncing the existing data and
whether the mirorr has failed. Similar to how it is done in SMAPIv1, we wait for
the sync to complete once we issue the `Data.mirror` call, by repeatedly polling
the status of the mirror using the `Data.stat` call. During this process, the status
of the mirror is also checked and if a failure is detected, a `Migration_mirror_failure`
will be raised and then gets handled by the code in `storage_migrate.ml` by calling
`Storage_smapiv3_migrate.receive_cancel2`, which will clean up the mirror datapath
and destroy the mirror VDI, similar to what is done in SMAPIv1.


### Copy failure (SMAPIv1)

The final step of storage migration for SMAPIv1 is to copy the snapshot from the
source to the destination. At this stage, most of the side effectful work has been
done, so we do need to call `MIRROR.stop` to clean things up if we experience an
failure during copying.


## SMAPIv1 Migration implementation detail

{{% notice info %}}
The following doc refers to the xapi a [version](https://github.com/xapi-project/xen-api/blob/v24.37.0/ocaml/xapi/storage_migrate.ml) 
of xapi that is before 24.37 after which point this code structure has undergone
many changes as part of adding support for SMAPIv3 SXM. Therefore the following
tutorial might be less relevant in terms of the implementation detail. Although
the general principle should remain the same.
{{% /notice %}}

```mermaid
sequenceDiagram
participant local_tapdisk as local tapdisk
participant local_smapiv2 as local SMAPIv2
participant xapi
participant remote_xapi as remote xapi
participant remote_smapiv2 as remote SMAPIv2 (might redirect)
participant remote_tapdisk as remote tapdisk

Note over xapi: Sort VDIs increasingly by size and then age

loop VM's & snapshots' VDIs & suspend images
  xapi->>remote_xapi: plug dest SR to dest host and pool master
  alt VDI is not mirrored
    Note over xapi: We don't mirror RO VDIs & VDIs of snapshots
    xapi->>local_smapiv2: DATA.copy remote_sm_url

    activate local_smapiv2
    local_smapiv2-->>local_smapiv2: SR.scan
    local_smapiv2-->>local_smapiv2: VDI.similar_content
    local_smapiv2-->>remote_smapiv2: SR.scan
    Note over local_smapiv2: Find nearest smaller remote VDI remote_base, if any
    alt remote_base
      local_smapiv2-->>remote_smapiv2: VDI.clone
      local_smapiv2-->>remote_smapiv2: VDI.resize
    else no remote_base
      local_smapiv2-->>remote_smapiv2: VDI.create
    end

    Note over local_smapiv2: call copy'
    activate local_smapiv2
    local_smapiv2-->>remote_smapiv2: SR.list
    local_smapiv2-->>remote_smapiv2: SR.scan
    Note over local_smapiv2: create new datapaths remote_dp, base_dp, leaf_dp
    Note over local_smapiv2: find local base_vdi with same content_id as dest, if any
    local_smapiv2-->>remote_smapiv2: VDI.attach2 remote_dp dest
    local_smapiv2-->>remote_smapiv2: VDI.activate remote_dp dest
    opt base_vdi
      local_smapiv2-->>local_smapiv2: VDI.attach2 base_dp base_vdi
      local_smapiv2-->>local_smapiv2: VDI.activate base_dp base_vdi
    end
    local_smapiv2-->>local_smapiv2: VDI.attach2 leaf_dp vdi
    local_smapiv2-->>local_smapiv2: VDI.activate leaf_dp vdi
    local_smapiv2-->>remote_xapi: sparse_dd base_vdi vdi dest [NBD URI for dest & remote_dp]
    Note over remote_xapi: HTTP handler verifies credentials
    remote_xapi-->>remote_tapdisk: then passes connection to tapdisk's NBD server
    local_smapiv2-->>local_smapiv2: VDI.deactivate leaf_dp vdi
    local_smapiv2-->>local_smapiv2: VDI.detach leaf_dp vdi
    opt base_vdi
      local_smapiv2-->>local_smapiv2: VDI.deactivate base_dp base_vdi
      local_smapiv2-->>local_smapiv2: VDI.detach base_dp base_vdi
    end
    local_smapiv2-->>remote_smapiv2: DP.destroy remote_dp
    deactivate local_smapiv2

    local_smapiv2-->>remote_smapiv2: VDI.snapshot remote_copy
    local_smapiv2-->>remote_smapiv2: VDI.destroy remote_copy
    local_smapiv2->>xapi: task(snapshot)
    deactivate local_smapiv2

  else VDI is mirrored
    Note over xapi: We mirror RW VDIs of the VM
    Note over xapi: create new datapath dp
    xapi->>local_smapiv2: VDI.attach2 dp
    xapi->>local_smapiv2: VDI.activate dp
    xapi->>local_smapiv2: DATA.MIRROR.start dp remote_sm_url

    activate local_smapiv2
    Note over local_smapiv2: copy disk data & mirror local writes
    local_smapiv2-->>local_smapiv2: SR.scan
    local_smapiv2-->>local_smapiv2: VDI.similar_content
    local_smapiv2-->>remote_smapiv2: DATA.MIRROR.receive_start similars
    activate remote_smapiv2
    remote_smapiv2-->>local_smapiv2: mirror_vdi,mirror_dp,copy_diffs_from,copy_diffs_to,dummy_vdi
    deactivate remote_smapiv2
    local_smapiv2-->>local_smapiv2: DP.attach_info dp
    local_smapiv2-->>remote_xapi: connect to [NBD URI for mirror_vdi & mirror_dp]
    Note over remote_xapi: HTTP handler verifies credentials
    remote_xapi-->>remote_tapdisk: then passes connection to tapdisk's NBD server
    local_smapiv2-->>local_tapdisk: pass socket & dp to tapdisk of dp
    local_smapiv2-->>local_smapiv2: VDI.snapshot local_vdi [mirror:dp]
    local_smapiv2-->>local_tapdisk: [Python] unpause disk, pass dp
    local_tapdisk-->>remote_tapdisk: mirror new writes via NBD to socket
    Note over local_smapiv2: call copy' snapshot copy_diffs_to
    local_smapiv2-->>remote_smapiv2: VDI.compose copy_diffs_to mirror_vdi
    local_smapiv2-->>remote_smapiv2: VDI.remove_from_sm_config mirror_vdi base_mirror
    local_smapiv2-->>remote_smapiv2: VDI.destroy dummy_vdi
    local_smapiv2-->>local_smapiv2: VDI.destroy snapshot
    local_smapiv2->>xapi: task(mirror ID)
    deactivate local_smapiv2

    xapi->>local_smapiv2: DATA.MIRROR.stat
    activate local_smapiv2
    local_smapiv2->>xapi: dest_vdi
    deactivate local_smapiv2
  end

  loop until task finished
    xapi->>local_smapiv2: UPDATES.get
    xapi->>local_smapiv2: TASK.stat
  end
  xapi->>local_smapiv2: TASK.stat
  xapi->>local_smapiv2: TASK.destroy
end
opt for snapshot VDIs
  xapi->>local_smapiv2: SR.update_snapshot_info_src remote_sm_url
  activate local_smapiv2
  local_smapiv2-->>remote_smapiv2: SR.update_snapshot_info_dest
  deactivate local_smapiv2
end
Note over xapi: ...
Note over xapi: reserve resources for the new VM in dest host
loop all VDIs
  opt VDI is mirrored
    xapi->>local_smapiv2: DP.destroy dp
  end
end
opt post_detach_hook
  opt active local mirror
    local_smapiv2-->>remote_smapiv2: DATA.MIRROR.receive_finalize [mirror ID]
    Note over remote_smapiv2: destroy mirror dp
  end
end
Note over xapi: memory image migration by xenopsd
Note over xapi: destroy the VM record
```

### Receiving SXM

These are the remote calls in the above diagram sent from the remote host to
the receiving end of storage motion:

* Remote SMAPIv2 -> local SMAPIv2 RPC calls:
  * `SR.list`
  * `SR.scan`
  * `SR.update_snapshot_info_dest`
  * `VDI.attach2`
  * `VDI.activate`
  * `VDI.snapshot`
  * `VDI.destroy`
  * For copying:
      * For copying from base:
          * `VDI.clone`
          * `VDI.resize`
      * For copying without base:
          * `VDI.create`
  * For mirroring:
      * `DATA.MIRROR.receive_start`
      * `VDI.compose`
      * `VDI.remove_from_sm_config`
      * `DATA.MIRROR.receive_finalize`
* HTTP requests to xapi:
  * Connecting to NBD URI via xapi's HTTP handler

---

This is how xapi coordinates storage migration. We'll do it as a code walkthrough through the two layers: xapi and storage-in-xapi (SMAPIv2).

### Xapi code

The entry point is in [xapi_vm_migration.ml](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/xapi_vm_migrate.ml#L786)

The function takes several arguments:

* a vm reference (`vm`)
* a dictionary of `(string * string)` key-value pairs about the destination (`dest)`. This is the result of a previous call to the destination pool, `Host.migrate_receive`
* `live`, a boolean of whether we should live-migrate or suspend-resume,
* `vdi_map`, a mapping of VDI references to destination SR references,
* `vif_map`, a mapping of VIF references to destination network references,
* `vgpu_map`, similar for VGPUs
* `options`, another dictionary of options

```ocaml
let migrate_send'  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~vgpu_map ~options =
  SMPERF.debug "vm.migrate_send called vm:%s" (Db.VM.get_uuid ~__context ~self:vm);

  let open Xapi_xenops in

  let localhost = Helpers.get_localhost ~__context in
  let remote = remote_of_dest dest in

  (* Copy mode means we don't destroy the VM on the source host. We also don't
     	   copy over the RRDs/messages *)
  let copy = try bool_of_string (List.assoc "copy" options) with _ -> false in
```

It begins by getting the local host reference, deciding whether we're copying or moving, and converting the input `dest` parameter from an untyped string association list to a typed record, `remote`, which is declared further up the file:

```ocaml
type remote = {
  rpc : Rpc.call -> Rpc.response;
  session : API.ref_session;
  sm_url : string;
  xenops_url : string;
  master_url : string;
  remote_ip : string; (* IP address *)
  remote_master_ip : string; (* IP address *)
  dest_host : API.ref_host;
}
```

this contains:

* A function, `rpc`, for calling XenAPI RPCs on the destination
* A `session` valid on the destination
* A `sm_url` on which SMAPIv2 APIs can be called on the destination
* A `master_url` on which XenAPI commands can be called (not currently used)
* The IP address, `remote_ip`, of the destination host
* The IP address, `remote_master_ip`, of the master of the destination pool

Next, we determine which VDIs to copy:

```ocaml
  (* The first thing to do is to create mirrors of all the disks on the remote.
     We look through the VM's VBDs and all of those of the snapshots. We then
     compile a list of all of the associated VDIs, whether we mirror them or not
     (mirroring means we believe the VDI to be active and new writes should be
     mirrored to the destination - otherwise we just copy it)
     We look at the VDIs of the VM, the VDIs of all of the snapshots, and any
     suspend-image VDIs. *)

  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vifs = Db.VM.get_VIFs ~__context ~self:vm in
  let snapshots = Db.VM.get_snapshots ~__context ~self:vm in
  let vm_and_snapshots = vm :: snapshots in
  let snapshots_vbds = List.concat_map (fun self -> Db.VM.get_VBDs ~__context ~self) snapshots in
  let snapshot_vifs = List.concat_map (fun self -> Db.VM.get_VIFs ~__context ~self) snapshots in
```

we now decide whether we're intra-pool or not, and if we're intra-pool whether we're migrating onto the same host (localhost migrate). Intra-pool is decided by trying to do a lookup of our current host uuid on the destination pool.

```ocaml
  let is_intra_pool = try ignore(Db.Host.get_uuid ~__context ~self:remote.dest_host); true with _ -> false in
  let is_same_host = is_intra_pool && remote.dest_host == localhost in

  if copy && is_intra_pool then raise (Api_errors.Server_error(Api_errors.operation_not_allowed, [ "Copy mode is disallowed on intra pool storage migration, try efficient alternatives e.g. VM.copy/clone."]));
```

Having got all of the VBDs of the VM, we now need to find the associated VDIs, filtering out empty CDs, and decide whether we're going to copy them or mirror them - read-only VDIs can be copied but RW VDIs must be mirrored.

```ocaml
  let vms_vdis = List.filter_map (vdi_filter __context true) vbds in
```

where `vdi_filter` is defined earler:

```ocaml
(* We ignore empty or CD VBDs - nothing to do there. Possible redundancy here:
   I don't think any VBDs other than CD VBDs can be 'empty' *)
let vdi_filter __context allow_mirror vbd =
  if Db.VBD.get_empty ~__context ~self:vbd || Db.VBD.get_type ~__context ~self:vbd = `CD
  then None
  else
    let do_mirror = allow_mirror && (Db.VBD.get_mode ~__context ~self:vbd = `RW) in
    let vm = Db.VBD.get_VM ~__context ~self:vbd in
    let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
    Some (get_vdi_mirror __context vm vdi do_mirror)
```

This in turn calls `get_vdi_mirror` which gathers together some important info:

```ocaml
let get_vdi_mirror __context vm vdi do_mirror =
  let snapshot_of = Db.VDI.get_snapshot_of ~__context ~self:vdi in
  let size = Db.VDI.get_virtual_size ~__context ~self:vdi in
  let xenops_locator = Xapi_xenops.xenops_vdi_locator ~__context ~self:vdi in
  let location = Db.VDI.get_location ~__context ~self:vdi in
  let dp = Storage_access.presentative_datapath_of_vbd ~__context ~vm ~vdi in
  let sr = Db.SR.get_uuid ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi) in
  {vdi; dp; location; sr; xenops_locator; size; snapshot_of; do_mirror}
```

The record is helpfully commented above:

```ocaml
type vdi_mirror = {
  vdi : [ `VDI ] API.Ref.t;           (* The API reference of the local VDI *)
  dp : string;                        (* The datapath the VDI will be using if the VM is running *)
  location : string;                  (* The location of the VDI in the current SR *)
  sr : string;                        (* The VDI's current SR uuid *)
  xenops_locator : string;            (* The 'locator' xenops uses to refer to the VDI on the current host *)
  size : Int64.t;                     (* Size of the VDI *)
  snapshot_of : [ `VDI ] API.Ref.t;   (* API's snapshot_of reference *)
  do_mirror : bool;                   (* Whether we should mirror or just copy the VDI *)
}
```

`xenops_locator` is `<sr uuid>/<vdi uuid>`, and `dp` is `vbd/<domid>/<device>` if the VM is running and `vbd/<vm_uuid>/<vdi_uuid>` if not.

So now we have a list of these records for all VDIs attached to the VM. For these we check explicitly that they're all defined in the `vdi_map`, the mapping of VDI references to their destination SR references.

```ocaml
  check_vdi_map ~__context vms_vdis vdi_map;
```

We then figure out the VIF map:

```ocaml
 let vif_map =
    if is_intra_pool then vif_map
    else infer_vif_map ~__context (vifs @ snapshot_vifs) vif_map
  in
```

More sanity checks: We can't do a storage migration if any of the VDIs is a reset-on-boot one - since the state will be lost on the destination when it's attached:

```ocaml
(* Block SXM when VM has a VDI with on_boot=reset *)
  List.(iter (fun vconf ->
      let vdi = vconf.vdi in
      if (Db.VDI.get_on_boot ~__context ~self:vdi ==`reset) then
        raise (Api_errors.Server_error(Api_errors.vdi_on_boot_mode_incompatible_with_operation, [Ref.string_of vdi]))) vms_vdis) ;
```

We now consider all of the VDIs associated with the snapshots. As for the VM's VBDs above, we end up with a `vdi_mirror` list. Note we pass `false` to the `allow_mirror` parameter of the `get_vdi_mirror` function as none of these snapshot VDIs will ever require mirrorring.

```ocaml
let snapshots_vdis = List.filter_map (vdi_filter __context false)
```

Finally we get all of the suspend-image VDIs from all snapshots as well as the actual VM, since it might be suspended itself:

```ocaml
snapshots_vbds in
  let suspends_vdis =
    List.fold_left
      (fun acc vm ->
         if Db.VM.get_power_state ~__context ~self:vm = `Suspended
         then
           let vdi = Db.VM.get_suspend_VDI ~__context ~self:vm in
           let sr = Db.VDI.get_SR ~__context ~self:vdi in
           if is_intra_pool && Helpers.host_has_pbd_for_sr ~__context ~host:remote.dest_host ~sr
           then acc
           else (get_vdi_mirror __context vm vdi false):: acc
         else acc)
      [] vm_and_snapshots in
```

Sanity check that we can see all of the suspend-image VDIs on this host:

```ocaml
 (* Double check that all of the suspend VDIs are all visible on the source *)
  List.iter (fun vdi_mirror ->
      let sr = Db.VDI.get_SR ~__context ~self:vdi_mirror.vdi in
      if not (Helpers.host_has_pbd_for_sr ~__context ~host:localhost ~sr)
      then raise (Api_errors.Server_error (Api_errors.suspend_image_not_accessible, [ Ref.string_of vdi_mirror.vdi ]))) suspends_vdis;
```

Next is a fairly complex piece that determines the destination SR for all of these VDIs. We don't require API uses to decide destinations for all of the VDIs on snapshots and hence we have to make some decisions here:

```ocaml
  let dest_pool = List.hd (XenAPI.Pool.get_all remote.rpc remote.session) in
  let default_sr_ref =
    XenAPI.Pool.get_default_SR remote.rpc remote.session dest_pool in
  let suspend_sr_ref =
    let pool_suspend_SR = XenAPI.Pool.get_suspend_image_SR remote.rpc remote.session dest_pool
    and host_suspend_SR = XenAPI.Host.get_suspend_image_sr remote.rpc remote.session remote.dest_host in
    if pool_suspend_SR <> Ref.null then pool_suspend_SR else host_suspend_SR in

  (* Resolve placement of unspecified VDIs here - unspecified VDIs that
            are 'snapshot_of' a specified VDI go to the same place. suspend VDIs
            that are unspecified go to the suspend_sr_ref defined above *)

  let extra_vdis = suspends_vdis @ snapshots_vdis in

  let extra_vdi_map =
    List.map
      (fun vconf ->
         let dest_sr_ref =
           let is_mapped = List.mem_assoc vconf.vdi vdi_map
           and snapshot_of_is_mapped = List.mem_assoc vconf.snapshot_of vdi_map
           and is_suspend_vdi = List.mem vconf suspends_vdis
           and remote_has_suspend_sr = suspend_sr_ref <> Ref.null
           and remote_has_default_sr = default_sr_ref <> Ref.null in
           let log_prefix =
             Printf.sprintf "Resolving VDI->SR map for VDI %s:" (Db.VDI.get_uuid ~__context ~self:vconf.vdi) in
           if is_mapped then begin
             debug "%s VDI has been specified in the map" log_prefix;
             List.assoc vconf.vdi vdi_map
           end else if snapshot_of_is_mapped then begin
             debug "%s Snapshot VDI has entry in map for it's snapshot_of link" log_prefix;
             List.assoc vconf.snapshot_of vdi_map
           end else if is_suspend_vdi && remote_has_suspend_sr then begin
             debug "%s Mapping suspend VDI to remote suspend SR" log_prefix;
             suspend_sr_ref
           end else if is_suspend_vdi && remote_has_default_sr then begin
             debug "%s Remote suspend SR not set, mapping suspend VDI to remote default SR" log_prefix;
             default_sr_ref
           end else if remote_has_default_sr then begin
             debug "%s Mapping unspecified VDI to remote default SR" log_prefix;
             default_sr_ref
           end else begin
             error "%s VDI not in VDI->SR map and no remote default SR is set" log_prefix;
             raise (Api_errors.Server_error(Api_errors.vdi_not_in_map, [ Ref.string_of vconf.vdi ]))
           end in
         (vconf.vdi, dest_sr_ref))
      extra_vdis in
```

At the end of this we've got all of the VDIs that need to be copied and destinations for all of them:

```ocaml
  let vdi_map = vdi_map @ extra_vdi_map in
  let all_vdis = vms_vdis @ extra_vdis in

  (* The vdi_map should be complete at this point - it should include all the
     VDIs in the all_vdis list. *)
```

Now we gather some final information together:

```ocaml
  assert_no_cbt_enabled_vdi_migrated ~__context ~vdi_map;

  let dbg = Context.string_of_task __context in
  let open Xapi_xenops_queue in
  let queue_name = queue_of_vm ~__context ~self:vm in
  let module XenopsAPI = (val make_client queue_name : XENOPS) in

  let remote_vdis = ref [] in

  let ha_always_run_reset = not is_intra_pool && Db.VM.get_ha_always_run ~__context ~self:vm in

  let cd_vbds = find_cds_to_eject __context vdi_map vbds in
  eject_cds __context cd_vbds;
```

check there's no CBT (we can't currently migrate the CBT metadata), make our client to talk to Xenopsd, make a mutable list of remote VDIs (which I think is redundant right now), decide whether we need to do anything for HA (we disable HA protection for this VM on the destination until it's fully migrated) and eject any CDs from the VM.

Up until now this has mostly been gathering info (aside from the ejecting CDs bit), but now we'll start to do some actions, so we begin a `try-catch` block:

```ocaml
try
```

but we've still got a bit of thinking to do: we sort the VDIs to copy based on age/size:

```ocaml
    (* Sort VDIs by size in principle and then age secondly. This gives better
       chances that similar but smaller VDIs would arrive comparatively
       earlier, which can serve as base for incremental copying the larger
       ones. *)
    let compare_fun v1 v2 =
      let r = Int64.compare v1.size v2.size in
      if r = 0 then
        let t1 = Date.to_unix_time (Db.VDI.get_snapshot_time ~__context ~self:v1.vdi) in
        let t2 = Date.to_unix_time (Db.VDI.get_snapshot_time ~__context ~self:v2.vdi) in
        compare t1 t2
      else r in
    let all_vdis = all_vdis |> List.sort compare_fun in

    let total_size = List.fold_left (fun acc vconf -> Int64.add acc vconf.size) 0L all_vdis in
    let so_far = ref 0L in
```

OK, let's copy/mirror:

```ocaml
    with_many (vdi_copy_fun __context dbg vdi_map remote is_intra_pool remote_vdis so_far total_size copy) all_vdis @@ fun all_map ->

```

The copy functions are written such that they take continuations. This it to make the error handling simpler - each individual component function can perform its setup and execute the continuation. In the event of an exception coming from the continuation it can then unroll its bit of state and rethrow the exception for the next layer to handle.

`with_many` is a simple helper function for nesting invocations of functions that take continuations. It has the delightful type:

```
('a -> ('b -> 'c) -> 'c) -> 'a list -> ('b list -> 'c) -> 'c
```

```ocaml
(* Helper function to apply a 'with_x' function to a list *)
let rec with_many withfn many fn =
  let rec inner l acc =
    match l with
    | [] -> fn acc
    | x::xs -> withfn x (fun y -> inner xs (y::acc))
  in inner many []
```

As an example of its operation, imagine our withfn is as follows:

```ocaml
let withfn x c =
  Printf.printf "Starting withfn: x=%d\n" x;
  try
    c (string_of_int x)
  with e ->
    Printf.printf "Handling exception for x=%d\n" x;
    raise e;;
```

applying this gives the output:

```ocaml
utop # with_many withfn [1;2;3;4] (String.concat ",");;
Starting with fn: x=1
Starting with fn: x=2
Starting with fn: x=3
Starting with fn: x=4
- : string = "4,3,2,1"
```
whereas raising an exception in the continutation results in the following:

```ocaml
utop # with_many with_fn [1;2;3;4] (fun _ -> failwith "error");;
Starting with fn: x=1
Starting with fn: x=2
Starting with fn: x=3
Starting with fn: x=4
Handling exception for x=4
Handling exception for x=3
Handling exception for x=2
Handling exception for x=1
Exception: Failure "error".
```

All the real action is in `vdi_copy_fun`, which copies or mirrors a single VDI:

```ocaml
let vdi_copy_fun __context dbg vdi_map remote is_intra_pool remote_vdis so_far total_size copy vconf continuation =
  TaskHelper.exn_if_cancelling ~__context;
  let open Storage_access in
  let dest_sr_ref = List.assoc vconf.vdi vdi_map in
  let dest_sr_uuid = XenAPI.SR.get_uuid remote.rpc remote.session dest_sr_ref in

  (* Plug the destination shared SR into destination host and pool master if unplugged.
     Plug the local SR into destination host only if unplugged *)
  let dest_pool = List.hd (XenAPI.Pool.get_all remote.rpc remote.session) in
  let master_host = XenAPI.Pool.get_master remote.rpc remote.session dest_pool in
  let pbds = XenAPI.SR.get_PBDs remote.rpc remote.session dest_sr_ref in
  let pbd_host_pair = List.map (fun pbd -> (pbd, XenAPI.PBD.get_host remote.rpc remote.session pbd)) pbds in
  let hosts_to_be_attached = [master_host; remote.dest_host] in
  let pbds_to_be_plugged = List.filter (fun (_, host) ->
      (List.mem host hosts_to_be_attached) && (XenAPI.Host.get_enabled remote.rpc remote.session host)) pbd_host_pair in
  List.iter (fun (pbd, _) ->
      if not (XenAPI.PBD.get_currently_attached remote.rpc remote.session pbd) then
        XenAPI.PBD.plug remote.rpc remote.session pbd) pbds_to_be_plugged;
```

It begins by attempting to ensure the SRs we require are definitely attached on the destination host and on the destination pool master.

There's now a little logic to support the case where we have cross-pool SRs and the VDI is already visible to the destination pool. Since this is outside our normal support envelope there is a key in xapi_globs that has to be set (via xapi.conf) to enable this:

```ocaml
  let rec dest_vdi_exists_on_sr vdi_uuid sr_ref retry =
    try
      let dest_vdi_ref = XenAPI.VDI.get_by_uuid remote.rpc remote.session vdi_uuid in
      let dest_vdi_sr_ref = XenAPI.VDI.get_SR remote.rpc remote.session dest_vdi_ref in
      if dest_vdi_sr_ref = sr_ref then
        true
      else
        false
    with _ ->
      if retry then
        begin
          XenAPI.SR.scan remote.rpc remote.session sr_ref;
          dest_vdi_exists_on_sr vdi_uuid sr_ref false
        end
      else
        false
  in

  (* CP-4498 added an unsupported mode to use cross-pool shared SRs - the initial
     use case is for a shared raw iSCSI SR (same uuid, same VDI uuid) *)
  let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vconf.vdi in
  let mirror = if !Xapi_globs.relax_xsm_sr_check then
      if (dest_sr_uuid = vconf.sr) then
        begin
          (* Check if the VDI uuid already exists in the target SR *)
          if (dest_vdi_exists_on_sr vdi_uuid dest_sr_ref true) then
            false
          else
            failwith ("SR UUID matches on destination but VDI does not exist")
        end
      else
        true
    else
      (not is_intra_pool) || (dest_sr_uuid <> vconf.sr)
  in
```

The check also covers the case where we're doing an intra-pool migration and not copying all of the disks, in which case we don't need to do anything for that disk.

We now have a wrapper function that creates a new datapath and passes it to a continuation function. On error it handles the destruction of the datapath:

```ocaml
let with_new_dp cont =
    let dp = Printf.sprintf (if vconf.do_mirror then "mirror_%s" else "copy_%s") vconf.dp in
    try cont dp
    with e ->
      (try SMAPI.DP.destroy ~dbg ~dp ~allow_leak:false with _ -> info "Failed to cleanup datapath: %s" dp);
      raise e in
```

and now a helper that, given a remote VDI uuid, looks up the reference on the remote host and gives it to a continuation function. On failure of the continuation it will destroy the remote VDI:

```ocaml
  let with_remote_vdi remote_vdi cont =
    debug "Executing remote scan to ensure VDI is known to xapi";
    XenAPI.SR.scan remote.rpc remote.session dest_sr_ref;
    let query = Printf.sprintf "(field \"location\"=\"%s\") and (field \"SR\"=\"%s\")" remote_vdi (Ref.string_of dest_sr_ref) in
    let vdis = XenAPI.VDI.get_all_records_where remote.rpc remote.session query in
    let remote_vdi_ref = match vdis with
      | [] -> raise (Api_errors.Server_error(Api_errors.vdi_location_missing, [Ref.string_of dest_sr_ref; remote_vdi]))
      | h :: [] -> debug "Found remote vdi reference: %s" (Ref.string_of (fst h)); fst h
      | _ -> raise (Api_errors.Server_error(Api_errors.location_not_unique, [Ref.string_of dest_sr_ref; remote_vdi])) in
    try cont remote_vdi_ref
    with e ->
      (try XenAPI.VDI.destroy remote.rpc remote.session remote_vdi_ref with _ -> error "Failed to destroy remote VDI");
      raise e in
```

another helper to gather together info about a mirrored VDI:

```ocaml
let get_mirror_record ?new_dp remote_vdi remote_vdi_reference =
    { mr_dp = new_dp;
      mr_mirrored = mirror;
      mr_local_sr = vconf.sr;
      mr_local_vdi = vconf.location;
      mr_remote_sr = dest_sr_uuid;
      mr_remote_vdi = remote_vdi;
      mr_local_xenops_locator = vconf.xenops_locator;
      mr_remote_xenops_locator = Xapi_xenops.xenops_vdi_locator_of_strings dest_sr_uuid remote_vdi;
      mr_local_vdi_reference = vconf.vdi;
      mr_remote_vdi_reference = remote_vdi_reference } in
```

and finally the really important function:

```ocaml
let mirror_to_remote new_dp =
    let task =
      if not vconf.do_mirror then
        SMAPI.DATA.copy ~dbg ~sr:vconf.sr ~vdi:vconf.location ~dp:new_dp ~url:remote.sm_url ~dest:dest_sr_uuid
      else begin
        (* Though we have no intention of "write", here we use the same mode as the
           associated VBD on a mirrored VDIs (i.e. always RW). This avoids problem
           when we need to start/stop the VM along the migration. *)
        let read_write = true in
        (* DP set up is only essential for MIRROR.start/stop due to their open ended pattern.
           It's not necessary for copy which will take care of that itself. *)
        ignore(SMAPI.VDI.attach ~dbg ~dp:new_dp ~sr:vconf.sr ~vdi:vconf.location ~read_write);
        SMAPI.VDI.activate ~dbg ~dp:new_dp ~sr:vconf.sr ~vdi:vconf.location;
        ignore(Storage_access.register_mirror __context vconf.location);
        SMAPI.DATA.MIRROR.start ~dbg ~sr:vconf.sr ~vdi:vconf.location ~dp:new_dp ~url:remote.sm_url ~dest:dest_sr_uuid
      end in

    let mapfn x =
      let total = Int64.to_float total_size in
      let done_ = Int64.to_float !so_far /. total in
      let remaining = Int64.to_float vconf.size /. total in
      done_ +. x *. remaining in

    let open Storage_access in

    let task_result =
      task |> register_task __context
      |> add_to_progress_map mapfn
      |> wait_for_task dbg
      |> remove_from_progress_map
      |> unregister_task __context
      |> success_task dbg in

    let mirror_id, remote_vdi =
      if not vconf.do_mirror then
        let vdi = task_result |> vdi_of_task dbg in
        remote_vdis := vdi.vdi :: !remote_vdis;
        None, vdi.vdi
      else
        let mirrorid = task_result |> mirror_of_task dbg in
        let m = SMAPI.DATA.MIRROR.stat ~dbg ~id:mirrorid in
        Some mirrorid, m.Mirror.dest_vdi in

    so_far := Int64.add !so_far vconf.size;
    debug "Local VDI %s %s to %s" vconf.location (if vconf.do_mirror then "mirrored" else "copied") remote_vdi;
    mirror_id, remote_vdi in
```

This is the bit that actually starts the mirroring or copying. Before the call to mirror we call `VDI.attach` and `VDI.activate` locally to ensure that if the VM is shutdown then the detach/deactivate there doesn't kill the mirroring process.

Note the parameters to the SMAPI call are `sr` and `vdi`, locating the local VDI and SM backend, `new_dp`, the datapath we're using for the mirroring, `url`, which is the remote url on which SMAPI calls work, and `dest`, the destination SR uuid. These are also the arguments to `copy` above too.

There's a little function to calculate the overall progress of the task, and the function waits until the completion of the task before it continues. The function `success_task` will raise an exception if the task failed. For `DATA.mirror`, completion implies both that the disk data has been copied to the destination and that all local writes are being mirrored to the destination. Hence more cleanup must be done on cancellation. In contrast, if the `DATA.copy` path had been taken then the operation at this point has completely finished.

The result of this function is an optional mirror id and the remote VDI uuid.

Next, there is a `post_mirror` function:

```ocaml
  let post_mirror mirror_id mirror_record =
    try
      let result = continuation mirror_record in
      (match mirror_id with
       | Some mid -> ignore(Storage_access.unregister_mirror mid);
       | None -> ());
      if mirror && not (Xapi_fist.storage_motion_keep_vdi () || copy) then
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            XenAPI.VDI.destroy rpc session_id vconf.vdi);
      result
    with e ->
      let mirror_failed =
        match mirror_id with
        | Some mid ->
          ignore(Storage_access.unregister_mirror mid);
          let m = SMAPI.DATA.MIRROR.stat ~dbg ~id:mid in
          (try SMAPI.DATA.MIRROR.stop ~dbg ~id:mid with _ -> ());
          m.Mirror.failed
        | None -> false in
      if mirror_failed then raise (Api_errors.Server_error(Api_errors.mirror_failed,[Ref.string_of vconf.vdi]))
      else raise e in
```

This is poorly named - it is post mirror _and_ copy. The aim of this function is to destroy the source VDIs on successful completion of the continuation function, which will have migrated the VM to the destination. In its exception handler it will stop the mirroring, but before doing so it will check to see if the mirroring process it was looking after has itself failed, and raise `mirror_failed` if so. This is because a failed mirror can result in a range of actual errors, and we decide here that the failed mirror was probably the root cause.

These functions are assembled together at the end of the `vdi_copy_fun` function:

```
   if mirror then
    with_new_dp (fun new_dp ->
        let mirror_id, remote_vdi = mirror_to_remote new_dp in
        with_remote_vdi remote_vdi (fun remote_vdi_ref ->
            let mirror_record = get_mirror_record ~new_dp remote_vdi remote_vdi_ref in
            post_mirror mirror_id mirror_record))
  else
    let mirror_record = get_mirror_record vconf.location (XenAPI.VDI.get_by_uuid remote.rpc remote.session vdi_uuid) in
    continuation mirror_record
```
again, `mirror` here is poorly named, and means mirror _or_ copy.

Once all of the disks have been mirrored or copied, we jump back to the body of `migrate_send`. We split apart the mirror records according to the source of the VDI:

```ocaml
      let was_from vmap = List.exists (fun vconf -> vconf.vdi = vmap.mr_local_vdi_reference) in

      let suspends_map, snapshots_map, vdi_map = List.fold_left (fun (suspends, snapshots, vdis) vmap ->
          if was_from vmap suspends_vdis then  vmap :: suspends, snapshots, vdis
          else if was_from vmap snapshots_vdis then suspends, vmap :: snapshots, vdis
          else suspends, snapshots, vmap :: vdis
        ) ([],[],[]) all_map in
```

then we reassemble all_map from this, for some reason:

```ocaml
    let all_map = List.concat [suspends_map; snapshots_map; vdi_map] in
```

Now we need to update the snapshot-of links:

```ocaml
     (* All the disks and snapshots have been created in the remote SR(s),
       * so update the snapshot links if there are any snapshots. *)
      if snapshots_map <> [] then
        update_snapshot_info ~__context ~dbg ~url:remote.sm_url ~vdi_map ~snapshots_map;
```

I'm not entirely sure why this is done in this layer as opposed to in the storage layer.

A little housekeeping:

```ocaml
     let xenops_vdi_map = List.map (fun mirror_record -> (mirror_record.mr_local_xenops_locator, mirror_record.mr_remote_xenops_locator)) all_map in

      (* Wait for delay fist to disappear *)
      wait_for_fist __context Xapi_fist.pause_storage_migrate "pause_storage_migrate";

      TaskHelper.exn_if_cancelling ~__context;
```

the `fist` thing here simply allows tests to put in a delay at this specific point.

We also check the task to see if we've been cancelled and raise an exception if so.

The VM metadata is now imported into the remote pool, with all the XenAPI level objects remapped:

```ocaml
let new_vm =
        if is_intra_pool
        then vm
        else
          (* Make sure HA replaning cycle won't occur right during the import process or immediately after *)
          let () = if ha_always_run_reset then XenAPI.Pool.ha_prevent_restarts_for ~rpc:remote.rpc ~session_id:remote.session ~seconds:(Int64.of_float !Xapi_globs.ha_monitor_interval) in
          (* Move the xapi VM metadata to the remote pool. *)
          let vms =
            let vdi_map =
              List.map (fun mirror_record -> {
                    local_vdi_reference = mirror_record.mr_local_vdi_reference;
                    remote_vdi_reference = Some mirror_record.mr_remote_vdi_reference;
                  })
                all_map in
            let vif_map =
              List.map (fun (vif, network) -> {
                    local_vif_reference = vif;
                    remote_network_reference = network;
                  })
                vif_map in
            let vgpu_map =
              List.map (fun (vgpu, gpu_group) -> {
                    local_vgpu_reference = vgpu;
                    remote_gpu_group_reference = gpu_group;
                  })
                vgpu_map
            in
            inter_pool_metadata_transfer ~__context ~remote ~vm ~vdi_map
              ~vif_map ~vgpu_map ~dry_run:false ~live:true ~copy
          in
          let vm = List.hd vms in
          let () = if ha_always_run_reset then XenAPI.VM.set_ha_always_run ~rpc:remote.rpc ~session_id:remote.session ~self:vm ~value:false in
          (* Reserve resources for the new VM on the destination pool's host *)
          let () = XenAPI.Host.allocate_resources_for_vm remote.rpc remote.session remote.dest_host vm true in
          vm in
```

More waiting for fist points:

```
     wait_for_fist __context Xapi_fist.pause_storage_migrate2 "pause_storage_migrate2";

      (* Attach networks on remote *)
      XenAPI.Network.attach_for_vm ~rpc:remote.rpc ~session_id:remote.session ~host:remote.dest_host ~vm:new_vm;
```

also make sure all the networks are plugged for the VM on the destination.
Next we create the xenopsd-level vif map, equivalent to the vdi_map above:

```ocaml
  (* Create the vif-map for xenops, linking VIF devices to bridge names on the remote *)
      let xenops_vif_map =
        let vifs = XenAPI.VM.get_VIFs ~rpc:remote.rpc ~session_id:remote.session ~self:new_vm in
        List.map (fun vif ->
            let vifr = XenAPI.VIF.get_record ~rpc:remote.rpc ~session_id:remote.session ~self:vif in
            let bridge = Xenops_interface.Network.Local
                (XenAPI.Network.get_bridge ~rpc:remote.rpc ~session_id:remote.session ~self:vifr.API.vIF_network) in
            vifr.API.vIF_device, bridge
          ) vifs
      in
```

Now we destroy any extra mirror datapaths we set up previously:

```ocaml
     (* Destroy the local datapaths - this allows the VDIs to properly detach, invoking the migrate_finalize calls *)
      List.iter (fun mirror_record ->
          if mirror_record.mr_mirrored
          then match mirror_record.mr_dp with | Some dp ->  SMAPI.DP.destroy ~dbg ~dp ~allow_leak:false | None -> ()) all_map;
```

More housekeeping:

```ocaml
    SMPERF.debug "vm.migrate_send: migration initiated vm:%s" vm_uuid;

      (* In case when we do SXM on the same host (mostly likely a VDI
         migration), the VM's metadata in xenopsd will be in-place updated
         as soon as the domain migration starts. For these case, there
         will be no (clean) way back from this point. So we disable task
         cancellation for them here.
       *)
      if is_same_host then (TaskHelper.exn_if_cancelling ~__context; TaskHelper.set_not_cancellable ~__context);

```

Finally we get to the memory-image part of the migration:

```ocaml
      (* It's acceptable for the VM not to exist at this point; shutdown commutes with storage migrate *)
      begin
        try
          Xapi_xenops.Events_from_xenopsd.with_suppressed queue_name dbg vm_uuid
            (fun () ->
               let xenops_vgpu_map = (* can raise VGPU_mapping *)
                 infer_vgpu_map ~__context ~remote new_vm in
               migrate_with_retry
                 ~__context queue_name dbg vm_uuid xenops_vdi_map
                 xenops_vif_map xenops_vgpu_map remote.xenops_url;
               Xapi_xenops.Xenopsd_metadata.delete ~__context vm_uuid)
        with
        | Xenops_interface.Does_not_exist ("VM",_)
        | Xenops_interface.Does_not_exist ("extra",_) ->
          info "%s: VM %s stopped being live during migration"
            "vm_migrate_send" vm_uuid
        | VGPU_mapping(msg) ->
          info "%s: VM %s - can't infer vGPU map: %s"
            "vm_migrate_send" vm_uuid msg;
          raise Api_errors.
                  (Server_error
                     (vm_migrate_failed,
                      ([ vm_uuid
                       ; Helpers.get_localhost_uuid ()
                       ; Db.Host.get_uuid ~__context ~self:remote.dest_host
                       ; "The VM changed its power state during migration"
                       ])))
      end;

      debug "Migration complete";
      SMPERF.debug "vm.migrate_send: migration complete vm:%s" vm_uuid;
```

Now we tidy up after ourselves:

```ocaml
      (* So far the main body of migration is completed, and the rests are
         updates, config or cleanup on the source and destination. There will
         be no (clean) way back from this point, due to these destructive
         changes, so we don't want user intervention e.g. task cancellation.
       *)
      TaskHelper.exn_if_cancelling ~__context;
      TaskHelper.set_not_cancellable ~__context;
      XenAPI.VM.pool_migrate_complete remote.rpc remote.session new_vm remote.dest_host;

      detach_local_network_for_vm ~__context ~vm ~destination:remote.dest_host;
      Xapi_xenops.refresh_vm ~__context ~self:vm;
```

the function `pool_migrate_complete` is called on the destination host, and consists of a few things that ordinarily would be set up during VM.start or the like:

```ocaml
let pool_migrate_complete ~__context ~vm ~host =
  let id = Db.VM.get_uuid ~__context ~self:vm in
  debug "VM.pool_migrate_complete %s" id;
  let dbg = Context.string_of_task __context in
  let queue_name = Xapi_xenops_queue.queue_of_vm ~__context ~self:vm in
  if Xapi_xenops.vm_exists_in_xenopsd queue_name dbg id then begin
    Cpuid_helpers.update_cpu_flags ~__context ~vm ~host;
    Xapi_xenops.set_resident_on ~__context ~self:vm;
    Xapi_xenops.add_caches id;
    Xapi_xenops.refresh_vm ~__context ~self:vm;
    Monitor_dbcalls_cache.clear_cache_for_vm ~vm_uuid:id
  end
```

More tidying up, remapping some remaining VBDs and clearing state on the sender:

```ocaml
      (* Those disks that were attached at the point the migration happened will have been
         remapped by the Events_from_xenopsd logic. We need to remap any other disks at
         this point here *)

      if is_intra_pool
      then
        List.iter
          (fun vm' ->
             intra_pool_vdi_remap ~__context vm' all_map;
             intra_pool_fix_suspend_sr ~__context remote.dest_host vm')
          vm_and_snapshots;

      (* If it's an inter-pool migrate, the VBDs will still be 'currently-attached=true'
         because we supressed the events coming from xenopsd. Destroy them, so that the
         VDIs can be destroyed *)
      if not is_intra_pool && not copy
      then List.iter (fun vbd -> Db.VBD.destroy ~__context ~self:vbd) (vbds @ snapshots_vbds);

      new_vm
    in
```

The remark about the `Events_from_xenopsd` is that we have a thread watching for events that are emitted by xenopsd, and we resynchronise xapi's state according to xenopsd's state for several fields for which xenopsd is considered the canonical source of truth. One of these is the exact VDI the VBD is associated with.

The suspend_SR field of the VM is set to the source's value, so we reset that.

Now we move the RRDs:

```ocaml
  if not copy then begin
      Rrdd_proxy.migrate_rrd ~__context ~remote_address:remote.remote_ip ~session_id:(Ref.string_of remote.session)
        ~vm_uuid:vm_uuid ~host_uuid:(Ref.string_of remote.dest_host) ()
    end;
```

This can be done for intra- and inter- pool migrates in the same way, simplifying the logic.

However, for messages and blobs we have to only migrate them for inter-pool migrations:

```ocaml
   if not is_intra_pool && not copy then begin
      (* Replicate HA runtime flag if necessary *)
      if ha_always_run_reset then XenAPI.VM.set_ha_always_run ~rpc:remote.rpc ~session_id:remote.session ~self:new_vm ~value:true;
      (* Send non-database metadata *)
      Xapi_message.send_messages ~__context ~cls:`VM ~obj_uuid:vm_uuid
        ~session_id:remote.session ~remote_address:remote.remote_master_ip;
      Xapi_blob.migrate_push ~__context ~rpc:remote.rpc
        ~remote_address:remote.remote_master_ip ~session_id:remote.session ~old_vm:vm ~new_vm ;
      (* Signal the remote pool that we're done *)
    end;
```

Lastly, we destroy the VM record on the source:

```ocaml
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        if not is_intra_pool && not copy then begin
          info "Destroying VM ref=%s uuid=%s" (Ref.string_of vm) vm_uuid;
          Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted;
          List.iter (fun self -> Db.VM.destroy ~__context ~self) vm_and_snapshots
        end);
    SMPERF.debug "vm.migrate_send exiting vm:%s" vm_uuid;
    new_vm
```

The exception handler still has to clean some state, but mostly things are handled in the CPS functions declared above:

```ocaml
with e ->
    error "Caught %s: cleaning up" (Printexc.to_string e);

    (* We do our best to tidy up the state left behind *)
    Events_from_xenopsd.with_suppressed queue_name dbg vm_uuid (fun () ->
        try
          let _, state = XenopsAPI.VM.stat dbg vm_uuid in
          if Xenops_interface.(state.Vm.power_state = Suspended) then begin
            debug "xenops: %s: shutting down suspended VM" vm_uuid;
            Xapi_xenops.shutdown ~__context ~self:vm None;
          end;
        with _ -> ());

    if not is_intra_pool && Db.is_valid_ref __context vm then begin
      List.map (fun self -> Db.VM.get_uuid ~__context ~self) vm_and_snapshots
      |> List.iter (fun self ->
          try
            let vm_ref = XenAPI.VM.get_by_uuid remote.rpc remote.session self in
            info "Destroying stale VM uuid=%s on destination host" self;
            XenAPI.VM.destroy remote.rpc remote.session vm_ref
          with e -> error "Caught %s while destroying VM uuid=%s on destination host" (Printexc.to_string e) self)
    end;

    let task = Context.get_task_id __context in
    let oc = Db.Task.get_other_config ~__context ~self:task in
    if List.mem_assoc "mirror_failed" oc then begin
      let failed_vdi = List.assoc "mirror_failed" oc in
      let vconf = List.find (fun vconf -> vconf.location=failed_vdi) vms_vdis in
      debug "Mirror failed for VDI: %s" failed_vdi;
      raise (Api_errors.Server_error(Api_errors.mirror_failed,[Ref.string_of vconf.vdi]))
    end;
    TaskHelper.exn_if_cancelling ~__context;
    begin match e with
      | Storage_interface.Backend_error(code, params) -> raise (Api_errors.Server_error(code, params))
      | Storage_interface.Unimplemented(code) -> raise (Api_errors.Server_error(Api_errors.unimplemented_in_sm_backend, [code]))
      | Xenops_interface.Cancelled _ -> TaskHelper.raise_cancelled ~__context
      | _ -> raise e
    end
```

Failures during the migration can result in the VM being in a suspended state. There's no point leaving it like this since there's nothing that can be done to resume it, so we force shut it down.

We also try to remove the VM record from the destination if we managed to send it there.

Finally we check for mirror failure in the task - this is set by the events thread watching for events from the storage layer, in [storage_access.ml](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_access.ml#L1169-L1207)


### Storage code

The part of the code that is conceptually in the storage layer, but physically in xapi, is located in
[storage_migrate.ml](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_migrate.ml). There are logically a few separate parts to this file:

* A [stateful module](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_migrate.ml#L34-L204) for persisting state across xapi restarts.
* Some general [helper functions](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_migrate.ml#L206-L281)
* Some quite specific [helper](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_migrate.ml#L206-L281) [functions](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_migrate.ml#L738-L791) related to actions to be taken on deactivate/detach
* An [NBD handler](https://github.com/xapi-project/xen-api/blob/f75d51e7a3eff89d952330ec1a739df85a2895e2/ocaml/xapi/storage_migrate.ml#L793-L818)
* The implementations of the SMAPIv2 [mirroring APIs](https://github.com/xapi-project/xcp-idl/blob/master/storage/storage_interface.ml#L430-L460)

Let's start by considering the way the storage APIs are intended to be used.

#### Copying a VDI

`DATA.copy` takes several parameters:

* `dbg` - a debug string
* `sr` - the source SR (a uuid)
* `vdi` - the source VDI (a uuid)
* `dp` - **unused**
* `url` - a URL on which SMAPIv2 API calls can be made
* `sr` - the destination SR in which the VDI should be copied

and returns a parameter of type `Task.id`. The API call is intended to be called in an asynchronous fashion - ie., the caller makes the call, receives the task ID back and polls or uses the event mechanism to wait until the task has completed. The task may be cancelled via the `Task.cancel` API call. The result of the operation is obtained by calling TASK.stat, which returns a record:

```ocaml
	type t = {
		id: id;
		dbg: string;
		ctime: float;
		state: state;
		subtasks: (string * state) list;
		debug_info: (string * string) list;
		backtrace: string;
	}
```

Where the `state` field contains the result once the task has completed:

```ocaml
type async_result_t =
	| Vdi_info of vdi_info
	| Mirror_id of Mirror.id

type completion_t = {
	duration : float;
	result : async_result_t option
}

type state =
	| Pending of float
	| Completed of completion_t
	| Failed of Rpc.t
```

Once the result has been obtained from the task, the task should be destroyed via the `TASK.destroy` API call.

The implementation uses the `url` parameter to make SMAPIv2 calls to the destination SR. This is used, for example, to invoke a VDI.create call if necessary. The URL contains an authentication token within it (valid for the duration of the XenAPI call that caused this DATA.copy API call).

The implementation tries to minimize the amount of data copied by looking for related VDIs on the destination SR. See below for more details.


#### Mirroring a VDI

`DATA.MIRROR.start` takes a similar set of parameters to that of copy:

* `dbg` - a debug string
* `sr` - the source SR (a uuid)
* `vdi` - the source VDI (a uuid)
* `dp` - the datapath on which the VDI has been attached
* `url` - a URL on which SMAPIv2 API calls can be made
* `sr` - the destination SR in which the VDI should be copied

Similar to copy above, this returns a task id. The task 'completes' once the mirror has been set up - that is, at any point afterwards we can detach the disk and the destination disk will be identical to the source. Unlike for copy the operation is ongoing after the API call completes, since new writes need to be mirrored to the destination. Therefore the completion type of the mirror operation is `Mirror_id` which contains a handle on which further API calls related to the mirror call can be made. For example [MIRROR.stat](https://github.com/xapi-project/xcp-idl/blob/a999ef6191629c8f68377f7c412ee98fc6a39dea/storage/storage_interface.ml#L446) whose signature is:

```ocaml
MIRROR.stat: dbg:debug_info -> id:Mirror.id -> Mirror.t
```

The return type of this call is a record containing information about the mirror:

```ocaml
type state =
	| Receiving
	| Sending
	| Copying

type t = {
	source_vdi : vdi;
	dest_vdi : vdi;
	state : state list;
	failed : bool;
}
```

Note that state is a list since the initial phase of the operation requires both copying and mirroring.

Additionally the mirror can be cancelled using the `MIRROR.stop` API call.

#### Code walkthrough

let's go through the implementation of `copy`:

##### DATA.copy

```ocaml
let copy ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
  debug "copy sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
  let remote_url = Http.Url.of_string url in
  let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
```

Here we are constructing a module `Remote` on which we can do SMAPIv2 calls directly on the destination.

```ocaml
  try
```

Wrap the whole function in an exception handler.

```ocaml
    (* Find the local VDI *)
    let vdis = Local.SR.scan ~dbg ~sr in
    let local_vdi =
      try List.find (fun x -> x.vdi = vdi) vdis
      with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
```

We first find the metadata for our source VDI by doing a local SMAPIv2 call `SR.scan`. This returns a list of VDI metadata, out of which we extract the VDI we're interested in.

```ocaml
    try
```

Another exception handler. This looks redundant to me right now.

```
      let similar_vdis = Local.VDI.similar_content ~dbg ~sr ~vdi in
      let similars = List.map (fun vdi -> vdi.content_id) similar_vdis in
      debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) similar_vdis));
```

Here we look for related VDIs locally using the `VDI.similar_content` SMAPIv2 API call. This searches for related VDIs and returns an ordered list where the most similar is first in the list. It returns both clones and snapshots, and hence is more general than simply following `snapshot_of` links.

```
      let remote_vdis = Remote.SR.scan ~dbg ~sr:dest in
      (** We drop cbt_metadata VDIs that do not have any actual data *)
      let remote_vdis = List.filter (fun vdi -> vdi.ty <> "cbt_metadata") remote_vdis in

      let nearest = List.fold_left
          (fun acc content_id -> match acc with
             | Some x -> acc
             | None ->
               try Some (List.find (fun vdi -> vdi.content_id = content_id && vdi.virtual_size <= local_vdi.virtual_size) remote_vdis)
               with Not_found -> None) None similars in

      debug "Nearest VDI: content_id=%s vdi=%s"
        (Opt.default "None" (Opt.map (fun x -> x.content_id) nearest))
        (Opt.default "None" (Opt.map (fun x -> x.vdi) nearest));
```

Here we look for VDIs on the destination with the same `content_id` as one of the locally similar VDIs. We will use this as a base image and only copy deltas to the destination. This is done by cloning the VDI on the destination and then using `sparse_dd` to find the deltas from our local disk to our local copy of the content_id disk and streaming these to the destination. Note that we need to ensure the VDI is smaller than the one we want to copy since we can't resize disks downwards in size.

```ocaml
      let remote_base = match nearest with
        | Some vdi ->
          debug "Cloning VDI %s" vdi.vdi;
          let vdi_clone = Remote.VDI.clone ~dbg ~sr:dest ~vdi_info:vdi in
          if vdi_clone.virtual_size <> local_vdi.virtual_size then begin
            let new_size = Remote.VDI.resize ~dbg ~sr:dest ~vdi:vdi_clone.vdi ~new_size:local_vdi.virtual_size in
            debug "Resize remote VDI %s to %Ld: result %Ld" vdi_clone.vdi local_vdi.virtual_size new_size;
          end;
          vdi_clone
        | None ->
          debug "Creating a blank remote VDI";
          Remote.VDI.create ~dbg ~sr:dest ~vdi_info:{ local_vdi with sm_config = [] }  in
```

If we've found a base VDI we clone it and resize it immediately. If there's nothing on the destination already we can use, we just create a new VDI. Note that the calls to create and clone may well fail if the destination host is not the SRmaster. This is [handled purely in the `rpc` function](https://github.com/xapi-project/xen-api/blob/master/ocaml/xapi/storage_migrate.ml#L214-L229):

```ocaml
let rec rpc ~srcstr ~dststr url call =
  let result = XMLRPC_protocol.rpc ~transport:(transport_of_url url)
      ~srcstr ~dststr ~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) ~query:(Http.Url.get_query_params url) (Http.Url.get_uri url)) call
  in
  if not result.Rpc.success then begin
    debug "Got failure: checking for redirect";
    debug "Call was: %s" (Rpc.string_of_call call);
    debug "result.contents: %s" (Jsonrpc.to_string result.Rpc.contents);
    match Storage_interface.Exception.exnty_of_rpc result.Rpc.contents with
    | Storage_interface.Exception.Redirect (Some ip) ->
      let open Http.Url in
      let newurl =
        match url with
        | (Http h, d) ->
          (Http {h with host=ip}, d)
        | _ ->
          remote_url ip in
      debug "Redirecting to ip: %s" ip;
      let r = rpc ~srcstr ~dststr newurl call in
      debug "Successfully redirected. Returning";
      r
    | _ ->
      debug "Not a redirect";
      result
  end
  else result
```

Back to the copy function:

```ocaml
      let remote_copy = copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi:remote_base.vdi |> vdi_info in
```

This calls the actual data copy part. See below for more on that.

```ocaml
      let snapshot = Remote.VDI.snapshot ~dbg ~sr:dest ~vdi_info:remote_copy in
      Remote.VDI.destroy ~dbg ~sr:dest ~vdi:remote_copy.vdi;
      Some (Vdi_info snapshot)
```

Finally we snapshot the remote VDI to ensure we've got a VDI of type 'snapshot' on the destination, and we delete the non-snapshot VDI.

```ocaml
    with e ->
      error "Caught %s: copying snapshots vdi" (Printexc.to_string e);
      raise (Internal_error (Printexc.to_string e))
  with
  | Backend_error(code, params)
  | Api_errors.Server_error(code, params) ->
    raise (Backend_error(code, params))
  | e ->
    raise (Internal_error(Printexc.to_string e))
```

The exception handler does nothing - so we leak remote VDIs if the exception happens after we've done our cloning :-(

##### DATA.copy_into

Let's now look at the data-copying part. This is common code shared between `VDI.copy`, `VDI.copy_into` and `MIRROR.start` and hence has some duplication of the calls made above.

```ocaml
let copy_into ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi =
  copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi
```

`copy_into` is a stub and just calls `copy'`

```ocaml
let copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi =
  let remote_url = Http.Url.of_string url in
  let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
  debug "copy local=%s/%s url=%s remote=%s/%s" sr vdi url dest dest_vdi;
```

This call takes roughly the same parameters as the ``DATA.copy` call above, except it specifies the destination VDI.
Once again we construct a module to do remote SMAPIv2 calls

```ocaml
  (* Check the remote SR exists *)
  let srs = Remote.SR.list ~dbg in
  if not(List.mem dest srs)
  then failwith (Printf.sprintf "Remote SR %s not found" dest);
```

Sanity check.

```ocaml
  let vdis = Remote.SR.scan ~dbg ~sr:dest in
  let remote_vdi =
    try List.find (fun x -> x.vdi = dest_vdi) vdis
    with Not_found -> failwith (Printf.sprintf "Remote VDI %s not found" dest_vdi)
  in
```

Find the metadata of the destination VDI

```ocaml
  let dest_content_id = remote_vdi.content_id in
```

If we've got a local VDI with the same content_id as the destination, we only need copy the deltas, so we make a note of the destination content ID here.

```ocaml
  (* Find the local VDI *)
  let vdis = Local.SR.scan ~dbg ~sr in
  let local_vdi =
    try List.find (fun x -> x.vdi = vdi) vdis
    with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

  debug "copy local=%s/%s content_id=%s" sr vdi local_vdi.content_id;
  debug "copy remote=%s/%s content_id=%s" dest dest_vdi remote_vdi.content_id;
```

Find the source VDI metadata.

```
  if local_vdi.virtual_size > remote_vdi.virtual_size then begin
    (* This should never happen provided the higher-level logic is working properly *)
    error "copy local=%s/%s virtual_size=%Ld > remote=%s/%s virtual_size = %Ld" sr vdi local_vdi.virtual_size dest dest_vdi remote_vdi.virtual_size;
    failwith "local VDI is larger than the remote VDI";
  end;
```

Sanity check - the remote VDI can't be smaller than the source.

```ocaml
  let on_fail : (unit -> unit) list ref = ref [] in
```

We do some ugly error handling here by keeping a mutable list of operations to perform in the event of a failure.

```ocaml
  let base_vdi =
    try
      let x = (List.find (fun x -> x.content_id = dest_content_id) vdis).vdi in
      debug "local VDI %s has content_id = %s; we will perform an incremental copy" x dest_content_id;
      Some x
    with _ ->
      debug "no local VDI has content_id = %s; we will perform a full copy" dest_content_id;
      None
  in
```

See if we can identify a local VDI with the same `content_id` as the destination. If not, no problem.

```ocaml
  try
    let remote_dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    let base_dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    let leaf_dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
```

Construct some `datapaths` - named reasons why the VDI is attached - that we will pass to `VDI.attach/activate`.

```ocaml
    let dest_vdi_url = Http.Url.set_uri remote_url (Printf.sprintf "%s/nbd/%s/%s/%s" (Http.Url.get_uri remote_url) dest dest_vdi remote_dp) |> Http.Url.to_string in

    debug "copy remote=%s/%s NBD URL = %s" dest dest_vdi dest_vdi_url;
```

Here we are constructing a URI that we use to connect to the destination xapi. The handler for this particular path will verify the credentials and then pass the connection on to tapdisk which will behave as a NBD server. The VDI has to be attached and activated for this to work, unlike the new NBD handler in `xapi-nbd` that is smarter. The handler for this URI is declared [in this file](https://github.com/xapi-project/xen-api/blob/master/ocaml/xapi/storage_migrate.ml#L858-L884)

```ocaml
    let id=State.copy_id_of (sr,vdi) in
    debug "Persisting state for copy (id=%s)" id;
    State.add id State.(Copy_op Copy_state.({
        base_dp; leaf_dp; remote_dp; dest_sr=dest; copy_vdi=remote_vdi.vdi; remote_url=url}));
```

Since we're about to perform a long-running operation that is stateful, we persist the state here so that if xapi is restarted we can cancel the operation and not leak VDI attaches. Normally in xapi code we would be doing VBD.plug operations to persist the state in the xapi db, but this is storage code so we have to use a different mechanism.

```ocaml
    SMPERF.debug "mirror.copy: copy initiated local_vdi:%s dest_vdi:%s" vdi dest_vdi;

    Pervasiveext.finally (fun () ->
        debug "activating RW datapath %s on remote=%s/%s" remote_dp dest dest_vdi;
        ignore(Remote.VDI.attach ~dbg ~sr:dest ~vdi:dest_vdi ~dp:remote_dp ~read_write:true);
        Remote.VDI.activate ~dbg ~dp:remote_dp ~sr:dest ~vdi:dest_vdi;

        with_activated_disk ~dbg ~sr ~vdi:base_vdi ~dp:base_dp
          (fun base_path ->
             with_activated_disk ~dbg ~sr ~vdi:(Some vdi) ~dp:leaf_dp
               (fun src ->
                  let dd = Sparse_dd_wrapper.start ~progress_cb:(progress_callback 0.05 0.9 task) ?base:base_path true (Opt.unbox src)
                      dest_vdi_url remote_vdi.virtual_size in
                  Storage_task.with_cancel task
                    (fun () -> Sparse_dd_wrapper.cancel dd)
                    (fun () ->
                       try Sparse_dd_wrapper.wait dd
                       with Sparse_dd_wrapper.Cancelled -> Storage_task.raise_cancelled task)
               )
          );
      )
      (fun () ->
         Remote.DP.destroy ~dbg ~dp:remote_dp ~allow_leak:false;
         State.remove_copy id
      );
```

In this chunk of code we attach and activate the disk on the remote SR via the SMAPI, then locally attach and activate both the VDI we're copying and the base image we're copying deltas from (if we've got one). We then call `sparse_dd` to copy the data to the remote NBD URL. There is some logic to update progress indicators and to cancel the operation if the SMAPIv2 call `TASK.cancel` is called.

Once the operation has terminated (either on success, error or cancellation), we remove the local attach and activations in the `with_activated_disk` function and the remote attach and activation by destroying the datapath on the remote SR. We then remove the persistent state relating to the copy.

```ocaml
    SMPERF.debug "mirror.copy: copy complete local_vdi:%s dest_vdi:%s" vdi dest_vdi;

    debug "setting remote=%s/%s content_id <- %s" dest dest_vdi local_vdi.content_id;
    Remote.VDI.set_content_id ~dbg ~sr:dest ~vdi:dest_vdi ~content_id:local_vdi.content_id;
    (* PR-1255: XXX: this is useful because we don't have content_ids by default *)
    debug "setting local=%s/%s content_id <- %s" sr local_vdi.vdi local_vdi.content_id;
    Local.VDI.set_content_id ~dbg ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id;
    Some (Vdi_info remote_vdi)
```

The last thing we do is to set the local and remote content_id. The local set_content_id is there because the content_id of the VDI is constructed from the location if it is unset in the [storage_access.ml](https://github.com/xapi-project/xen-api/blob/3bf897b3accfc172f365689c3c6927746e059177/ocaml/xapi/storage_access.ml#L69-L72) module of xapi (still part of the storage layer)


```ocaml
  with e ->
    error "Caught %s: performing cleanup actions" (Printexc.to_string e);
    perform_cleanup_actions !on_fail;
    raise e
```

Here we perform the list of cleanup operations. Theoretically. It seems we don't ever actually set this to anything, so this is dead code.


##### DATA.MIRROR.start

```ocaml
let start' ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
  debug "Mirror.start sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
  SMPERF.debug "mirror.start called sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
  let remote_url = Http.Url.of_string url in
  let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in

  (* Find the local VDI *)
  let vdis = Local.SR.scan ~dbg ~sr in
  let local_vdi =
    try List.find (fun x -> x.vdi = vdi) vdis
    with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
```

As with the previous calls, we make a remote module for SMAPIv2 calls on the destination, and we find local VDI metadata via `SR.scan`

```ocaml
  let id = State.mirror_id_of (sr,local_vdi.vdi) in
```

Mirror ids are deterministically constructed.

```ocaml
  (* A list of cleanup actions to perform if the operation should fail. *)
  let on_fail : (unit -> unit) list ref = ref [] in
```

This `on_fail` list is actually used.

```ocaml
  try
    let similar_vdis = Local.VDI.similar_content ~dbg ~sr ~vdi in
    let similars = List.filter (fun x -> x <> "") (List.map (fun vdi -> vdi.content_id) similar_vdis) in
    debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) similar_vdis));
```

As with copy we look locally for similar VDIs. However, rather than use that here we actually pass this information on to the destination SR via the `receive_start` internal SMAPIv2 call:

```ocaml
    let result_ty = Remote.DATA.MIRROR.receive_start ~dbg ~sr:dest ~vdi_info:local_vdi ~id ~similar:similars in
    let result = match result_ty with
        Mirror.Vhd_mirror x -> x
    in
```

This gives the destination SR a chance to say what sort of migration it can support. We only support `Vhd_mirror` style migrations which require the destination to support the `compose` SMAPIv2 operation. The type of `x` is a record:

```ocaml
type mirror_receive_result_vhd_t = {
	mirror_vdi : vdi_info;
	mirror_datapath : dp;
	copy_diffs_from : content_id option;
	copy_diffs_to : vdi;
	dummy_vdi : vdi;
}
```
Field descriptions:

* `mirror_vdi` is the VDI to which new writes should be mirrored.
* `mirror_datapath` is the remote datapath on which the VDI has been attached and activated. This is required to construct the remote NBD url
* `copy_diffs_from` represents the source base VDI to be used for the non-mirrored data copy.
* `copy_diffs_to` is the remote VDI to copy those diffs to
* `dummy_vdi` exists to prevent leaf-coalesce on the `mirror_vdi`

```ocaml
    (* Enable mirroring on the local machine *)
    let mirror_dp = result.Mirror.mirror_datapath in

    let uri = (Printf.sprintf "/services/SM/nbd/%s/%s/%s" dest result.Mirror.mirror_vdi.vdi mirror_dp) in
    let dest_url = Http.Url.set_uri remote_url uri in
    let request = Http.Request.make ~query:(Http.Url.get_query_params dest_url) ~version:"1.0" ~user_agent:"smapiv2" Http.Put uri in
    let transport = Xmlrpc_client.transport_of_url dest_url in
```
This is where we connect to the NBD server on the destination.


```ocaml
    debug "Searching for data path: %s" dp;
    let attach_info = Local.DP.attach_info ~dbg:"nbd" ~sr ~vdi ~dp in
    debug "Got it!";
```

we need the local `attach_info` to find the local tapdisk so we can send it the connected NBD socket.

```ocaml
    on_fail := (fun () -> Remote.DATA.MIRROR.receive_cancel ~dbg ~id) :: !on_fail;
```

This should probably be set directly after the call to `receive_start`

```ocaml
    let tapdev = match tapdisk_of_attach_info attach_info with
      | Some tapdev ->
        debug "Got tapdev";
        let pid = Tapctl.get_tapdisk_pid tapdev in
        let path = Printf.sprintf "/var/run/blktap-control/nbdclient%d" pid in
        with_transport transport (with_http request (fun (response, s) ->
            debug "Here inside the with_transport";
            let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
            finally
              (fun () ->
                 debug "Connecting to path: %s" path;
                 Unix.connect control_fd (Unix.ADDR_UNIX path);
                 let msg = dp in
                 let len = String.length msg in
                 let written = Unixext.send_fd control_fd msg 0 len [] s in
                 debug "Sent fd";
                 if written <> len then begin
                   error "Failed to transfer fd to %s" path;
                   failwith "foo"
                 end)
              (fun () ->
                 Unix.close control_fd)));
        tapdev
      | None ->
        failwith "Not attached"
    in
```
Here we connect to the remote NBD server, then pass that connected fd to the local tapdisk that is using the disk. This fd is passed with a name that is later used to tell tapdisk to start using it - we use the datapath name for this.

```ocaml
    debug "Adding to active local mirrors: id=%s" id;
    let alm = State.Send_state.({
        url;
        dest_sr=dest;
        remote_dp=mirror_dp;
        local_dp=dp;
        mirror_vdi=result.Mirror.mirror_vdi.vdi;
        remote_url=url;
        tapdev;
        failed=false;
        watchdog=None}) in
    State.add id (State.Send_op alm);
    debug "Added";
```

As for copy we persist some state to disk to say that we're doing a mirror so we can undo any state changes after a toolstack restart.

```ocaml
    debug "About to snapshot VDI = %s" (string_of_vdi_info local_vdi);
    let local_vdi = add_to_sm_config local_vdi "mirror" ("nbd:" ^ dp) in
    let local_vdi = add_to_sm_config local_vdi "base_mirror" id in
    let snapshot =
    try
      Local.VDI.snapshot ~dbg ~sr ~vdi_info:local_vdi
    with
    | Storage_interface.Backend_error(code, _) when code = "SR_BACKEND_FAILURE_44" ->
      raise (Api_errors.Server_error(Api_errors.sr_source_space_insufficient, [ sr ]))
    | e ->
      raise e
    in
    debug "Done!";

    SMPERF.debug "mirror.start: snapshot created, mirror initiated vdi:%s snapshot_of:%s"
      snapshot.vdi local_vdi.vdi ;

    on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi) :: !on_fail;
```

This bit inserts into `sm_config` the name of the fd we passed earlier to do mirroring. This is interpreted by the python SM backends and passed on the `tap-ctl` invocation to unpause the disk. This causes all new writes to be mirrored via NBD to the file descriptor passed earlier.


```ocaml
    begin
      let rec inner () =
        debug "tapdisk watchdog";
        let alm_opt = State.find_active_local_mirror id in
        match alm_opt with
        | Some alm ->
          let stats = Tapctl.stats (Tapctl.create ()) tapdev in
          if stats.Tapctl.Stats.nbd_mirror_failed = 1 then
            Updates.add (Dynamic.Mirror id) updates;
          alm.State.Send_state.watchdog <- Some (Scheduler.one_shot scheduler (Scheduler.Delta 5) "tapdisk_watchdog" inner)
        | None -> ()
      in inner ()
    end;
```

This is the watchdog that runs `tap-ctl stats` every 5 seconds watching `mirror_failed` for evidence of a failure in the mirroring code. If it detects one the only thing it does is to notify that the state of the mirroring has changed. This will be picked up by the thread in xapi that is monitoring the state of the mirror. It will then issue a `MIRROR.stat` call which will return the state of the mirror including the information that it has failed.

```ocaml
    on_fail := (fun () -> stop ~dbg ~id) :: !on_fail;
    (* Copy the snapshot to the remote *)
    let new_parent = Storage_task.with_subtask task "copy" (fun () ->
        copy' ~task ~dbg ~sr ~vdi:snapshot.vdi ~url ~dest ~dest_vdi:result.Mirror.copy_diffs_to) |> vdi_info in
    debug "Local VDI %s == remote VDI %s" snapshot.vdi new_parent.vdi;
```

This is where we copy the VDI returned by the snapshot invocation to the remote VDI called `copy_diffs_to`. We only copy deltas, but we rely on `copy'` to figure out which disk the deltas should be taken from, which it does via the `content_id` field.

```ocaml
    Remote.VDI.compose ~dbg ~sr:dest ~vdi1:result.Mirror.copy_diffs_to ~vdi2:result.Mirror.mirror_vdi.vdi;
    Remote.VDI.remove_from_sm_config ~dbg ~sr:dest ~vdi:result.Mirror.mirror_vdi.vdi ~key:"base_mirror";
    debug "Local VDI %s now mirrored to remote VDI: %s" local_vdi.vdi result.Mirror.mirror_vdi.vdi;
```

Once the copy has finished we invoke the `compose` SMAPIv2 call that composes the diffs from the mirror with the base image copied from the snapshot.

```ocaml
    debug "Destroying dummy VDI %s on remote" result.Mirror.dummy_vdi;
    Remote.VDI.destroy ~dbg ~sr:dest ~vdi:result.Mirror.dummy_vdi;
    debug "Destroying snapshot %s on src" snapshot.vdi;
    Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi;

    Some (Mirror_id id)
```

we can now destroy the dummy vdi on the remote (which will cause a leaf-coalesce in due course), and we destroy the local snapshot here (which will also cause a leaf-coalesce in due course, providing we don't destroy it first). The return value from the function is the mirror_id that we can use to monitor the state or cancel the mirror.

```ocaml
  with
  | Sr_not_attached(sr_uuid) ->
    error " Caught exception %s:%s. Performing cleanup." Api_errors.sr_not_attached sr_uuid;
    perform_cleanup_actions !on_fail;
    raise (Api_errors.Server_error(Api_errors.sr_not_attached,[sr_uuid]))
  | e ->
    error "Caught %s: performing cleanup actions" (Api_errors.to_string e);
    perform_cleanup_actions !on_fail;
    raise e
```

The exception handler just cleans up afterwards.

This is not the end of the story, since we need to detach the remote datapath being used for mirroring when we detach this end. The hook function is in [storage_migrate.ml](https://github.com/xapi-project/xen-api/blob/master/ocaml/xapi/storage_migrate.ml#L775-L791):

```ocaml
let post_detach_hook ~sr ~vdi ~dp =
  let open State.Send_state in
  let id = State.mirror_id_of (sr,vdi) in
  State.find_active_local_mirror id |>
  Opt.iter (fun r ->
      let remote_url = Http.Url.of_string r.url in
      let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
      let t = Thread.create (fun () ->
          debug "Calling receive_finalize";
          log_and_ignore_exn
            (fun () -> Remote.DATA.MIRROR.receive_finalize ~dbg:"Mirror-cleanup" ~id);
          debug "Finished calling receive_finalize";
          State.remove_local_mirror id;
          debug "Removed active local mirror: %s" id
        ) () in
      Opt.iter (fun id -> Scheduler.cancel scheduler id) r.watchdog;
      debug "Created thread %d to call receive finalize and dp destroy" (Thread.id t))
```

This removes the persistent state and calls `receive_finalize` on the destination. The body of that functions is:

```ocaml
let receive_finalize ~dbg ~id =
  let recv_state = State.find_active_receive_mirror id in
  let open State.Receive_state in Opt.iter (fun r -> Local.DP.destroy ~dbg ~dp:r.leaf_dp ~allow_leak:false) recv_state;
  State.remove_receive_mirror id
```

which removes the persistent state on the destination and destroys the datapath associated with the mirror.

Additionally, there is also a pre-deactivate hook. The rationale for this is that we want to detect any failures to write that occur right at the end of the SXM process. So if there is a mirror operation going on, before we deactivate we wait for tapdisk to flush its queue of outstanding requests, then we query whether there has been a mirror failure. The code is just above the detach hook in [storage_migrate.ml](https://github.com/xapi-project/xen-api/blob/master/ocaml/xapi/storage_migrate.ml#L738-L773):

```ocaml
let pre_deactivate_hook ~dbg ~dp ~sr ~vdi =
  let open State.Send_state in
  let id = State.mirror_id_of (sr,vdi) in
  let start = Mtime_clock.counter () in
  let get_delta () = Mtime_clock.count start |> Mtime.Span.to_s in
  State.find_active_local_mirror id |>
  Opt.iter (fun s ->
      try
        (* We used to pause here and then check the nbd_mirror_failed key. Now, we poll
				   until the number of outstanding requests has gone to zero, then check the
				   status. This avoids confusing the backend (CA-128460) *)
        let open Tapctl in
        let ctx = create () in
        let rec wait () =
          if get_delta () > reqs_outstanding_timeout then raise Timeout;
          let st = stats ctx s.tapdev in
          if st.Stats.reqs_outstanding > 0
          then (Thread.delay 1.0; wait ())
          else st
        in
        let st = wait () in
        debug "Got final stats after waiting %f seconds" (get_delta ());
        if st.Stats.nbd_mirror_failed = 1
        then begin
          error "tapdisk reports mirroring failed";
          s.failed <- true
        end;
      with
      | Timeout ->
        error "Timeout out after %f seconds waiting for tapdisk to complete all outstanding requests" (get_delta ());
        s.failed <- true
      | e ->
        error "Caught exception while finally checking mirror state: %s"
          (Printexc.to_string e);
        s.failed <- true
    )
```

