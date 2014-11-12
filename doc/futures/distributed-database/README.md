Distributed database
====================

All hosts in a pool use the shared database by sending queries to
the pool master. This creates
- a performance bottleneck as the pool size increases
- a reliability problem when the master fails.

The reliability problem can be ameliorated by running with HA enabled,
but this is not always possible.

Both problems can be addressed by observing that the database objects
correspond to distinct physical objects where eventual consistency is
perfectly ok. For example if host 'A' is running a VM and changes the
VM's name, it doesn't matter if it takes a while before the change shows
up on host 'B'. If host 'B' changes its network configuration then it
doesn't matter how long it takes host 'A' to notice. We would still like
the metadata to be replicated to cope with failure, but we can allow
changes to be committed locally and synchronised later.

Note the one exception to this pattern: the current SM plugins use database
fields to implement locks. This should be shifted to a special-purpose
lock acquire/release API.

Using git via Irmin
-------------------

A git repository is a database of key=value pairs with branching history.
If we placed our host and VM metadata in git then we could ```commit```
changes and ```pull``` and ```push``` them between replicas. The
[Irmin](https://github.com/mirage/irmin) library provides an easy programming
interface on top of git which we could link with the Xapi database layer.

Proposed new architecture
-------------------------

![Pools of one](https://xapi-project.github.io/xen-api/doc/futures/distributed-database/architecture.png)

The diagram above shows two hosts: one a master and the other a regular host.
The XenAPI client has sent a request to the wrong host; normally this would
result in a ```HOST_IS_SLAVE``` error being sent to the client. In the new
world, the host is able to process the request, only contacting the master
if it is necessary to acquire a lock. Starting a VM would require a lock; but
rebooting or migrating an existing VM would not. Assuming the lock can
be acquired, then the operation is executed locally with all state updates
being made to a git topic branch.

![Topic branches](https://xapi-project.github.io/xen-api/doc/futures/distributed-database/topic.png)

Roughly we would have 1 topic branch per
pending XenAPI Task. Once the Task completes successfully, the topic branch
(containing the new VM state) is merged back into master.
Separately each
host will pull and push updates between each other for replication.

We would avoid merge conflicts by construction; either
- a host's configuration will always be "owned" by the host and it will be
  an error for anyone else to merge updates to it
- the master's locking will guarantee that a VM is running on at most one
  host at a time. It will be an error for anyone else to merge updates to it.

What we gain
------------

We will gain the following
- the master will only be a bottleneck when the number of VM locks gets
  really large;
- you will be able to connect XenCenter to hosts without a master and manage
  them. Today such hosts are unmanageable.
- the database will have a history and you'll be able to "go back in time"
  either for debugging or to recover from mistakes
- bugs caused by concurrent threads (in separate Tasks) confusing each other
  will be vanquished. A typical failure mode is: one active thread destroys
  an object; a passive thread sees the object and then tries to read it
  and gets a database failure instead. Since every thread is operating a
  separate Task they will all have their own branch and will be isolated from
  each other.

What we lose
------------

We will lose the following
- the ability to use the Xapi database as a "lock"
- coherence between hosts: there will be no guarantee that an effect seen
  by host 'A' will be seen immediately by host 'B'. In particular this means
  that clients should send all their commands and ```event.from``` calls to
  the same host (although any host will do)


Stuff we need to build
======================

- A ```pull```/```push``` replicator: this would have to monitor the list
  of hosts in the pool and distribute updates to them in some vaguely
  efficient manner. Ideally we would avoid hassling the pool master and
  use some more efficient topology: perhaps a tree?

- A ```git diff``` to XenAPI event converter: whenever a host ```pull```s
  updates from another it needs to convert the diff into a set of touched
  objects for any ```event.from``` to read. We could send the changeset hash
  as the ```event.from``` token.

- Irmin nested views: since Tasks can be nested (and git branches can be
  nested) we need to make sure that Irmin views can be nested.

- We need to go through the xapi code and convert all mixtures of database
  access and XenAPI updates into pure database calls. With the previous system
  it was better to use a XenAPI to remote large chunks of database effects to
  the master than to perform them locally. It will now be better to run them
  all locally and merge them at the end. Additionally since a Task will have
  a local branch, it won't be possible to see the state on a remote host
  without triggering an early merge (which would harm efficiency)

- We need to create a first-class locking API to use instead of the
  ```VDI.sm_config``` locks.

Prototype
=========

A basic prototype has been created:
```
  opam pin xen-api-client git://github.com/djs55/xen-api-client#improvements
  opam pin add xapi-database git://github.com/djs55/xapi-database
  opam pin add xapi git://github.com/djs55/xen-api#schema-sexp
```
The ```xapi-database``` is clone of the existing Xapi database code
configured to run as a separate process. There is
[code to convert from XML to git](https://github.com/djs55/xapi-database/blob/master/core/db_git.ml#L55)
and
[an implementation of the Xapi remote database API](https://github.com/djs55/xapi-database/blob/master/core/db_git.ml#L186)
which uses the following layout:
```
$ git clone /xapi.db db
Cloning into 'db'...
done.

$ cd db; ls
xapi

$ ls xapi
console   host_metrics  PCI          pool     SR      user  VM
host      network       PIF          session  tables  VBD   VM_metrics
host_cpu  PBD           PIF_metrics  SM       task    VDI

$ ls xapi/pool
OpaqueRef:39adc911-0c32-9e13-91a8-43a25939110b

$ ls xapi/pool/OpaqueRef\:39adc911-0c32-9e13-91a8-43a25939110b/
crash_dump_SR                 __mtime           suspend_image_SR
__ctime                       name_description  uuid
default_SR                    name_label        vswitch_controller
ha_allow_overcommit           other_config      wlb_enabled
ha_enabled                    redo_log_enabled  wlb_password
ha_host_failures_to_tolerate  redo_log_vdi      wlb_url
ha_overcommitted              ref               wlb_username
ha_plan_exists_for            _ref              wlb_verify_cert
master                        restrictions

$ ls xapi/pool/OpaqueRef\:39adc911-0c32-9e13-91a8-43a25939110b/other_config/
cpuid_feature_mask  memory-ratio-hvm  memory-ratio-pv

$ cat xapi/pool/OpaqueRef\:39adc911-0c32-9e13-91a8-43a25939110b/other_config/cpuid_feature_mask
ffffff7f-ffffffff-ffffffff-ffffffff
```
Notice how:
- every object is a directory
- every key/value pair is represented as a file
