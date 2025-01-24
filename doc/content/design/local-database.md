---
title: Local database
layout: default
design_doc: true
revision: 1
status: proposed
---

All hosts in a pool use the shared database by sending queries to
the pool master. This creates a performance bottleneck as the pool
size increases. All hosts in a pool receive a database backup from
the master periodically, every couple of hours. This creates a
reliability problem as updates may be lost if the master fails during
the window before the backup.

The reliability problem can be avoided by running with HA or the redo
log enabled, but this is not always possible.

We propose to:

- adapt the existing event machinery to allow every host to maintain
  an up-to-date database replica;
- actively cache the database locally on each host and satisfy read
  operations from the cache. Most database operations are reads so
  this should reduce the number of RPCs across the network.

In a later phase we can move to a completely
[distributed database](distributed-database/index).

Replicating the database
------------------------

We will create a database-level variant of the existing XenAPI `event.from`
API. The new RPC will block until a database event is generated, and then
the events will be returned using the existing "redo-log" event types. We
will add a few second delay into the RPC to batch the updates.

We will replace the pool database download logic with an `event.from`-like
loop which fetches all the events from the master's database and applies
them to the local copy. The first call will naturally return the full database
contents.

We will turn on the existing "in memory db cache" mechanism on all hosts,
not just the master. This will be where the database updates will go.

The result should be that every host will have a `/var/xapi/state.db` file,
with writes going to the master first and then filtering down to all slaves.

Using the replica as a cache
----------------------------

We will re-use the [Disaster Recovery](../../toolstack/features/DR) multiple
database mechanism to allow slaves to access their local database. We will
change the defalult database "context" to snapshot the local database,
perform reads locally and write-through to the master.

We will add an HTTP header to all forwarded XenAPI calls from the master which
will include the current database generation count. When a forwarded XenAPI
operation is received, the slave will deliberately wait until the local cache
is at least as new as this, so that we always use fresh metadata for XenAPI
calls (e.g. the VM.start uses the absolute latest VM memory size).

We will document the new database coherence policy, i.e. that writes on a host
will not immediately be seen by reads on another host. We believe that this
is only a problem when we are using the database for locking and are attempting
to hand over a lock to another host. We are already using XenAPI calls forwarded
to the master for some of this, but may need to do a bit more of this; in
particular the storage backends may need some updating.
