# Making the disk smaller

A qcow-formatted disk allocates blocks on demand and the file grows as more
blocks are allocated. A block may be marked as free by a "TRIM" or "discard"
operation. To cause the file to shrink we use a concurrent GC, described in
this document.

## Why is this so complicated?

The simplest possible implementation of "TRIM" or "discard" is to call a
filesystem API to "punch" holes out of the file. This is possible on BSDs and
Linux but not on macOS. On macOS we must physically shuffle the blocks away
from the end to make the file smaller. The implementation is further complicated
by the need to avoid calling `fflush` too often as it's very slow and hurts
performance.

## General approach

We keep track of the set of unused ("discarded") "clusters" (clusters are blocks
in a qcow2 file) and the references from one cluster to another. We have 2
significant pieces of code:

- the block allocator: previously this was a pointer to the next cluster and
  always extended the file. Now this allocates from a free list of blocks which
  have been erased and flushed.
- the block GC: this is completely new and is responsible for maintaining a
  reasonably-sized free list of blocks and performing compaction of the disk
  by moving clusters from the end of the file to holes nearer the beginning.

When there are new "discarded" clusters (called "junk" clusters in the code)
we first top up the free list used by the block allocator, before using the
rest to compact the file. This is because

- if we're going to overwrite a block as part of a move, it's pointless to
  first erase and flush it
- if we're compacting the file and the file is extended because the free list is
  empty then the allocation will be from the end and the new cluster will need to be
  moved before we can shrink the file -- better to get the cluster placement
  right first time than to have to move it immediately.

Since `fflush` is very expensive we try to amortise the cost over many block
copies/erases. If there is outstanding unflushed work we will call `fflush`
after 5s, unless the user calls it themselves.

At all times we try to avoid blocking I/O from the client as this can lead to
timeouts (e.g. AHCI controller resets).

## States of clusters

Clusters within a file can be in any one of the following states:

- referenced and in-use
- `junk`: these have been recently discarded
- `erased`: these have been erased, but the zeroes have not been flushed which
  means it's unsafe to use them. If the computer crashes then the old data
  could re-appear. This is particularly bad for metadata blocks because we need
  them to contain zeroes (interpreted as NULL pointers i.e. unallocated clusters)
- `available`: these have been erased and flushed and are safe to reallocate
- `Copying`: these are being moved to another place on the disk
- `Copied`: these have been duplicated but not flushed
- `Flushed`: these have been duplicated and the duplicated data has been flushed.
  It is now safe to change the pointer to them.
- `Referenced`: these have been duplicated, the duplicate has been flushed and
  the pointer has been changed but this has not been flushed. The old cluster
  still cannot be reused because the pointer update might be undone by a crash;
  but at least new writes go to the new location (and would obviously be lost if
  the pointer update was undone by a crash -- but this is ok, unflushed updates
  can be lost). After the next flush the original cluster becomes `junk` and the
  new cluster becomes referenced and in-use.

## Locking

The principles are:

- clusters are updated atomically
- client I/O is not blocked on the output of the GC. The GC proceeds optimistically
  and can be rolled back

We have the following locks

- a per-cluster read/write lock: this guarantees clusters are updated atomically
  and protects access to the per-cluster move state. When a cluster is written
  to, any in-progress move can be marked as cancelled.
- a global metadata mutex: this is held when following the (cached) metadata
  pointers and prevents following a pointer and it being immediately invalidated
  by the GC
