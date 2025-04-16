## 0.11.0 (2020-06-05)
- Update the build to use `dune` (@emillon, #112)
- Update to Mirage 4.0 interfaces (@djs55, #112)
- LICENSE.md: add title and copyright year range (@waldyrious, #109)

## 0.10.5 (2017-12-14):
- CLI: use the disk locking feature in mirage-block-unix >= 0.9.0

## 0.10.4 (2017-12-07):
- fix build on OCaml 4.06 (and -safe-string)
- update to new sha.1.10 signature
- document the prometheus support

## 0.10.3 (2017-08-02):
- avoid linking ppx tools into the library

## 0.10.2 (2017-06-18):
- remove false dependency on cmdliner

## 0.10.1 (2017-06-17):
- update to new io-page/ io-page-unix
- fix prometheus accounting error

## 0.10.0 (2017-05-13)
- fix a major performance problem with `compact`
- split into 2 packages: qcow and qcow-tool
- add `qcow-tool dehydrate` and `qcow-tool rehydrate` for extracting
  metadata for debug/support
- add prometheus metrics for I/O and GC operations
- restore the `qcow-tool compact --progress` progress bar
- add `qcow-tool compact --progress-fd` for json-formatted progress
- build via jbuilder

## 0.9.5 (2017-03-12)
- CLI: `check` and `sha` will nolonger resize the file as a side-effect
  (#84)
- Allow the number of `cluster_bits` to be set in `create`

## 0.9.4 (2017-03-07)
- Strictly enforce the cluster move state machine
- Don't start moving new blocks while existing moves are in progress
  (fix bug where the same destination block could be reused)
- Hold a lock to exclude `flush` while updating references to ensure
  reference updates hit the disk before the move is considered complete
- Simplify allocator by always adding blocks to the Roots set before
  returning. The caller must transfer them somewhere else.
- Simplify the cluster moving API by combining `get_moves` with
  `start_moves`, so it's not possible to block and affect the moves
  which can legally be started
- When detecting a duplicate reference or hitting an I/O error, log
  analysis of the internal state
- Check for move cancellation before copying a block to avoid accidentally
  copying a block which is now outside the file
- Avoid adding a cluster to the Junk set twice during a reference update
- Add lots of assertions

## 0.9.3 (2017-03-02)
- Hold a read lock on the L1 during read/write
- Minimise locking while updating references
- When moving an L2 cluster, update the cluster map

## 0.9.2 (2017-02-26)
- Don't hold the global lock while updating references
- Log an error if a client I/O takes more than 30s
- Improve the performance of discard by writing each L2 cluster to disk
  only once
- Track clusters which are being erased and copied into, to prevent the
  file being shrunk, orphaning them (which typically manifests as a later
  double-allocation)

## 0.9.1 (2017-02-25)
- Add configuration `runtime_assert` to check GC invariants at runtime
- Use tail-recursive calls in the block recycler (which deals with large
  block lists)
- Wait for the compaction work list to stabilise before processing it
  (otherwise we move blocks which are then immediately discarded)
- Track the difference between blocks on the end of the file being full
  of zeroes due to ftruncate versus being full of junk due to discard
- On open, truncate the file to erase trailing junk
- Don't try to use free space between header structures for user data
  since we assume all blocks after the start of free space are movable
  and header blocks aren't (in this implementation)
- Make cluster locks recursive, hold relevant metadata read locks while
  reading or writing data clusters to ensure they aren't moved while
  we're using them.
- Add a debug testing mode and use it in a test case to verify that
  compact mid-write works as expected.

## 0.9.0 (2017-02-21)
- Add online coalescing mode and background cluster recycling thread
- Rename internal modules and types
- Ensure the interval tree remains balanced to improve performance

## 0.8.1 (2017-02-13)
- fix error in META file

## 0.8.0 (2017-02-13)
- update to Mirage 3 APIs
- now requires OCaml 4.03+
- ensure the interval tree is kept balanced

## 0.7.2 (2016-12-21)
- if `discard` is not enabled, fail `discard` calls
- if `discard` is enabled, enable lazy-refcounts and zero refcount clusters
  to avoid breaking refcounts over `discard`, `compact`

## 0.7.1 (2016-12-15)
- speed up `check` and `compact` up to 50x
- `qcow-tool compact` work around files which aren't a whole number of
  sectors

## 0.7.0 (2016-12-10)
- now functorised over `TIME`
- allow background compact to be cancelled
- cancel background compact to allow regular I/O to go through
- don't trigger the background compact until 1s after the last
  `discard`
- on `connect`, sanity-check the image

## 0.6.0 (2016-12-04)
- rename ocamlfind package from `qcow-format` to `qcow` for uniformity
- add support for runtime configuration arguments to `connect` and `create`
- add support for `discard` (aka TRIM or UNMAP) and online compaction
  (through a stop-the-world GC)
- switch the build from `oasis` to `topkg` (thanks to @jgimenez)

## 0.5.0 (2016-11-26)
- `resize` now takes a new size in bytes (rather than sectors) and uses a
  labelled argument
- `qcow-tool info` now takes a `--filter <expression>` for example
  `qcow-tool info ... --filter .size` to view the virtual size

## 0.4.2 (2016-09-21)
- Don't break the build if `Block.connect` has optional arguments

## 0.4.1 (2016-08-17)
- Remove one necessary source of `flush` calls
- CLI: add `mapped` command to list the mapped regions of a file

## 0.4 (2016-08-03)
- For buffered block devices, call `flush` to guarantee metadata correctness
- In lazy_refcounts mode (the default), do not compute any refcounts
- CLI: the `repair` command should recompute refcounts

## 0.3 (2016-05-12)
- Depend on ppx, require OCaml 4.02+

## 0.2 (2016-01-15)
- Use qcow version 3 by default, setting `lazy_refcount=on`
- Unit tests now verify that `qemu-img check` is happy and that `qemu-nbd`
  sees the same data we wrote

## 0.1 (2015-11-09)
- initial `V1_LWT.BLOCK` support
- caches metadata for performance
- CLI tool for manipulating images
- supports the `seek_mapped` `seek_unmapped` interface for iterating over
  sparse regions
