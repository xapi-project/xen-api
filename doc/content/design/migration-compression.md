
# Migration Stream Compression inside XenGuest

To reduce VM migration time, we want to compress the VM migration data
stream.  This document describes a new approach that might replace an
existing feature, described first below. 

## Existing Stream Compression

Xapi implements external migration stream compression. This uses an
external zstd process each to compress (source) and decompress
(destination) the VM and GPU streams, restively. The compression is
transparent to the underlying daemons like XenGuest and vgpu/demu. It is
controlled by an optional boolean parameter `compress` to the VM.migrate
API/XE call.  When not explicitly provided, a boolean pool parameter
`Pool.migration_compression` is used. The current default (false) is to
not use use compression for migration. A pool-level default simplifies
managing the policy to use for migration compared to a host-level
default.

This feature requires to communicate the use of compression to the
receiving host such that it can set up the decompression; this is done
using cookies. Both VM and GPU memory is compressed during VM migration.

Benchmarking found that this compression scheme is only beneficial when
using slow networks (like 10Gb Ethernet). It comes with considerable
internal overhead from using processes and pipe communication for the
compression and decompression work. That is the reason stream
compression is not enabled by default.

The main implementation of the feature (with some changes added later)
is in commit: 

* f14fb91137197f24a4784612dd0f2d863ee22fb1 
* CP-39640/CP-39157 Add stream compression for VM migration

## XenGuest-based Stream Compression

This design is about adding an alternative stream compression.  This one
is implemented inside XenGuest such that the stream it produces is
internally using compression. XenGuest is not using an external process
for this and hence incurs less overhead because no
process-intercommunication is required. Benchmarking confirmed the
performance advantage of this architecture. 

The use of compression on the source side needs to be signaled by
Xenopsd via a command line argument. On the receiving side XenGuest will
recognise the compressed stream and does not require special signaling.

Xapi only permits migration from older to newer versions of the Xapi
Toolstack. This implies that no older XenGuest version will ever receive
a potentially compressed stream that it would not be equipped to handle.
For backward compatibility, xapi still needs to be able to receive a
compressed migration stream using external compression - even if future
Xapi implementation chose no longer to create them.

Currently only the stream created by XenGuest would use compression, but
not the stream containing GPU data created by vgpu/demu. This might
change in the future when we decide to implement in-stream compression
there, too.

Theoretically, internal compression implemented in XenGuest and existing
external compression are independent and could be used both. However,
this would apply a compression to an already compressed stream, which is
unlikely to be effective and we don't plan to support this feature.

## API Design

* VM.migrate API and `xe vm-migrate` remain unchanged. Both accept an
  optional boolean value that indicates whether to use compression or
  not. In the absence of this parameter, the default is taken from
  `Pool.migration_compression` (which is currently `false`).

* New parameter `Pool.migration_compressor` is a string value indicating
  which compression method to use, if compression is used. This permits
  to change change the default compression method over time, as well
  as giving users a choice - albeit only on the pool level and not per
  migration or per host. I believe a policy is more useful than detailed
  control.

* `Pool.migration_compressor=stream` - use the existing external method

* `Pool.migration_compressor=xenguest` - use the new internal method

## Upgrade

Xapi will always accept stream/external compressed migration streams but
might stop using them for migration.

## Outlook

We might add internal compression to vGPU migration in the future. The
API would be unaffected by this.
