# The XenServer VM import/format

This document describes the disk encoding used in version XXX of the XenServer VM import/export format.

Each disk is encoded as a directory full of files, within a stream in 'tar' format. The directory name must match the name of the VDI within the VM metadata XML file. Each disk is subdivided into blocks each of which is represented by 2 files:

    ---------- 1 djs djs 1048576 Jan  1  1970 00000000
    ---------- 1 djs djs      40 Jan  1  1970 00000000.checksum

The file stem is treated as a counter, *not as a disk offset*. The counter increases monotonically through the stream. The file with the suffix .checksum contains the sha1sum of the corresponding block e.g.

 $ sha1sum 00000000
 3b71f43ff30f4b15b5cd85dd9e95ebc7e84eb5a3  00000000
 $ cat 00000000.checksum
 3b71f43ff30f4b15b5cd85dd9e95ebc7e84eb5a3

The first and last blocks must be present. Readers expect the block size to be encoded in the size of the first block. A typical block size is 1MiB.

## Omitting empty blocks

If it is known that a block is entirely empty (i.e. full of zeroes) then it may
be ommitted from the stream *provided it is neither the first nor the last block*.
The ommission is signaled by incrementing the counter value by 1 for every
ommitted block.

Example sequence 1:

```sh
00000000
00000000.checksum
00000001
00000001.checksum
00000002
00000002.checksum
```

no blocks have been ommitted, since the counter value increases by 1 each block.

Example sequence 2:

```sh
00000000
00000000.checksum
00000002
00000002.checksum
```

one block has been ommitted, since the counter value increased by 2.

## Inserting dummy blocks

Sometimes it is convenient to insert extra information into the stream. This can
be done by adding extra zero-length blocks, incrementing the counter in the
normal way.
