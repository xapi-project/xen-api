#!/usr/bin/python

"""
Dummy extent reader that returns a huge extent list
"""

import json
import sys

# We simulate a 4 TiB disk
DUMMY_DISK_SIZE = 4 * 1024 * 1024 * 1024 * 1024

# Every second 64 KiB block in the disk will be allocated to get a large extent
# list. This is the granularity at which QEMU 2.12 reports allocated blocks for
# qcow images.
BLOCK_SIZE = 64 * 1024

def _main():
    offset = int(sys.argv[6])
    length = int(sys.argv[8])
    if not (length + offset <= DUMMY_DISK_SIZE and length > 0 and offset >= 0):
        raise ValueError("Invalid length={}, offset={} for disk size {}".format(
            length, offset, DUMMY_DISK_SIZE))
    extents = [
        {'flags': 0, 'length': BLOCK_SIZE}
        for _offset in xrange(0, length, BLOCK_SIZE)
    ]
    print json.dumps(extents)

if __name__ == '__main__':
    _main()
