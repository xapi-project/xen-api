#!/usr/bin/python

import json

# We simulate a 4 TiB disk
DUMMY_DISK_SIZE = 4 * 1024 * 1024 * 1024 * 1024

# Every second 64 KiB block in the disk will be allocated to get a large extent
# list. This is the granularity at which QEMU 2.12 reports allocated blocks for
# qcow images.
BLOCK_SIZE = 64 * 1024

def main():
    extents = []
    for offset in range(0, DUMMY_DISK_SIZE, BLOCK_SIZE * 2):
        extents += [{'flags': 0, 'length': BLOCK_SIZE}]
    print json.dumps(extents)

if __name__ == '__main__':
    main()
