shared-block-ring
=================

[![Build Status](https://travis-ci.org/mirage/shared-block-ring.png?branch=master)](https://travis-ci.org/mirage/shared-block-ring)
[![Coverage Status](https://coveralls.io/repos/mirage/shared-block-ring/badge.png?branch=master)](https://coveralls.io/r/mirage/shared-block-ring?branch=master)

A simple on-disk fixed length queue in the style of the
Xen [shared-memory-ring](https://github.com/mirage/shared-memory-ring).
In particular the producer and consumer APIs allow clients to control
exactly when data is exposed to the consumer and removed from the queue.

Example usage
-------------

First create a "block device"-- any file will do:
```
dd if=/dev/zero of=test.raw bs=10240 count=1
```

Then initialise the ring in the file:
```
./main.native create
```

Then start two shells, in one run:
```
./main.native produce
```
and in the other
```
./main.native consume
```

The producer takes input from stdin and puts it onto the ring, and the consumer takes entries from the ring and prints them to stdout.
