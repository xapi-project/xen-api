+++
title = "Metadata-on-LUN"
+++

In the present version of XenServer, metadata changes resulting in
writes to the database are not persisted in non-volatile storage. Hence,
in case of failure, up to five minutes’ worth of metadata changes could
be lost. The Metadata-on-LUN feature addresses the issue by
ensuring that all database writes are retained. This will be used to
improve recovery from failure by storing incremental *deltas* which can
be re-applied to an old version of the database to bring it more
up-to-date. An implication of this is that clients will no longer be
required to perform a ‘pool-sync-database’ to protect critical writes,
because all writes will be implicitly protected.

This is implemented by saving descriptions of all persistent database
writes to a LUN when HA is active. Upon xapi restart after failure, such
as on master fail-over, these descriptions are read and parsed to
restore the latest version of the database.

Layout on block device
======================

It is useful to store the database on the block device as well as the
deltas, so that it is unambiguous on recovery which version of the
database the deltas apply to.

The content of the block device will be structured as shown in
the table below. It consists of a header; the rest of the
device is split into two halves.

|                       |    Length (bytes) | Description
|-----------------------|------------------:|----------------------------------------------
| Header                |                16 | Magic identifier
|                       |                 1 | ASCII NUL
|                       |                 1 | Validity byte
| First half database   |                36 | UUID as ASCII string
|                       |                16 | Length of database as decimal ASCII
|                       |  *(as specified)* | Database (binary data)
|                       |                16 | Generation count as decimal ASCII
|                       |                36 | UUID as ASCII string
| First half deltas     |                16 | Length of database delta as decimal ASCII
|                       |  *(as specified)* | Database delta (binary data)
|                       |                16 | Generation count as decimal ASCII
|                       |                36 | UUID as ASCII string
| Second half database  |                36 | UUID as ASCII string
|                       |                16 | Length of database as decimal ASCII
|                       |  *(as specified)* | Database (binary data)
|                       |                16 | Generation count as decimal ASCII
|                       |                36 | UUID as ASCII string
| Second half deltas    |                16 | Length of database delta as decimal ASCII
|                       |  *(as specified)* | Database delta (binary data)
|                       |                16 | Generation count as decimal ASCII
|                       |                36 | UUID as ASCII string

After the header, one or both halves may be devoid of content. In a half
which contains a database, there may be zero or more deltas (repetitions
of the last three entries in each half).

The structure of the device is split into two halves to provide
double-buffering. In case of failure during write to one half, the other
half remains intact.

The magic identifier at the start of the file protect against attempting
to treat a different device as a redo log.

The validity byte is a single `ascii</span> character indicating the
state of the two halves. It can take the following values:

|  Byte | Description
|-------|------------------------
|   `0` | Neither half is valid
|   `1` | First half is valid
|   `2` | Second half is valid

The use of lengths preceding data sections permit convenient reading.
The constant repetitions of the UUIDs act as nonces to protect
against reading in invalid data in the case of an incomplete or corrupt
write.

Architecture
============

The I/O to and from the block device may involve long delays. For
example, if there is a network problem, or the iSCSI device disappears,
the I/O calls may block indefinitely. It is important to isolate this
from xapi. Hence, I/O with the block device will occur in a separate
process.

Xapi will communicate with the I/O process via a UNIX domain socket using a
simple text-based protocol described below. The I/O process will use to
ensure that it can always accept xapi’s requests with a guaranteed upper
limit on the delay. Xapi can therefore communicate with the process
using blocking I/O.

Xapi will interact with the I/O process in a best-effort fashion. If it
cannot communicate with the process, or the process indicates that it
has not carried out the requested command, xapi will continue execution
regardless. Redo-log entries are idempotent (modulo the raising of
exceptions in some cases) so it is of little consequence if a particular
entry cannot be written but others can. If xapi notices that the process
has died, it will attempt to restart it.

The I/O process keeps track of a pointer for each half indicating the
position at which the next delta will be written in that half.

Protocol
--------

Upon connection to the control socket, the I/O process will attempt to
connect to the block device. Depending on whether this is successful or
unsuccessful, one of two responses will be sent to the client.

-   `connect|ack_` if it is successful; or

-   `connect|nack|<length>|<message>` if it is unsuccessful, perhaps
    because the block device does not exist or cannot be read from. The
    `<message>` is a description of the error; the `<length>` of the message
    is expressed using 16 digits of decimal ascii.

The former message indicates that the I/O process is ready to receive
commands. The latter message indicates that commands can not be sent to
the I/O process.

There are three commands which xapi can send to the I/O
process. These are described below, with a high level description of the
operational semantics of the I/O process’ actions, and the corresponding
responses. For ease of parsing, each command is ten bytes in length.

### Write database

Xapi requests that a new database is written to the block device, and
sends its content using the data socket.

##### Command:

:   `writedb___|<uuid>|<generation-count>|<length>`
:   The UUID is expressed as 36 ASCII
    characters. The *length* of the data and the *generation-count* are
    expressed using 16 digits of decimal ASCII.

##### Semantics:

1.  Read the validity byte.
2.  If one half is valid, we will use the other half. If no halves
    are valid, we will use the first half.
3.  Read the data from the data socket and write it into the
    chosen half.
4.  Set the pointer for the chosen half to point to the position
    after the data.
5.  Set the validity byte to indicate the chosen half is valid.

##### Response:

:   `writedb|ack_`                        in case of successful write; or
:   `writedb|nack|<length>|<message>`     otherwise.
:   For error messages, the *length* of the message is expressed using
    16 digits of decimal <span
    style="font-variant:small-caps;">ascii</span>. In particular, the
    error message for timeouts is the string `Timeout`.

### Write database delta

Xapi sends a description of a database delta to append to the block
device.

##### Command:

:   `writedelta|<uuid>|<generation-count>|<length>|<data>`
:   The UUID is expressed as 36 ASCII
    characters. The *length* of the data and the *generation-count* are
    expressed using 16 digits of decimal ASCII.

##### Semantics:

1.  Read the validity byte to establish which half is valid. If
    neither half is valid, return with a `nack`.
2.  If the half’s pointer is set, seek to that position. Otherwise,
    scan through the half and stop at the position after the
    last write.
3.  Write the entry.
4.  Update the half’s pointer to point to the position after
    the entry.

##### Response:

:   `writedelta|ack_`                      in case of successful append; or
:   `writedelta|nack|<length>|<message>`   otherwise.
:   For error messages, the *length* of the message is expressed using
    16 digits of decimal ASCII. In particular, the
    error message for timeouts is the string `Timeout`.

### Read log

Xapi requests the contents of the log.

##### Command:

:   `read______`

##### Semantics:

1.  Read the validity byte to establish which half is valid. If
    neither half is valid, return with an `end`.
2.  Attempt to read the database from the current half.
3.  If this is successful, continue in that half reading entries up
    to the position of the half’s pointer. If the pointer is not
    set, read until a record of length zero is found or the end of
    the half is reached. Otherwise—if the attempt to the read the
    database was not successful—switch to using the other half and
    try again from step 2.
4.  Finally output an `end`.

##### Response:

:   `read|nack_|<length>|<message>`                   in case of error; or
:   `read|db___|<generation-count>|<length>|<data>`   for a database record, then a
                                                      sequence of zero or more
:   `read|delta|<generation-count>|<length>|<data>`   for each delta record, then
:   `read|end__`                                          
:   For each record, and for error messages, the *length* of the data or
    message is expressed using 16 digits of decimal <span
    style="font-variant:small-caps;">ascii</span>. In particular, the
    error message for timeouts is the string `Timeout`.

### Re-initialise log

Xapi requests that the block device is re-initialised with a fresh
redo-log.

##### Command:

:   `empty_____`\

##### Semantics:

:   1.  Set the validity byte to indicate that neither half is valid.

##### Response:

:   `empty|ack_`                      in case of successful re-initialisation; or
    `empty|nack|<length>|<message>`   otherwise.
:   For error messages, the *length* of the message is expressed using
    16 digits of decimal ASCII. In particular, the
    error message for timeouts is the string `Timeout`.

Impact on xapi performance
==========================

The implementation of the feature causes a slow-down in xapi of around
6% in the general case. However, if the LUN becomes inaccessible this
can cause a slow-down of up to 25% in the worst case.

The figure below shows the result of testing four configurations,
counting the number of database writes effected through a command-line
‘xe pool-param-set’ call.

-   The first and second configurations are xapi *without* the
    Metadata-on-LUN feature, with HA disabled and
    enabled respectively.

-   The third configuration shows xapi *with* the
    Metadata-on-LUN feature using a healthy LUN to which
    all database writes can be successfully flushed.

-   The fourth configuration shows xapi *with* the
    Metadata-on-LUN feature using an inaccessible LUN for
    which all database writes fail.

![Impact of feature on xapi database-writing performance. (Green points
represent individual samples; red bars are the arithmetic means of
samples.)](performance.svg)

Testing strategy
================

The section above shows how xapi performance is affected by this feature. The
sections below describe the dev-testing which has already been undertaken, and
propose how this feature will impact on regression testing.

Dev-testing performed
---------------------

A variety of informal tests have been performed as part of the
development process:

Enable HA.

:   Confirm LUN starts being used to persist database writes.

Enable HA, disable HA.

:   Confirm LUN stops being used.

Enable HA, kill xapi on master, restart xapi on master.

:   Confirm that last database write before kill is successfully
    restored on restart.

Repeatedly enable and disable HA.

:   Confirm that no file descriptors are leaked (verified by counting
    the number of descriptors in /proc/*pid*/fd/).

Enable HA, reboot the master.

:   Due to HA, a slave becomes the master (or this can be forced using
    ‘xe pool-emergency-transition-to-master’). Confirm that the new
    master starts is able to restore the database from the LUN from the
    point the old master left off, and begins to write new changes to
    the LUN.

Enable HA, disable the iSCSI volume.

:   Confirm that xapi continues to make progress, although database
    writes are not persisted.

Enable HA, disable and enable the iSCSI volume.

:   Confirm that xapi begins to use the LUN when the iSCSI volume is
    re-enabled and subsequent writes are persisted.

These tests have been undertaken using an iSCSI target VM and a real
iSCSI volume on lannik. In these scenarios, disabling the iSCSI volume
consists of stopping the VM and unmapping the LUN, respectively.

Proposed new regression test
----------------------------

A new regression test is proposed to confirm that all database writes
are persisted across failure.

There are three types of database modification to test: row creation,
field-write and row deletion. Although these three kinds of write could
be tested in separate tests, the means of setting up the pre-conditions
for a field-write and a row deletion require a row creation, so it is
convenient to test them all in a single test.

1.  Start a pool containing three hosts.

2.  Issue a CLI command on the master to create a row in the
    database, e.g.

    `xe network-create name-label=a`.

3.  Forcefully power-cycle the master.

4.  On fail-over, issue a CLI command on the new master to check that
    the row creation persisted:

    `xe network-list name-label=a`,

    confirming that the returned string is non-empty.

5.  Issue a CLI command on the master to modify a field in the new row
    in the database:

    `xe network-param-set uuid=<uuid> name-description=abcd`,

    where `<uuid>` is the UUID returned from step 2.

6.  Forcefully power-cycle the master.

7.  On fail-over, issue a CLI command on the new master to check that
    the field-write persisted:

    `xe network-param-get uuid=<uuid> param-name=name-description`,

    where `<uuid>` is the UUID returned from step 2. The returned string
    should contain

    `abcd`.

8.  Issue a CLI command on the master to delete the row from the
    database:

    `xe network-destroy uuid=<uuid>`,

    where `<uuid>` is the UUID returned from step 2.

9.  Forcefully power-cycle the master.

10. On fail-over, issue a CLI command on the new master to check that
    the row does not exist:

    `xe network-list name-label=a`,

    confirming that the returned string is empty.

Impact on existing regression tests
-----------------------------------

The Metadata-on-LUN feature should mean that there is no
need to perform an ‘xe pool-sync-database’ operation in existing HA
regression tests to ensure that database state persists on xapi failure.
