---
title: RRDD archival redesign
layout: default
design_doc: true
revision: 1
status: released (7,0)
---

## Introduction

Current problems with rrdd:

* rrdd stores knowledge about whether it is running on a master or a slave

This determines the host to which rrdd will archive a VM's rrd when the VM's
domain disappears - rrdd will always try to archive to the master. However,
when a host joins a pool as a slave rrdd is not restarted so this knowledge is
out of date. When a VM shuts down on the slave rrdd will archive the rrd
locally. When starting this VM again the master xapi will attempt to push any
locally-existing rrd to the host on which the VM is being started, but since
no rrd archive exists on the master the slave rrdd will end up creating a new
rrd and the previous rrd will be lost.

* rrdd handles rebooting VMs unpredictably

When rebooting a VM, there is a chance rrdd will attempt to update that VM's rrd
during the brief period when there is no domain for that VM. If this happens,
rrdd will archive the VM's rrd to the master, and then create a new rrd for the
VM when it sees the new domain. If rrdd doesn't attempt to update that VM's rrd
during this period, rrdd will continue to add data for the new domain to the old
rrd.

## Proposal

To solve these problems, we will remove some of the intelligence from rrdd and
make it into more of a slave process of xapi. This will entail removing all
knowledge from rrdd of whether it is running on a master or a slave, and also
modifying rrdd to only start monitoring a VM when it is told to, and only
archiving an rrd (to a specified address) when it is told to. This matches the
way xenopsd only manages domains which it has been told to manage.

## Design

For most VM lifecycle operations, xapi and rrdd processes (sometimes across more
than one host) cooperate to start or stop recording a VM's metrics and/or to
restore or backup the VM's archived metrics. Below we will describe, for each
relevant VM operation, how the VM's rrd is currently handled, and how we propose
it will be handled after the redesign.

#### VM.destroy

The master xapi makes a remove_rrd call to the local rrdd, which causes rrdd to
to delete the VM's archived rrd from disk. This behaviour will remain unchanged.

#### VM.start(\_on) and VM.resume(\_on)

The master xapi makes a push_rrd call to the local rrdd, which causes rrdd to
send any locally-archived rrd for the VM in question to the rrdd of the host on
which the VM is starting. This behaviour will remain unchanged.

#### VM.shutdown and VM.suspend

Every update cycle rrdd compares its list of registered VMs to the list of
domains actually running on the host. Any registered VMs which do not have a
corresponding domain have their rrds archived to the rrdd running on the host
believed to be the master. We will change this behaviour by stopping rrdd from
doing the archiving itself; instead we will expose a new function in rrdd's
interface:

```
val archive_rrd : vm_uuid:string -> remote_address:string -> unit
```

This will cause rrdd to remove the specified rrd from its table of registered
VMs, and archive the rrd to the specified host. When a VM has finished shutting
down or suspending, the xapi process on the host on which the VM was running
will call archive_rrd to ask the local rrdd to archive back to the master rrdd.

#### VM.reboot

Removing rrdd's ability to automatically archive the rrds for disappeared
domains will have the bonus effect of fixing how the rrds of rebooting VMs are
handled, as we don't want the rrds of rebooting VMs to be archived at all.

#### VM.checkpoint

This will be handled automatically, as internally VM.checkpoint carries out a
VM.suspend followed by a VM.resume.

#### VM.pool_migrate and VM.migrate_send

The source host's xapi makes a migrate_rrd call to the local rrd, with a
destination address and an optional session ID. The session ID is only required
for cross-pool migration. The local rrdd sends the rrd for that VM to the
destination host's rrdd as an HTTP PUT. This behaviour will remain unchanged.
