vhd-tool
========

Command-line tools to manipulate, transcode and stream 
[vhd](http://en.wikipedia.org/wiki/VHD_(file_format)) format data.

Basic command-line tool examples
--------------------------------

To create an empty dynamic (i.e. grows on demand) vhd:
```
vhd-tool create filename.vhd --size 16GiB
```

To create an empty difference vhd:
```
vhd-tool create filename.vhd --size 16GiB --parent otherfile.vhd
```

To query all the parameters of a vhd:
```
vhd-tool info filename.vhd
```

To query a specific parameter:
```
vhd-tool get filename.vhd current-size
```

Example: incremental backup
---------------------------

(This is a work in progress)

When running VMs on a hypervisor like [XenServer](http://www.xenserver.org/), it's important to have a backup strategy for your important virtual disks. One possibility is to perform periodic disk snapshots and archive the "deltas" (or differences) between the new snapshot and the last.

First take a snapshot: this will be the first backup:
```
xe vdi-snapshot uuid=<uuid>
```
Next download the snapshot as a single .vhd:
```
xe vdi-export uuid=<uuid>
```
This will print a filename to the terminal.

Periodically (e.g. from cron), perform a new snapshot:
```
xe vdi-snapshot uuid=<uuid>
```
Next download the differences from a previous snapshot as a single .vhd:
```
xe vdi-export uuid=<uuid> relative-to=<previous-uuid>
```
Next, to avoid using too much disk space, count the number of snapshots and delete the oldest if you have too many:
```
xe vdi-destroy uuid=<oldest-uuid>
vhd-tool commit filename.vhd --into older.vhd
```

To restore a backup onto a fresh system use:
```
vhd-tool stream --source filename.vhd
                --source-format vhd
                --destination http://user:password@xenserver/import_vdi
                --destination-format vhd
                --progress
```


