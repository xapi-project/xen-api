---
title: Hooks
---

There are a number of hook points at which xenopsd may execute certain scripts. These scripts are found in hook-specific directories of the form `/etc/xapi.d/<hookname>/`. All executable scripts in these directories are run with the following arguments:

    <script.sh> -reason <reason> -vmuuid <uuid of VM>

The scripts are executed in filename-order. By convention, the filenames are usually of the form `10resetvdis`.

The hook points are:

    vm-pre-shutdown
    vm-pre-migrate
    vm-post-migrate (Dundee only)
    vm-pre-start
    vm-pre-reboot
    vm-pre-resume
    vm-post-resume (Dundee only)
    vm-post-destroy
    
and the reason codes are:

    clean-shutdown
    hard-shutdown
    clean-reboot
    hard-reboot
    suspend
    source -- passed to pre-migrate hook on source host
    destination -- passed to post-migrate hook on destination (Dundee only)
    none

For example, in order to execute a script on VM shutdown, it would be sufficient to create the script in the post-destroy hook point:

    /etc/xapi.d/vm-post-destroy/01myscript.sh

containing

    #!/bin/bash
    echo I was passed $@ > /tmp/output

And when, for example, VM e30d0050-8f15-e10d-7613-cb2d045c8505 is shut-down, the script is executed:

    [vagrant@localhost ~]$ sudo xe vm-shutdown --force uuid=e30d0050-8f15-e10d-7613-cb2d045c8505
    [vagrant@localhost ~]$ cat /tmp/output
    I was passed -vmuuid e30d0050-8f15-e10d-7613-cb2d045c8505 -reason hard-shutdown


