---
title: Guest agents
layout: default
---

"Guest agents" are special programs which run inside VMs which can be controlled
via the XenAPI.

One communication method between XenAPI clients is via Xenstore.

Adding Xenstore entries to VMs
------------------------------

Developers may wish to install guest agents into VMs which take special action based on the type of the VM. In order to communicate this information into the guest, a special Xenstore name-space known as `vm-data` is available which is populated at VM creation time. It is populated from the `xenstore-data` map in the VM record.

Set the `xenstore-data` parameter in the VM record:

    xe vm-param-set uuid= xenstore-data:vm-data/foo=bar

Start the VM.

If it is a Linux-based VM, install the COMPANY\_TOOLS and use the `xenstore-read` to verify that the node exists in Xenstore.

> **Note**
>
> Only prefixes beginning with `vm-data` are permitted, and anything not in this name-space will be silently ignored when starting the VM.
