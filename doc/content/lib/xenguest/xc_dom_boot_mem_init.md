---
title: xc_dom_boot_mem_init()
description: VM boot memory setup by calling meminit_hvm() or meminit_pv()
mermaid:
  force: true
---
## VM boot memory setup

[xenguest's](../../xenopsd/walkthroughs/VM.build/xenguest/_index.md)
`hvm_build_setup_mem()` and `libxl` and the `xl` CLI call
[xc_dom_boot_mem_init()](https://github.com/xen-project/xen/blob/39c45c/tools/libs/guest/xg_dom_boot.c#L110-L126)
to allocate and populate the domain's system memory for booting it:

{{% include "boot_mem_init-chart.md" %}}

The allocation strategies of them called functions are:

### Strategy of the libxenguest meminit functions

- Attempt to allocate 1GB superpages when possible
- Fall back to 2MB pages when 1GB allocation failed
- Fall back to 4k pages when both failed

They use
[xc_domain_populate_physmap()](../xenctrl/xc_domain_populate_physmap.md)
to perform memory allocation and to map the allocated memory
to the system RAM ranges of the domain.

### Strategy of xc_domain_populate_physmap()

[xc_domain_populate_physmap()](../xenctrl/xc_domain_populate_physmap.md)
calls the `XENMEM_populate_physmap` command of the Xen memory hypercall.


For a more detailed walk-through of the inner workings of this hypercall,
see the reference on
[xc_domain_populate_physmap()](../xenctrl/xc_domain_populate_physmap).

For more details on the VM build step involving `xenguest` and Xen side see:
https://wiki.xenproject.org/wiki/Walkthrough:_VM_build_using_xenguest
