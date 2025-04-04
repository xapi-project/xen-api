---
title: Memory Setup
description: Creation and allocation of the boot memory layout of VMs
weight: 30
mermaid:
  force: true
---
## HVM boot memory setup

For HVM domains, `hvm_build_setup_mem()` is responsible for deriving the memory
layout of the new domain, allocating the required memory and populating for the
new domain. It must:

1.  Derive the `e820` memory layout of the system memory of the domain
    including memory holes depending on PCI passthrough and vGPU flags.
2.  Load the BIOS/UEFI firmware images
3.  Store the final MMIO hole parameters in the Xenstore
4.  Call the `libxenguest` function `xc_dom_boot_mem_init()` (see below)
5.  Call `construct_cpuid_policy()` to apply the CPUID `featureset` policy

It starts this by:
- Getting `struct xc_dom_image`, `max_mem_mib`, and `max_start_mib`.
- Calculating start and size of lower ranges of the domain's memory maps
  - taking memory holes for I/O into account, e.g. `mmio_size` and `mmio_start`.
- Calculating `lowmem_end` and `highmem_end`.

## Calling into libxenguest for the bootmem setup

`hvm_build_setup_mem()` then calls the [libxenguest](../../../../lib/xenguest/)
function
[xc_dom_boot_mem_init()](../../../../lib/xenguest/xc_dom_boot_mem_init.md)
to set up the boot memory of domains.

`xl` CLI also uses it to set up the boot memory of domains.
It constructs the memory layout of the domain and allocates and populates
the main system memory of the domain using calls to
[xc_domain_populate_physmap()](../../../../lib/xenctrl/xc_domain_populate_physmap.md).

For more details on the VM build step involving `xenguest` and Xen side see
https://wiki.xenproject.org/wiki/Walkthrough:_VM_build_using_xenguest
