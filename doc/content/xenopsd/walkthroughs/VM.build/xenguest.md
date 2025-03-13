---
title: xenguest
description:
  "Perform building VMs: Allocate and populate the domain's system memory."
---
As part of starting a new domain in VM_build, `xenopsd` calls `xenguest`.
When multiple domain build threads run in parallel,
also multiple instances of `xenguest` also run in parallel:

```mermaid
flowchart
subgraph xenopsd VM_build[xenopsd&nbsp;VM_build&nbsp;micro#8209;ops]
direction LR
xenopsd1[Domain.build - Thread #1] --> xenguest1[xenguest #1]
xenopsd2[Domain.build - Thread #2] --> xenguest2[xenguest #2]
xenguest1 --> libxenguest
xenguest2 --> libxenguest2[libxenguest]
click xenopsd1 "../Domain.build/index.html"
click xenopsd2 "../Domain.build/index.html"
click xenguest1 "https://github.com/xenserver/xen.pg/blob/XS-8/patches/xenguest.patch" _blank
click xenguest2 "https://github.com/xenserver/xen.pg/blob/XS-8/patches/xenguest.patch" _blank
click libxenguest "https://github.com/xen-project/xen/tree/master/tools/libs/guest" _blank
click libxenguest2 "https://github.com/xen-project/xen/tree/master/tools/libs/guest" _blank
libxenguest --> Xen[Xen<br>Hypervisor]
libxenguest2 --> Xen
end
```

## About xenguest

`xenguest` is called by the xenopsd [Domain.build](Domain.build) function
to perform the build phase for new VMs, which is part of the `xenopsd`
[VM.start operation](VM.start).

[xenguest](https://github.com/xenserver/xen.pg/blob/XS-8/patches/xenguest.patch)
was created as a separate program due to issues with
`libxenguest`:

- It wasn't threadsafe: fixed, but it still uses a per-call global struct
- It had an incompatible licence, but now licensed under the LGPL.

Those were fixed, but we still shell out to `xenguest`, which is currently
carried in the patch queue for the Xen hypervisor packages, but could become
an individual package once planned changes to the Xen hypercalls are stabilised.

Over time, `xenguest` has evolved to build more of the initial domain state.

## Interface to xenguest

```mermaid
flowchart
subgraph xenopsd VM_build[xenopsd&nbsp;VM_build&nbsp;micro#8209;op]
direction TB
mode
domid
memmax
Xenstore
end
mode[--mode build_hvm] --> xenguest
domid --> xenguest
memmax --> xenguest
Xenstore[Xenstore platform data] --> xenguest
```

`xenopsd` must pass this information to `xenguest` to build a VM:

- The domain type to build for (HVM, PHV or PV).
  - It is passed using the command line option `--mode hvm_build`.
- The `domid` of the created empty domain,
- The amount of system memory of the domain,
- A number of other parameters that are domain-specific.

`xenopsd` uses the Xenstore to provide platform data:

- the vCPU affinity
- the vCPU credit2 weight/cap parameters
- whether the NX bit is exposed
- whether the viridian CPUID leaf is exposed
- whether the system has PAE or not
- whether the system has ACPI or not
- whether the system has nested HVM or not
- whether the system has an HPET or not

When called to build a domain, `xenguest` reads those and builds the VM accordingly.

## Walkthrough of the xenguest build mode

```mermaid
flowchart
subgraph xenguest[xenguest&nbsp;#8209;#8209;mode&nbsp;hvm_build&nbsp;domid]
direction LR
stub_xc_hvm_build[stub_xc_hvm_build#40;#41;] --> get_flags[
    get_flags#40;#41;&nbsp;<#8209;&nbsp;Xenstore&nbsp;platform&nbsp;data
]
stub_xc_hvm_build --> configure_vcpus[
    configure_vcpus#40;#41;&nbsp;#8209;>&nbsp;Xen&nbsp;hypercall
]
stub_xc_hvm_build --> setup_mem[
    setup_mem#40;#41;&nbsp;#8209;>&nbsp;Xen&nbsp;hypercalls&nbsp;to&nbsp;setup&nbsp;domain&nbsp;memory
]
end
```

Based on the given domain type, the `xenguest` program calls dedicated
functions for the build process of the given domain type.

These are:

- `stub_xc_hvm_build()` for HVM,
- `stub_xc_pvh_build()` for PVH, and
- `stub_xc_pv_build()` for PV domains.

These domain build functions call these functions:

1. `get_flags()` to get the platform data from the Xenstore
2. `configure_vcpus()` which uses the platform data from the Xenstore to configure vCPU affinity and the credit scheduler parameters vCPU weight and vCPU cap (max % pCPU time for throttling)
3.  The `setup_mem` function for the given VM type.

## The function hvm_build_setup_mem()

For HVM domains, `hvm_build_setup_mem()` is responsible for deriving the memory
layout of the new domain, allocating the required memory and populating for the
new domain. It must:

1.  Derive the `e820` memory layout of the system memory of the domain
    including memory holes depending on PCI passthrough and vGPU flags.
2.  Load the BIOS/UEFI firmware images
3.  Store the final MMIO hole parameters in the Xenstore
4.  Call the `libxenguest` function `xc_dom_boot_mem_init()` (see below)
5.  Call `construct_cpuid_policy()` to apply the CPUID `featureset` policy

## The function xc_dom_boot_mem_init()

```mermaid
flowchart LR
subgraph xenguest
hvm_build_setup_mem[hvm_build_setup_mem#40;#41;]
end
subgraph libxenguest
hvm_build_setup_mem --> xc_dom_boot_mem_init[xc_dom_boot_mem_init#40;#41;]
xc_dom_boot_mem_init -->|vmemranges| meminit_hvm[meninit_hvm#40;#41;]
click xc_dom_boot_mem_init "https://github.com/xen-project/xen/blob/39c45c/tools/libs/guest/xg_dom_boot.c#L110-L126" _blank
click meminit_hvm "https://github.com/xen-project/xen/blob/39c45c/tools/libs/guest/xg_dom_x86.c#L1348-L1648" _blank
end
```

`hvm_build_setup_mem()` calls
[xc_dom_boot_mem_init()](https://github.com/xen-project/xen/blob/39c45c/tools/libs/guest/xg_dom_boot.c#L110-L126)
to allocate and populate the domain's system memory.

It calls
[meminit_hvm()](https://github.com/xen-project/xen/blob/39c45c/tools/libs/guest/xg_dom_x86.c#L1348-L1648)
to loop over the `vmemranges` of the domain for mapping the system RAM
of the guest from the Xen hypervisor heap. Its goals are:

- Attempt to allocate 1GB superpages when possible
- Fall back to 2MB pages when 1GB allocation failed
- Fall back to 4k pages when both failed

It uses the hypercall
[XENMEM_populate_physmap](https://github.com/xen-project/xen/blob/39c45c/xen/common/memory.c#L1408-L1477)
to perform memory allocation and to map the allocated memory
to the system RAM ranges of the domain.

https://github.com/xen-project/xen/blob/39c45c/xen/common/memory.c#L1022-L1071

`XENMEM_populate_physmap`:

1.  Uses
    [construct_memop_from_reservation](https://github.com/xen-project/xen/blob/39c45c/xen/common/memory.c#L1022-L1071)
    to convert the arguments for allocating a page from
    [struct xen_memory_reservation](https://github.com/xen-project/xen/blob/master/xen/include/public/memory.h#L46-L80)
    to `struct memop_args`.
2.  Sets flags and calls functions according to the arguments
3.  Allocates the requested page at the most suitable place
    - depending on passed flags, allocate on a specific NUMA node
    - else, if the domain has node affinity, on the affine nodes
    - also in the most suitable memory zone within the NUMA node
4.  Falls back to less desirable places if this fails
    - or fail for "exact" allocation requests
5.  When no pages of the requested size are free,
    it splits larger superpages into pages of the requested size.

For more details on the VM build step involving `xenguest` and Xen side see:
https://wiki.xenproject.org/wiki/Walkthrough:_VM_build_using_xenguest
