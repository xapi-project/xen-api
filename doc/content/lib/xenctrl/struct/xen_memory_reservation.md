---
title: xen_memory_reservation
description: xen_memory_reservation for memory-related hypercalls
hidden: true
---
[struct xen_memory_reservation](https://github.com/xen-project/xen/blob/96970b46/xen/include/public/memory.h#L46-80)
is used by
[these XENMEM hypercall commands](https://github.com/xen-project/xen/blob/96970b46/xen/include/public/memory.h#L48-59):

- `XENMEM_increase_reservation`: Returns the first MFN of the allocated extents
- `XENMEM_decrease_reservation`: To pass the first GPFN of extents to free
- [XENMEM_populate_physmap](../xc_domain_populate_physmap):
   - In: To pass the first GPFN to populate with memory
   - Out: Returns the first GMFN base of extents that were allocated
     (NB. This command also updates the mach_to_phys translation table)
- `XENMEM_claim_pages`: Not used, must be passed as 0
  (This is explicitly checked: Otherwise, it returns `-EINVAL`)

[struct xen_memory_reservation](https://github.com/xen-project/xen/blob/96970b46/xen/include/public/memory.h#L46-80)
is defined as:

```c
struct xen_memory_reservation {
    XEN_GUEST_HANDLE(xen_pfn_t) extent_start; /* PFN of the starting extent */
    xen_ulong_t  nr_extents;   /* number of extents of size extent_order */
    unsigned int extent_order; /* an order of 0 means: 4k pages, 1: 8k, etc. */
    unsigned int mem_flags;
    domid_t      domid;        /* integer ID of the domain */
};
```

The `mem_flags` bit field is accessed using:

```js
/*
 * Maximum # bits addressable by the user of the allocated region (e.g., I/O
 * devices often have a 32-bit limitation even in 64-bit systems). If zero
 * then the user has no addressing restriction. This field is not used by
 * XENMEM_decrease_reservation.
 */
#define XENMEMF_address_bits(x)     (x)
#define XENMEMF_get_address_bits(x) ((x) & 0xffu)
/* NUMA node to allocate from. */
#define XENMEMF_node(x)     (((x) + 1) << 8)
#define XENMEMF_get_node(x) ((((x) >> 8) - 1) & 0xffu)
/* Flag to populate physmap with populate-on-demand entries */
#define XENMEMF_populate_on_demand (1<<16)
/* Flag to request allocation only from the node specified */
#define XENMEMF_exact_node_request  (1<<17)
#define XENMEMF_exact_node(n) (XENMEMF_node(n) | XENMEMF_exact_node_request)
/* Flag to indicate the node specified is virtual node */
#define XENMEMF_vnode  (1<<18)
```
