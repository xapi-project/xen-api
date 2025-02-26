---
title: xc_domain_populate_physmap()
description: Populate a Xen domain's physical memory map
mermaid:
  force: true
---
`xc_domain_populate_physmap()` and `xc_domain_populate_physmap_exact()`
populate a Xen domain's physical memory map:
Both call the `populate_physmap`
hypercall and `xc_domain_populate_physmap_exact()` also sets the flag
for allocating memory only on the given NUMA node.

As an overview, it
[constructs](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1022-L1071)
a `struct memop_args` from the requested
[reservation](struct/xen_memory_reservation)
(start address, page size, now many of them, optionally on which NUMA node) and
[passes](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1459)
it to
[populate_physmap()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L159-L314)
to
[allocate](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L197)
the requested amount of pages:

{{% include "populate_physmap-dataflow.md" %}}

## construct_memop_from_reservation()

[construct_memop_from_reservation()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1022-L1071)
populates `struct memop_args` using the
[hypercall arguments](struct/xen_memory_reservation). It:

- Copies `extent_start`, `nr_extents`, and `extent_order`.
- Populates `memop_args->memflags` using the given `mem_flags`.

### Converting a vNODE to a pNODE for vNUMA

When a vNUMA vnode is passed using `XENMEMF_vnode`, and `domain->vnuma` and
`domain->vnuma->nr_vnodes` are set, and the vnode maps to a pnode, it also:

- Populates the `pnode` in the `memflags` of the `struct memop_args`
- and sets a `XENMEMF_exact_node_request` in them as well.

### Using propagate_node() to pass a pNODE

If no vNUMA node is passed, `construct_memop_from_reservation`
[calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1067)
[propagate_node()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L524-L547)
to propagate the NUMA node and `XENMEMF_exact_node_request` for use in Xen.

## Allocate pages for the domain

`memory_op()`
[passes](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1459)
the populated `struct memop_args` to
[populate_physmap()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L159-L314)
to
[loop over the extents](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L197)
to populate:

For each extent in the reservation,
[it calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L275)
[alloc_domheap_pages()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L2641)
which
[calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L2673)
[alloc_heap_pages()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L968)
which in turn
[calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L1005)
[get_free_buddy()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L855)
to allocate the requested memory page.

## Find a page using the buddy allocator

[get_free_buddy()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L855-L1116)
is the main function of the Xen buddy allocator.
If possible, it tries to find the best NUMA node and memory zone to allocate from.

This flowchart shows an overview of the effects of the decisions described below:

{{% include "get_free_buddy-flowchart.md" %}}

Input parameters:
- `struct domain`
- Zones to allocate from (`zone_hi` until `zone_lo`)
- Page order (size of the page)
  - populate_physmap() starts with 1GB pages and falls back to 2MB and 4k pages.

Its first attempt is to find a page of matching page order
on the requested NUMA node(s).

If this is not successful, it looks to breaking higher page orders,
and if that fails too, it lowers the zone until `zone_lo`.

It does not attempt to use not scrubbed pages, but when `memflags`
tell it `MEMF_no_scrub`, it uses `check_and_stop_scrub(pg)` on 4k
pages to prevent breaking higher order pages instead.

If this fails, it checks if other NUMA nodes shall be tried.

### Exact NUMA allocation (on request, e.g. for vNUMA)

For example for vNUMA domains, the calling functions pass one specific
NUMA node, and they would also set `MEMF_exact_node` to make sure that
memory is specifically only allocated from this NUMA node.

If no NUMA node was passed or the allocation from it failed, and
`MEMF_exact_node` was not set in `memflags`, the function falls
back to the first fallback, NUMA-affine allocation.

### NUMA-affine allocation

For local NUMA memory allocation, the domain should have one or more NUMA nodes
in its `struct domain->node_affinity` field when this function is called.

This happens as part of
[NUMA placement](../../../xenopsd/walkthroughs/VM.build/Domain.build/#numa-placement)
which writes the planned vCPU affinity of the domain's vCPUs to the XenStore
which [xenguest](../../../xenopsd/walkthroughs/VM.build/xenguest) reads to
update the vCPU affinities of the domain's vCPUs in Xen, which in turn, by
default (when to domain->auto_node_affinity is active) also updates the
`struct domain->node_affinity` field.

Note: In case it contains multiple
NUMA nodes, this step allocates from the next NUMA node after the previous
NUMA node the domain allocated from in a round-robin way.

Otherwise, the function falls back to host-wide round-robin allocation.

### Host-wide round-robin allocation

When the domain's `node_affinity` is not defined or did not succeed
and `MEMF_exact_node` was not passed in `memflags`, all remaining
NUMA nodes are attempted in a round-robin way: Each subsequent call
uses the next NUMA node after the previous node that the domain
allocated memory from.
