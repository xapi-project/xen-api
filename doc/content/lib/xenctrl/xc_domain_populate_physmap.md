---
title: xc_domain_populate_physmap()
description: Populate a Xen domain's physical memory map
mermaid:
  force: true
---
## Overview

[xenguest](../../../xenopsd/walkthroughs/VM.build/xenguest) uses
`xc_domain_populate_physmap()` to populate a Xen domain's physical memory map:
Both call the `XENMEM_populate_physmap` hypercall.

`xc_domain_populate_physmap_exact()` also sets the "exact" flag
for allocating memory only on the given NUMA node.
This is a very simplified overview of the hypercall:

{{% include "../xen/populate_physmap-chart.md" %}}

### memory_op(XENMEM_populate_physmap)

It calls
[construct_memop_from_reservation()](https://github.com/xen-project/xen/blob/39c45c/xen/common/memory.c#L1022-L1071)
to convert the arguments for allocating a page from
[struct xen_memory_reservation](https://github.com/xen-project/xen/blob/master/xen/include/public/memory.h#L46-L80)
to `struct memop_args` and
[passes](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1459)
it to
[populate_physmap()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L159-L314):

### construct_memop_from_reservation()

It populates `struct memop_args` using the
[hypercall arguments](struct/xen_memory_reservation). It:

- Copies `extent_start`, `nr_extents`, and `extent_order`.
- Populates `memop_args->memflags` using the given `mem_flags`.

#### Converting a vNODE to a pNODE for vNUMA

When a vNUMA vnode is passed using `XENMEMF_vnode`, and `domain->vnuma`
and `domain->vnuma->nr_vnodes` are set, and the
`vnode` (virtual NUMA node) maps to a `pnode` (physical NUMA node), it also:

- Populates the `pnode` in the `memflags` of the `struct memop_args`
- and sets a `XENMEMF_exact_node_request` in them as well.

#### propagate_node()

If no vNUMA node is passed, `construct_memop_from_reservation()`
[calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L1067)
[propagate_node()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L524-L547)
to propagate the NUMA node and `XENMEMF_exact_node_request` for use in Xen.

### populate_physmap()

It handles hypercall preemption and resumption after a preemption, keeps track of
the already populated pages.

For each range (extent), it runs an iteration of the
[allocation loop](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L197).
It
[passes](https://github.com/xen-project/xen/blob/e16acd80/xen/common/memory.c#L275)
the

- `struct domain`
- page order
- count remaining pages populate
- and the converted `memflags`

to
[alloc_domheap_pages()](https://github.com/xen-project/xen/blob/91772f84/xen/common/page_alloc.c#L2640-L2696):

### alloc_domheap_pages()

It
[calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L2673)
[alloc_heap_pages()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L967-L1116)
and on success, assigns the allocated pages to the domain.

### alloc_heap_pages()

It
[calls](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L1005)
get_free_buddy()[^1] to allocate a page at the most suitable place:
When no pages of the requested size are free,
it splits larger superpages into pages of the requested size.

### get_free_buddy()

It finds memory based on the flags and domain and return its `page struct`:

- Optionally allocate prefer to allocate from a passed NUMA node
- Optionally allocate from the domain's next affine NUMA node (round-robin)
- Optionally return if the preferred NUMA allocation did not succeed
- Optionally allocate from not-yet scrubbed memory
- Optionally allocate from the given range of memory zones
- Fall back allocate from the next NUMA node on the system (round-robin)

For details see the
[get_free_buddy()](../xen/get_free_buddy) finds memory based on the flags and domain.

## Full flowchart

{{% include "../xen/populate_physmap-dataflow.md" %}}
