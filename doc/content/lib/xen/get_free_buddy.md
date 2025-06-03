---
title: get_free_buddy()
description: Find free memory based on the given flags and optionally, a domain
mermaid:
  force: true
---
## Overview

[get_free_buddy()](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L855-L1116) is
[called](https://github.com/xen-project/xen/blob/e16acd80/xen/common/page_alloc.c#L1005)
from [alloc_heap_pages()](xc_domain_populate_physmap#alloc_heap_pages)
to find a page at the most suitable place for a memory allocation.

It finds memory depending on the given flags and domain:

- Optionally allocate prefer to allocate from a passed NUMA node
- Optionally allocate from the domain's next affine NUMA node (round-robin)
- Optionally return if the preferred NUMA allocation did not succeed
- Optionally allocate from not-yet scrubbed memory
- Optionally allocate from the given range of memory zones
- Fall back allocate from the next NUMA node on the system (round-robin)

## Input parameters

- `struct domain`
- Zones to allocate from (`zone_hi` until `zone_lo`)
- Page order (size of the page)
  - populate_physmap() starts with 1GB pages and falls back to 2MB and 4k pages.

## Allocation strategy

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

## Flowchart

This flowchart shows an overview of the decision chain of `get_free_buddy()`

{{% include "get_free_buddy-flowchart.md" %}}
