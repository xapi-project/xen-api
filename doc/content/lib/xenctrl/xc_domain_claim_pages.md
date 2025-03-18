---
title: xc_domain_claim_pages()
description: Stake a claim for further memory for a domain, and release it too.
---

## Purpose

The purpose of `xc_domain_claim_pages()` is to attempt to
stake a claim on an amount of memory for a given domain which guarantees that
memory allocations for the claimed amount will be successful.

The domain can still attempt to allocate beyond the claim, but those are not
guaranteed to be successful and will fail if the domain's memory reaches it's
`max_mem` value.

The aim is to attempt to stake a claim for a domain on a quantity of pages
of system RAM, but _not_ assign specific page frames.
It performs only arithmetic so the hypercall is very fast and not be preempted.
Thereby, it sidesteps any time-of-check-time-of-use races for memory allocation.

## Usage notes

`xc_domain_claim_pages()` returns 0 if the Xen page allocator has atomically
and successfully claimed the requested number of pages, else non-zero.

> [!info]
> Errors returned by `xc_domain_claim_pages()` must be handled as they are a
normal result of the `xenopsd` thread-pool claiming and starting many VMs
in parallel during a boot storm scenario.

> [!warning]
> This is especially important when staking claims on NUMA nodes using an updated
version of this function. In this case, the only options of the calling worker
thread would be to adapt to the NUMA boot storm:
Attempt to find a different NUMA node for claiming the memory and try again.

## Design notes

For reference, see the hypercall comment Xen hypervisor header:
[xen/include/public/memory.h](https://github.com/xen-project/xen/blob/e7e0d485/xen/include/public/memory.h#L552-L578)

The design of the backing hypercall in Xen is as follows:

- A domain can only have one claim.
- Subsequent calls to the backing hypercalls update claim.
- The domain ID is the key of the claim.
- By killing the domain, the claim is also released.
- When sufficient memory has been allocated to resolve the claim,
  the claim silently expires.
- Depending on the given size argument, the remaining stack of the domain
  can be set initially, updated to the given amount, or reset.
- Claiming zero pages effectively resets any outstanding claim and
  is always successful.

### Users

To set up the boot memory of new domain, the [libxenguest](../xenguest) function
[xc_dom_boot_mem_init](../xenguest/xc_dom_boot_mem_init) relies on this design:

The [memory setup](../../../xenopsd/walkthroughs/VM.build/xenguest/setup_mem)
of `xenguest` and by the `xl` CLI using `libxl` use it.
`libxl` actively uses it to stack an initial claim before allocating memory.

> [!note]
> The functions
> [meminit_hvm()](https://github.com/xen-project/xen/blob/39c45c/tools/libs/guest/xg_dom_x86.c#L1348-L1648)
> and `meminit_pv` used by
> [xc_dom_boot_mem_init()](../xenguest/xc_dom_boot_mem_init)
> always destroy any remaining claim before they return.
> Thus, after [libxenguest](../xenguest) has completed allocating and populating
> the physical memory of the domain, no domain has a remaining claim!

> [!warning]
> From this point onwards, no memory allocation is guaranteed!
> While swapping memory between domains can be expected to always succeed,
> Allocating a new page after freeing another can always fail unless a
> privileged domain stakes a claim for such allocations beforehand!

### Implementation

The Xen upstream memory claims code is implemented to work as follows:

When allocating memory, while a domain has an outstanding claim,
Xen updates remaining claim accordingly.

If a domain has a stake from claim, when memory is freed,
freed amount of memory increases the stake of memory.

> [!note] Note: This detail may have to change for implementing NUMA claims
> Xen doesn't know if a page was allocated using a NUMA node claim.
> Therefore, it cannot know if it would be legal to increment a stake a NUMA
> node claim when freeing pages.

> [!info]
> The smallest possible change to achieve a similar effect would be
> to add a field to the Xen hypervisor's domain struct.
> It would be used for remembering the last total NUMA node claim.
> With it, freeing memory from a NUMA node could attempt to increase
> the outstanding claim on the claimed NUMA node, but only given this
> amount is available and not claimed.

## Management of claims

- The stake is centrally managed by the Xen hypervisor using a
  [Hypercall](https://wiki.xenproject.org/wiki/Hypercall).
- Claims are not reflected in the amount of free memory reported by Xen.

## Reporting of claims

- `xl claims` reports the outstanding claims of the domains:
  > [!info] Sample output of `xl claims`:
  > ```js
  > Name         ID   Mem VCPUs      State   Time(s)  Claimed
  > Domain-0      0  2656     8     r-----  957418.2     0
  > ```
- `xl info` reports the host-wide outstanding claims:
  > [!info] Sample output from `xl info | grep outstanding`:
  > ```js
  > outstanding_claims     : 0
  > ```

## Tracking of claims

Xen only tracks:
- the outstanding claims of each domain and
- the outstanding host-wide claims.

Claiming zero pages effectively cancels the domain's outstanding claim
and is always successful.

> [!info]
> - Allocations for outstanding claims are expected to always be successful.
> - But this reduces the amount of outstanding claims if the domain.
> - Freeing memory of the domain increases the domain's claim again:
>   - But, when a domain consumes its claim, it is reset.
>   - When the claim is reset, freed memory is longer moved to the outstanding claims!
>   - It would have to get a new claim on memory to have spare memory again.

> [!warning] The domain's `max_mem` value is used to deny memory allocation
> If an allocation would cause the domain to exceed it's `max_mem`
> value, it will always fail.

## Hypercall API

Function signature of the libXenCtrl function to call the Xen hypercall:

```c
long xc_memory_op(libxc_handle, XENMEM_claim_pages, struct xen_memory_reservation *)
```

`struct xen_memory_reservation` is defined as :

```c
struct xen_memory_reservation {
    .nr_extents   = nr_pages, /* number of pages to claim */
    .extent_order = 0,        /* an order 0 means: 4k pages, only 0 is allowed */
    .mem_flags    = 0,        /* no flags, only 0 is allowed (at the moment) */
    .domid        = domid     /* numerical domain ID of the domain */
};
```

## Current users

It is used by [libxenguest](../xenguest), which is used at least by:
- [xenguest](../../../xenopsd/walkthroughs/VM.build/xenguest)
- `libxl`/the `xl` CLI.

### <tt>libxl</tt> and the <tt>xl</tt> CLI

The `xl` cli uses claims actively. By default, it enables `libxl` to pass
the `struct xc_dom_image` with its `claim_enabled` field set.

The functions dispatched by [xc_boot_mem_init()](../xenguest/xc_dom_boot_mem_init)
then attempt to claim the boot using `xc_domain_claim_pages()`.
They also (unconditionally) destroy any open claim upon return.

This means that in case the claim fails, `xl` avoids:
- The effort of allocating the memory, thereby not blocking it for other domains.
- The effort of potentially needing to scrub the memory after the build failure.

## Updates: Improved NUMA memory allocation

### Enablement of a NUMA node claim instead of a host-wide claim

With a
[proposed update](https://lore.kernel.org/xen-devel/20250314172502.53498-1-alejandro.vallejo@cloud.com/T/#mc2b0d3a8b994708da1ac199df3262c912c559efd)
of `xc_domain_claim_pages()` for NUMA node claims, a node argument is added.

It can be `XC_NUMA_NO_NODE` for defining a host-wide claim or a NUMA node
for staking a claim on one NUMA node.

This update does not change the foundational design of memory claims of
the Xen hypervisor where a claim is defined as a single claim for the domain.

For improved support for [NUMA](../../../toolstack/features/NUMA/), `xenopsd`
is updated to call an updated version of this function for the domain.

It reserves NUMA node memory before `xenguest` is called, and a new `pnode`
argument is added to `xenguest`.

It sets the NUMA node for memory allocations by the xenguest
[boot memory setup](../../../xenopsd/walkthroughs/VM.build/xenguest/setup_mem)
[xc_boot_mem_init()](../xenguest/xc_dom_boot_mem_init) which also
sets the `exact` flag.

The `exact` flag forces [get_free_buddy()](../xen/get_free_buddy) to fail
if it could not find scrubbed memory for the give `pnode` which causes
[alloc_heap_pages()](./xc_domain_populate_physmap#alloc_heap_pages)
to re-run [get_free_buddy()](../xen/get_free_buddy) with the flag to
fall back to dirty, not yet scrubbed memory.

[alloc_heap_pages()](./xc_domain_populate_physmap#alloc_heap_pages) then
checks each 4k page for the need to scrub and scrubs those before returning them.

This is expected to improve the issue of potentially spreading memory over
all NUMA nodes in case of parallel boot by preventing one NUMA node to become
the target of parallel memory allocations which cannot fit on it and also the
issue of spreading memory over all NUMA nodes if case of a huge amount of
dirty memory that needs to be scrubbed for before a domain can start or restart.

### NUMA grant tables and ballooning

This is not seen as an issue as grant tables and I/O pages are usually not as
frequently used as regular system memory of domains, but this potential issue
remains:

In discussions, it was said that Windows PV drivers `unmap` and `free` memory
for grant tables to Xen and then re-allocate memory for those grant tables.

`xenopsd` may want to try to stake a very small claim for the domain on the
NUMA node of the domain so that Xen can increase this claim when the PV drivers
`free` this memory and re-use the resulting claimed amount for allocating
the grant tables.

This would ensure that the grant tables are then allocated on the local NUMA
node of the domain, avoiding remote memory accesses when accessing the grant
tables from inside the domain.

Note: In case the corresponding backend process in Dom0 is running on another
NUMA node, it would access the domain's grant tables from a remote NUMA node,
but in this would enable a future improvement for Dom0, where it could prefer to
run the corresponding backend process on the same or a neighbouring NUMA node.
