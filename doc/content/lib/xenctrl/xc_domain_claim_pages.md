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

Each domain can only have one claim, and the domid is the key of the claim.
By killing the domain, the claim is also released.

Depending on the given size argument, the remaining stack of the domain
can be set initially, updated to the given amount, or reset to no claim (0).

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


## Implementation

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

### Concurrency

Xen protects the consistency of the stake of the domain
using the domain's `page_alloc_lock` and the global `heap_lock` of Xen.
Thse spin-locks prevent any "time-of-check-time-of-use" races.
As the hypercall needs to take those spin-locks, it cannot be preempted.

### Return value

The call returns 0 if the hypercall successfully claimed the requested amount
of memory, else it returns non-zero.

## Current users

### <tt>libxl</tt> and the <tt>xl</tt> CLI

If the `struct xc_dom_image` passed by `libxl` to the
[libxenguest](https://github.com/xen-project/xen/tree/master/tools/libs/guest)
functions
[meminit_hvm()](https://github.com/xen-project/xen/blob/de0254b9/tools/libs/guest/xg_dom_x86.c#L1348-L1649)
and
[meminit_pv()](https://github.com/xen-project/xen/blob/de0254b9/tools/libs/guest/xg_dom_x86.c#L1183-L1333)
has it's `claim_enabled` field set, they,
before allocating the domain's system memory using the allocation function
[xc_populate_physmap()](https://github.com/xen-project/xen/blob/de0254b9/xen/common/memory.c#L159-L314) which calls the hypercall to allocate and populate
the domain's main system memory, will attempt to claim the to-be allocated
memory using a call to `xc_domain_claim_pages()`.
In case this fails, they do not attempt to continue and return the error code
of `xc_domain_claim_pages()`.

Both functions also (unconditionally) reset the claim upon return.

But, the `xl` CLI uses this functionality (unless disabled in `xl.conf`)
to make building the domains fail to prevent running out of memory inside
the `meminit_hvm` and `meminit_pv` calls.
Instead, they immediately return an error.

This means that in case the claim fails, `xl` avoids:
- The effort of allocating the memory, thereby not blocking it for other domains.
- The effort of potentially needing to scrub the memory after the build failure.

### xenguest

While [xenguest](../../../xenopsd/walkthroughs/VM.build/xenguest) calls the
[libxenguest](https://github.com/xen-project/xen/tree/master/tools/libs/guest)
functions
[meminit_hvm()](https://github.com/xen-project/xen/blob/de0254b9/tools/libs/guest/xg_dom_x86.c#L1348-L1649)
and
[meminit_pv()](https://github.com/xen-project/xen/blob/de0254b9/tools/libs/guest/xg_dom_x86.c#L1183-L1333)
like `libxl` does, it does not set
[struct xc_dom_image.claim_enabled](https://github.com/xen-project/xen/blob/de0254b9/tools/include/xenguest.h#L186),
so it does not enable the first call to `xc_domain_claim_pages()`
which would claim the amount of memory that these functions will
attempt to allocate and populate for the domain.

#### Future design ideas for improved NUMA support

For improved support for [NUMA](../../../toolstack/features/NUMA/), `xenopsd`
may want to call an updated version of this function for the domain, so it has
a stake on the NUMA node's memory before `xenguest` will allocate for the domain
before assigning an NUMA node to a new domain.

Further, as PV drivers `unmap` and `free` memory for grant tables to Xen and
then re-allocate memory for those grant tables, `xenopsd` may want to try to
stake a very small claim for the domain on the NUMA node of the domain so that
Xen can increase this claim when the PV drivers `free` this memory and re-use
the resulting claimed amount for allocating the grant tables. This would ensure
that the grant tables are then allocated on the local NUMA node of the domain,
avoiding remote memory accesses when accessing the grant tables from inside
the domain.

Note: In case the corresponding backend process in Dom0 is running on another
NUMA node, it would access the domain's grant tables from a remote NUMA node,
but in this would enable a future improvement for Dom0, where it could prefer to
run the corresponding backend process on the same or a neighbouring NUMA node.
