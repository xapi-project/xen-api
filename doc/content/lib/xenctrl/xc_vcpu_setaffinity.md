---
title: xc_vcpu_setaffinity()
description: Set a Xen vCPU's pCPU affinity and the domain's NUMA node affinity
mermaid:
    force: true
---
## Purpose

The libxenctrl library call `xc_set_vcpu_affinity()`
controls the pCPU affinity of the given vCPU.

[xenguest](../../../xenopsd/walkthroughs/VM.build/xenguest/#walkthrough-of-the-xenguest-build-mode)
uses it when building domains if
[xenopsd](../../xenopsd/walkthroughs/VM.build/Domain.build)
added vCPU affinity information to the XenStore platform data path
`platform/vcpu/#domid/affinity` of the domain.

### Updating the NUMA node affinity of a domain

Besides that, `xc_set_vcpu_affinity()` can also modify the NUMA node
affinity of the Xen domain if the vCPU:

When Xen creates a domain, it enables the domain's `d->auto_node_affinity`
feature flag.

When it is enabled, setting the vCPU affinity also updates the NUMA node
affinity which is used for memory allocations for the domain:

### Simplified flowchart

{{% include "xc_vcpu_setaffinity-simplified.md" %}}

## Current use by xenopsd and xenguest

When `Host.numa_affinity_policy` is set to
[best_effort](../../../toolstack/features/NUMA/#xapi-datamodel-design),
[xenopsd](../../../xenopsd/walkthroughs/VM.build) attempts NUMA node placement
when building new VMs and instructs
[xenguest](../../../xenopsd/walkthroughs/VM.build/xenguest/#walkthrough-of-the-xenguest-build-mode)
to set the vCPU affinity of the domain.

With the domain's `auto_node_affinity` flag enabled by default in Xen,
this automatically also sets the `d->node_affinity` mask of the domain.

This then causes the Xen memory allocator to prefer the NUMA nodes in the
`d->node_affinity` NUMA node mask when allocating memory.

That is, (for completeness) unless Xen's allocation function
`alloc_heap_pages()` receives a specific NUMA node in its `memflags`
argument when called.

See [xc_domain_node_setaffinity()](xc_domain_node_setaffinity) for more
information about another way to set the `node_affinity` NUMA node mask
of Xen domains and more depth on how it is used in Xen.

### Flowchart of its current use for NUMA affinity

{{% include "xc_vcpu_setaffinity-xenopsd.md" %}}
