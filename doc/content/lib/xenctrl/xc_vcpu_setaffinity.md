---
title: xc_vcpu_setaffinity()
description: Set a Xen vCPU's pCPU affinity and the domain's NUMA node affinity
mermaid:
    force: true
---
## Introduction

In the Xen hypervisor, each vCPU has:

- A _soft affinity_, This is the list of pCPUs where a vCPU prefers to run:

  This can be used in cases to make vCPUs prefer to run on a set on pCPUs,
  for example the pCPUs of a NUMA node, but in case those are already busy,
  the Credit schedule can still ignore the soft-affinity.
  A typical use case for this are NUMA machines, where the soft affinity
  for the vCPUs of a domain should be set equal to the pCPUs of the NUMA node where the domain's memory shall be placed.

  See the description of the [NUMA feature](../../../toolstack/features/NUMA/)
  for more details.

- A _hard affinity_, also known as pinning.
  This is the list of pCPUs where a vCPU is allowed to run

  Hard affinity is currently not used for NUMA placement, but can be configured
  manually for a given domain, either using `xe VCPUs-params:mask=` or the API.

  For example, the vCPU’s pinning can be configured for a VM with:[^1]
  [^1]: The VM parameter
  [VCPUs-params:mask](https://docs.xenserver.com/en-us/citrix-hypervisor/command-line-interface.html#vm-parameters)
  is documented in the official XAPI user documentation.

  ```py
  xe vm-param-set uuid=<template_uuid> vCPUs-params:mask=1,2,3
  ```

  There are also host-level `guest_VCPUs_params` which are used by
  `host-cpu-tune` to exclusively pin Dom0 and guests (i.e. that their
  pCPUs never overlap). Note: This isn't currently supported by the
  NUMA code: It could result that the NUMA placement picks a node that
  has reduced capacity or unavailable due to the host mask that
  `host-cpu-tune` has set.

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

{{% include "xc_vcpu_setaffinity-xenopsd-notes.md" %}}
{{% include "xc_vcpu_setaffinity-xenopsd.md" %}}
