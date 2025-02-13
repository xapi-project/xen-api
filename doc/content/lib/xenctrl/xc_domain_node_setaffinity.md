---
title: xc_domain_node_setaffinity()
description: Set a Xen domain's NUMA node affinity
---

`xc_domain_node_setaffinity()` controls the NUMA node affinity of a domain.

By default, Xen enables the `auto_node_affinity` feature flag,
where setting the vCPU affinity also sets the NUMA node affinity for
memory allocations to be aligned with the vCPU affinity of the domain.

Setting the NUMA node affinity using this call can be used,
for example, when there might not be enough memory on the
preferred NUMA node, but there are other NUMA nodes that have
enough free memory to be used for the system memory of the domain.

In terms of future NUMA design, it might be even more favourable to
have a strategy in `xenguest` where in such cases, the superpages
of the preferred node are used first and a fallback to neighbouring
NUMA nodes only happens to the extent necessary.

Likely, the future allocation strategy should be passed to `xenguest`
using Xenstore like the other platform parameters for the VM.

## Walk-through of xc_domain_node_setaffinity()

```mermaid
classDiagram
class `xc_domain_node_setaffinity()` {
    +xch: xc_interface #42;
    +domid: uint32_t
    +nodemap: xc_nodemap_t
    0(on success)
    -EINVAL(if a node in the nodemask is not online)
}
click `xc_domain_node_setaffinity()` href "
https://github.com/xen-project/xen/blob/master/tools/libs/ctrl/xc_domain.c#L122-L158"

`xc_domain_node_setaffinity()` --> `Xen hypercall: do_domctl()`
`xc_domain_node_setaffinity()` <-- `Xen hypercall: do_domctl()`
class `Xen hypercall: do_domctl()` {
    Calls domain_set_node_affinity#40;#41; and returns its return value
    Passes: domain (struct domain *, looked up using the domid)
    Passes: new_affinity (modemask, converted from xc_nodemap_t)
}
click `Xen hypercall: do_domctl()` href "
https://github.com/xen-project/xen/blob/master/xen/common/domctl.c#L516-L525"

`Xen hypercall: do_domctl()` --> `domain_set_node_affinity()`
`Xen hypercall: do_domctl()` <-- `domain_set_node_affinity()`
class `domain_set_node_affinity()` {
    domain: struct domain
    new_affinity: nodemask
    0(on success, the domain's node_affinity is updated)
    -EINVAL(if a node in the nodemask is not online)
}
click `domain_set_node_affinity()` href "
https://github.com/xen-project/xen/blob/master/xen/common/domain.c#L943-L970"
```

### domain_set_node_affinity()

This function implements the functionality of `xc_domain_node_setaffinity`
to set the NUMA affinity of a domain as described above.
If the new_affinity does not intersect the `node_online_map`,
it returns `-EINVAL`, otherwise on success `0`.

When the `new_affinity` is a specific set of NUMA nodes, it updates the NUMA
`node_affinity` of the domain to these nodes and disables `auto_node_affinity`
for this domain. It also notifies the Xen scheduler of the change.

This sets the preference the memory allocator to the new NUMA nodes,
and in theory, it could also alter the behaviour of the scheduler.
This of course depends on the scheduler and its configuration.

## Notes on future design improvements

This call cannot influence the past: The `xenopsd`
[VM_create](../../xenopsd/walkthroughs/VM.start.md#2-create-a-xen-domain)
micro-ops calls `Xenctrl.domain_create`. It currently creates
the domain's data structures before `numa_placement` was done.

Improving `Xenctrl.domain_create` to pass a NUMA node
for allocating the Hypervisor's data structures (e.g. vCPU)
of the domain would require changes
to the Xen hypervisor and the `xenopsd`
[xenopsd VM_create](../../xenopsd/walkthroughs/VM.start.md#2-create-a-xen-domain)
micro-op.
