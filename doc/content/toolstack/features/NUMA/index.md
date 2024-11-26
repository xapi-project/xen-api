+++
title = "NUMA"
+++

## NUMA in a nutshell

Systems that contain more than one CPU socket are typically built on a Non-Uniform Memory Architecture (NUMA) [^xen_numa][^kernel_numa].
In a NUMA system each node has fast, lower latency access to local memory.

![hwloc](hwloc.svg)

In the diagram [^lstopo] above we have 4 NUMA nodes:
 * 2 of those are due to 2 separate physical packages (sockets)
 * a further 2 is due to Sub-NUMA-Clustering (aka Nodes Per Socket for AMD) where the L3 cache is split

The L3 cache is shared among multiple cores, but cores `0-5` have lower latency access to one part of it, than cores `6-11`, and this is also reflected by splitting memory addresses into 4 31GiB ranges in total.

In the diagram the closer the memory is to the core, the lower the access latency:
   * per-core caches: L1, L2
   * per-package shared cache: L3 (local part), L3 (remote part)
   * local NUMA node (to a group of cores, e.g. `L#0 P#0`), node 0
   * remote NUMA node in same package (`L#1 P#2`), node 1
   * remote NUMA node in other packages (`L#2 P#1` and 'L#3P#3'), node 2 and 3

### The NUMA distance matrix 

Accessing remote NUMA node in the other package has to go through a shared interconnect, which has lower bandwidth than the direct connections, and also a bottleneck if both cores have to access remote memory: the bandwidth for a single core is effectively at most half.

This is reflected in the NUMA distance/latency matrix.
The units are arbitrary, and by convention access latency to the local NUMA node is given distance '10'.
 
Relative latency matrix by logical indexes:

| index |   0 |   2 |   1 |  3 |
| ----- | --- | --- | --- | ---|
|    0  |  10 |   21|   11|  21|
|    2  |  21 |   10|   21|  11|
|    1  |  11 |   21|   10|  21|
|    3  |  21 |   11|   21|  10|

This follows the latencies described previously:
   * fast access to local NUMA node memory (by definition), node 0, cost 10
   * slightly slower access latency to the other NUMA node in same package, node 1, cost 11
   * twice as slow access latency to remote NUMA memory in the other physical package (socket): nodes 2 and 3, cost 21

There is also I/O NUMA where a cost is similarly associated to where a PCIe is plugged in, but exploring that is future work (it requires exposing NUMA topology to the Dom0 kernel to benefit from it), and for simplicity the diagram above does not show it.

## Advantages of NUMA

NUMA does have advantages though: if each node accesses only its local memory, then each node can independently achieve maximum throughput.

For best performance, we should:
   - minimize the amount of interconnect bandwidth we are using
   - run code that accesses memory allocated on the closest NUMA node
   - maximize the number of NUMA nodes that we use in the system as a whole

If a VM's memory and vCPUs can entirely fit within a single NUMA node then we should tell Xen to prefer to allocate memory from and run the vCPUs on a single NUMA node.

## Xen vCPU soft-affinity

The Xen scheduler supports 2 kinds of constraints:
* hard pinning: a vCPU may only run on the specified set of pCPUs and nowhere else
* soft pinning: a vCPU is *preferably* run on the specified set of pCPUs, but if they are all busy then it may run elsewhere

Hard pinning can be used to partition the system. But, it can potentially leave part of the system idle while another part is bottlenecked by many vCPUs competing for the same limited set of pCPUs.

Xen does not migrate workloads between NUMA nodes on its own (the Linux kernel can). Although, it is possible to achieve a similar effect with explicit migration.
However, migration introduces additional delays and is best avoided for entire VMs.

Therefore, soft pinning is preferred: Running on a potentially suboptimal pCPU that uses remote memory could still be better than not running it at all until a pCPU is free to run it.

Xen will also allocate memory for the VM according to the vCPU (soft) pinning: If the vCPUs are pinned to NUMA nodes A and B, Xen allocates memory from NUMA nodes A and B in a round-robin way, resulting in interleaving.

### Current default: No vCPU pinning

By default, when no vCPU pinning is used, Xen interleaves memory from all NUMA nodes. This averages the memory performance, but individual tasks' performance may be significantly higher or lower depending on which NUMA node the application may have "landed" on.
As a result, restarting processes will speed them up or slow them down as address space randomization picks different memory regions inside a VM.

This uses the memory bandwidth of all memory controllers and distributes the load across all nodes.
However, the memory latency is higher as the NUMA interconnects are used for most memory accesses and vCPU synchronization within the Domains.

Note that this is not the worst case: the worst case would be for memory to be allocated on one NUMA node, but the vCPU always running on the furthest away NUMA node.

## Best effort NUMA-aware memory allocation for VMs


### Summary

The best-effort mode attempts to fit Domains into NUMA nodes and to balance memory usage.
It soft-pins Domains on the NUMA node with the most available memory when adding the Domain.
Memory is currently allocated when booting the VM (or while constructing the resuming VM).

Parallel boot issue: Memory is not pre-allocated on creation, but allocated during boot.
The result is that parallel VM creation and boot can exhaust the memory of NUMA nodes.

### Goals

By default, Xen stripes the VM's memory across all NUMA nodes of the host, which means that every VM has to go through all the interconnects.
The goal here is to find a better allocation than the default, not necessarily an optimal allocation.
An optimal allocation would require knowing what VMs you would start/create in the future, and planning across hosts.
This allows the host to use all NUMA nodes to take advantage of the full memory bandwidth available on the pool hosts.

Overall, we want to balance the VMs across NUMA nodes, such that we use all NUMA nodes to take advantage of the maximum memory bandwidth available on the system.
For now this proposed balancing will be done only by balancing memory usage: always heuristically allocating VMs on the NUMA node that has the most available memory.
For now, this allocation has a race condition: This happens when multiple VMs are booted in parallel, because we don't wait until Xen has constructed the domain for each one (that'd serialize domain construction, which is currently parallel).
This may be improved in the future by having an API to query Xen where it has allocated the memory, and to explicitly ask it to place memory on a given NUMA node (instead of best_effort).

If a VM doesn't fit into a single node then it is not so clear what the best approach is.
One criteria to consider is minimizing the NUMA distance between the nodes chosen for the VM.
Large NUMA systems may not be fully connected in a mesh requiring multiple hops to each a node, or even have asymmetric links, or links with different bandwidth.
The specific NUMA topology is provided by the ACPI SLIT table as the matrix of distances between nodes.
It is possible that 3 NUMA nodes have a smaller average/maximum distance than 2, so we need to consider all possibilities.

For N nodes there would be 2^N possibilities, so [Topology.NUMA.candidates] limits the number of choices to 65520+N (full set of 2^N possibilities for 16 NUMA nodes, and a reduced set of choices for larger systems).

### Implementation

[Topology.NUMA.candidates] is a sorted sequence of node sets, in ascending order of maximum/average distances.
Once we've eliminated the candidates not suitable for this VM (that do not have enough total memory/pCPUs) we are left with a monotonically increasing sequence of nodes.
There are still multiple possibilities with same average distance.
This is where we consider our second criteria - balancing - and pick the node with most available free memory.

Once a suitable set of NUMA nodes are picked we compute the CPU soft affinity as the union of the CPUs from all these NUMA nodes.
If we didn't find a solution then we let Xen use its default allocation.

The "distances" between NUMA nodes may not all be equal, e.g. some nodes may have shorter links to some remote NUMA nodes, while others may have to go through multiple hops to reach it.
See page 13 in [^AMD_numa] for a diagram of an AMD Opteron 6272 system.

## Limitations and tradeoffs

* Booting multiple VMs in parallel will result in potentially allocating both on the same NUMA node (race condition) 
* When we're about to run out of host memory we'll fall back to striping memory again, but the soft affinity mask won't reflect that (this needs an API to query Xen on where it has actually placed the VM, so we can fix up the mask accordingly)
* XAPI is not aware of NUMA balancing across a pool. Xenopsd chooses NUMA nodes purely based on amount of free memory on the NUMA nodes of the host, even if a better NUMA placement could be found on another host
* Very large (>16 NUMA nodes) systems may only explore a limited number of choices (fit into a single node vs fallback to full interleaving)
* The exact VM placement is not yet controllable
* Microbenchmarks with a single VM on a host show both performance improvements and regressions on memory bandwidth usage: previously a single VM may have been able to take advantage of the bandwidth of both NUMA nodes if it happened to allocate memory from the right places, whereas now it'll be forced to use just a single node.
   As soon as you have more than 1 VM that is busy on a system enabling NUMA balancing should almost always be an improvement though.
* It is not supported to combine hard vCPU masks with soft affinity: if hard affinities are used, then no NUMA scheduling is done by the toolstack, and we obey exactly what the user has asked for with hard affinities.
   This shouldn't affect other VMs since the memory used by hard-pinned VMs will still be reflected in overall less memory available on individual NUMA nodes.
* Corner case: the ACPI standard allows certain NUMA nodes to be unreachable (distance `0xFF` = `-1` in the Xen bindings).
   This is not supported and will cause an exception to be raised.
   If this is an issue in practice the NUMA matrix could be pre-filtered to contain only reachable nodes.
   NUMA nodes with 0 CPUs *are* accepted (it can result from hard affinity pinning)
* NUMA balancing is not considered during HA planning
* Dom0 is a single VM that needs to communicate with all other VMs, so NUMA balancing is not applied to it (we'd need to expose NUMA topology to the Dom0 kernel, so it can better allocate processes)
* IO NUMA is out of scope for now

## XAPI datamodel design

* New API field: `Host.numa_affinity_policy`. 
* Choices: `default_policy`, `any`, `best_effort`.
* On upgrade the field is set to `default_policy`
* Changes in the field only affect newly (re)booted VMs, for changes to take effect on existing VMs a host evacuation or reboot is needed

There may be more choices in the future (e.g. `strict`, which requires both Xen and toolstack changes).

Meaning of the policy:
* `any`: the Xen default where it allocated memory by striping across NUMA nodes
* `best_effort`: the algorithm described in this document, where soft pinning is used to achieve better balancing and lower latency
* `default_policy`: when the admin hasn't expressed a preference

* Currently, `default_policy` is treated as `any`, but the admin can change it, and then the system will remember that change across upgrades.
   If we didn't have a `default_policy` then changing the "default" policy on an upgrade would be tricky: we either risk overriding an explicit choice of the admin, or existing installs cannot take advantage of the improved performance from `best_effort`
* Future XAPI versions may change `default_policy` to mean `best_effort`.
   Admins can still override it to `any` if they wish on a host by host basis.

It is not expected that users would have to change `best_effort`, unless they run very specific workloads, so a pool level control is not provided at this moment.

There is also no separate feature flag: this host flag acts as a feature flag that can be set through the API without restarting the toolstack.
Although obviously only new VMs will benefit.

Debugging the allocator is done by running `xl vcpu-list` and investigating the soft pinning masks, and by analyzing `xensource.log`.

### Xenopsd implementation

See the documentation in [softaffinity.mli] and [topology.mli].

* [Softaffinity.plan] returns a [CPUSet] given a host's NUMA allocation state and a VM's NUMA allocation request.
* [Topology.CPUSet] provides helpers for operating on a set of CPU indexes.
* [Topology.NUMAResource] is a [CPUSet] and the free memory available on a NUMA node.
* [Topology.NUMARequest] is a request for a given number of vCPUs and memory in bytes.
* [Topology.NUMA] represents a host's NUMA allocation state.
* [Topology.NUMA.candidates] are groups of nodes orderd by minimum average distance.
The sequence is limited to [N+65520], where [N] is the number of NUMA nodes.
This avoids exponential state space explosion on very large systems (>16 NUMA nodes).
* [Topology.NUMA.choose] will choose one NUMA node deterministically, while trying to keep overall NUMA node usage balanced.
* [Domain.numa_placement] builds a [NUMARequest] and uses the above [Topology] and [Softaffinity] functions to compute and apply a plan.

We used to have a `xenopsd.conf` configuration option to enable NUMA placement, for backwards compatibility this is still supported, but only if the admin hasn't set an explicit policy on the Host.
It is best to remove the experimental `xenopsd.conf` entry though, a future version may completely drop it.

Tests are in [test_topology.ml] which checks balancing properties and whether the plan has improved best/worst/average-case access times in a simulated test based on 2 predefined NUMA distance matrixes (one from Intel and one from an AMD system).

## Future work

* Enable 'best_effort' mode by default once more testing has been done
* Add an API to query Xen for the NUMA node memory placement (where it has actually allocated the VM's memory).
   Currently, only the `xl debug-keys` interface exists which is not supported in production as it can result in killing the host via the watchdog, and is not a proper API, but a textual debug output with no stability guarantees.
* More host policies, e.g. `strict`.
   Requires the XAPI pool scheduler to be NUMA aware and consider it as part of choosing hosts.
* VM level policy that can set a NUMA affinity index, mapped to a NUMA node modulo NUMA nodes available on the system (this is needed so that after migration we don't end up trying to allocate vCPUs to a non-existent NUMA node)
* VM level anti-affinity rules for NUMA placement (can be achieved by setting unique NUMA affinity indexes)

[^xen_numa]: [Xen on NUMA Machines](https://wiki.xenproject.org/wiki/Xen_on_NUMA_Machines)
[^kernel_numa]: [What is NUMA?](https://www.kernel.org/doc/html/v6.6/mm/numa.html)
[^kernel_numa_perf]: [NUMA memory performance](https://www.kernel.org/doc/html/v6.6/admin-guide/mm/numaperf.html)
[^kernel_numa_policy]: [NUMA memory policy](https://www.kernel.org/doc/html/v6.6/admin-guide/mm/numa_memory_p
[^AMD_numa]: Lepers, Baptiste. ["Improving performance on NUMA systems."](https://theses.hal.science/tel-01549294/document) PhD diss., Université de Grenoble, 2014.
[^lstopo]: created with `lstopo-no-graphics --no-io --of svg --vert=L3 >hwloc.svg` on a bare metal Linux
