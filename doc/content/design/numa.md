---
title: NUMA
layout: default
design_doc: true
revision: 1
status: proposed
---

# NUMA

NUMA stands for Non-Uniform Memory Access and describes that RAM access
for CPUs in a large system is not equally fast for all of them. CPUs
are grouped into so-called nodes and each node has fast access to RAM
that is considered local to its node and slower access to other RAM.
Conceptually, a node is a container that bundles some CPUs and RAM and
there is an associated cost when accessing RAM in a different node.  In
the context of  CPU virtualisation assigning vCPUs to NUMA nodes is an
optimisation strategy to reduce memory latency. This document describes
a design to make NUMA-related assignments for Xen domains (hence, VMs)
visible to the user. Below we refer to these assignments and
optimisations collectively as NUMA for simplicity.

NUMA is more generally discussed as
[NUMA Feature](../toolstack/features/NUMA/index.md).


## NUMA Properties

Xen 4.20 implements NUMA optimisation. We want to expose the following
NUMA-related properties of VMs to API clients, and in particualar
XenCenter. Each one is represented by a new field in XAPI's `VM_metrics`
data model:

* RO `VM_metrics.numa_optimised`: boolean: if the VM is
  optimised for NUMA
* RO `VM_metrics.numa_nodes`: integer: number of NUMA nodes of the host
  the VM is using
* MRO `VM_metrics.numa_node_memory`: int -> int map; mapping a NUMA node
  (int) to an amount of memory (bytes) in that node.

Required NUMA support is only available in Xen 4.20. Some parts of the
code will have to be managed by patches.

## XAPI High-Level Implementation

As far as Xapi clients are concerned, we implement new fields in the
`VM_metrics` class of the data model and surface the values in the CLI
via `records.ml`; we could decide to make `numa_optimised` visible by
default in `xe vm-list`.

Introducing new fields requires defaults; these would be:

* `numa_optimised`: false
* `numa_nodes`: 0
* `numa_node_memory`: []

The data model ensures that the values are visible to API clients.

## XAPI Low-Level Implementation

NUMA properties are observed by Xenopsd and Xapi learns about them as
part of the `Client.VM.stat` call implemented by Xenopsd. Xapi makes
these calls frequently and we will update the Xapi VM fields related to
NUMA simply as part of processing the result of such a call in Xapi.

For this to work, we extend the return type of `VM.stat` in

* `xenops_types.ml`, type `Vm.state`

with three fields:

* `numa_optimised: bool`
* `numa_nodes: int`
* `numa_node_memory: (int, int64) list`

matching the semantics from above.

## Xenopsd Implementation

Xenopsd implements the `VM.stat` return value in

* `Xenops_server_sen.get_state`

where the three fields would be set. Xenopsds relies on bindings to Xen to
observe NUMA-related properties of a domain.

Given that NUMA related functionality is only available for Xen 4.20, we
probably will have to maintain a patch in xapi.spec for compatibility
with earlier Xen versions.

The (existing) C bindings and changes come in two forms: new functions
and an extension of a type used by and existing function.

```ocaml
    external domain_get_numa_info_node_pages_size : handle -> int -> int
      = "stub_xc_domain_get_numa_info_node_pages_size"
```

Thia function reports the number of NUMA nodes used by a Xen domain
(supplied as an argument)

```ocaml
    type domain_numainfo_node_pages = {
      tot_pages_per_node : int64 array;
    }
    external domain_get_numa_info_node_pages :
      handle -> int -> int -> domain_numainfo_node_pages
      = "stub_xc_domain_get_numa_info_node_pages"
```

This function receives as arguments a domain ID and the number of nodes
this domain is using (acquired using `domain_get_numa_info_node_pages`)

The number of NUMA nodes of the host (not domain) is reported by
`Xenctrl.physinfo` which returns a value of type `physinfo`.

```diff
    index b4579862ff..491bd3fc73 100644
    --- a/tools/ocaml/libs/xc/xenctrl.ml
    +++ b/tools/ocaml/libs/xc/xenctrl.ml
    @@ -155,6 +155,7 @@ type physinfo =
         capabilities     : physinfo_cap_flag list;
         max_nr_cpus      : int;
         arch_capabilities : arch_physinfo_cap_flags;
    +    nr_nodes         : int;
       }
```

We are not reporting `nr_nodes`  directly but use it to determine the
value of `numa_optimised` for a domain/VM:

    numa_optimised =
        (VM.numa_nodes = 1)
        or (VM.numa_nodes < physinfo.Xenctrl.nr_nodes)

### Details

The three new fields that become part of type `VM.state` are updated as
part of `get_state()` using the primitives above.



