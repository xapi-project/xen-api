---
title: Notes for the flowchart on the use of setaffinity for VM.start
hidden: true
---
In the flowchart, two code paths are set in bold:
- Show the path when `Host.numa_affinity_policy` is the default (off) in `xenopsd`.
- Show the default path of `xc_vcpu_setaffinity(XEN_VCPUAFFINITY_SOFT)` in Xen,
  when the Domain's `auto_node_affinity` flag is enabled (default) to show
  how it changes to the vCPU affinity update the domain's `node_affinity`
  in this default case as well.

[xenguest](../../xenopsd/walkthroughs/VM.build/xenguest/) uses the Xenstore
to read the static domain configuration that it needs reads to build the domain.
