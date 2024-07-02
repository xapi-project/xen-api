---
title: Memory
layout: xenapi
xenapi_tag: memory
---

Memory is used for many things:

- the hypervisor code: this is the Xen executable itself
- the hypervisor heap: this is needed for per-domain structures and per-vCPU
  structures
- the crash kernel: this is needed to collect information after a host crash
- domain RAM: this is the memory the VM believes it has
- shadow memory: for HVM guests running on hosts without hardware assisted
  paging (HAP) Xen uses shadow to optimise page table updates. For all guests
  shadow is used during live migration for tracking the memory transfer.
- video RAM for the virtual graphics card

Some of these are constants (e.g. hypervisor code) while some depend on the VM
configuration (e.g. domain RAM). Xapi calls the constants "host overhead" and
the variables due to VM configuration as "VM overhead".
These overheads are subtracted from free memory on the host when starting,
resuming and migrating VMs.
