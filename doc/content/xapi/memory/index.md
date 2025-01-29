+++
title = "Host memory accounting"
linkTitle = "Memory"
+++

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
the variables due to VM configuration as "VM overhead". There is no low-level
API to query this information, therefore xapi will sample the host overheads
at system boot time and model the per-VM overheads.

Host overhead
-------------

The host overhead is not managed by xapi, instead it is sampled. After the host
boots and before any VMs start, xapi asks Xen how much memory the host has in
total, and how much memory is currently free. Xapi subtracts the free from the
total and stores this as the host overhead.

VM overhead
------------

The inputs to the model are

- `VM.memory_static_max`: the maximum amount of RAM the domain will be able to use
- `VM.HVM_shadow_multiplier`: allows the shadow memory to be increased
- `VM.VCPUs_max`: the maximum number of vCPUs the domain will be able to use

First the shadow memory is calculated, in MiB

![Shadow memory in MiB](shadow.svg)

Second the VM overhead is calculated, in MiB

![Memory overhead in MiB](overhead.svg)

Memory required to start a VM
-----------------------------

If ballooning is disabled, the memory required to start a VM is the same as the VM
overhead above.

If ballooning is enabled then the memory calculation above is modified to use the
`VM.memory_dynamic_max` rather than the `VM.memory_static_max`.

Memory required to migrate a VM
-------------------------------

If ballooning is disabled, the memory required to receive a migrating VM is the same
as the VM overhead above.

If ballooning is enabled, then the VM will first be ballooned down to `VM.memory_dynamic_min`
and then it will be migrated across. If the VM fails to balloon all the way down, then
correspondingly more memory will be required on the receiving side.
