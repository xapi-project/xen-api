Host memory accounting
======================

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

TBD

VM overhead
------------

The inputs to the model are
- VM.memory_static_max: the maximum amount of RAM the domain will be able to use
- VM.HVM_shadow_multiplier: allows the shadow memory to be increased
- VM.VCPUs_max: the maximum number of vCPUs the domain will be able to use

First the shadow memory is calculated, in MiB

![Shadow memory in MiB](http://xapi-project.github.io/xen-api/doc/design/shadow.svg)

Second the VM overhead is calculated, in MiB

![Memory overhead in MiB](http://xapi-project.github.io/xen-api/doc/design/overhead.svg)

Memory required to start a VM
-----------------------------

TBD

Memory required to migrate a VM
-------------------------------

TBD
