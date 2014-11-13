Xapi design details
===================

Before reading these, first familiarise yourself with the
[architecture](../architecture/README.md).

- [GPU passthrough](gpu-passthrough.md): explains how discrete GPUs (not vGPUs) can be
  assigned to VMs
- [GRE tunnelling](tunnelling.md): describes the API used to set up GRE tunnels for guest
  network traffic
- [NIC offload](pif-properties.md): describes the API used to configure network offload,
  including GRO
- [The Host Internal Management Network](udhcp.md): shows how the internal management
  network can be used, and how IP addresses are managed.
- [XenAPI evolution](XenAPI-evolution.md): the XenAPI lifecycle i.e. how the XenAPI evolves
  over time
- [memory accounting](memory-accounting.md): explains how the host memory
  is managed, in particular how the "VM overhead" and "host overhead" are
  calculated
- [backtraces](backtraces.md): how xapi collects and reports diagnostic
  backtraces across host and process boundaries
