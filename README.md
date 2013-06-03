xenopsd: a simple VM manager
============================

xenopsd manages VMs running

  * on Xen, via direct libxc calls
  * on Xen/KVM via libvirt [experimental]
  * on KVM via qemu directly [experimental]

and provides a simple RPC control interface to the layer above (typically xapi).

