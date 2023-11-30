+++
title = "Xenopsd"
weight = 30
+++

Xenopsd is the VM manager of the XAPI Toolstack.
Xenopsd is responsible for:

- Starting, stopping, rebooting, suspending, resuming, migrating VMs.
- (Hot-)plugging and unplugging devices such as VBDs, VIFs, vGPUs and PCI devices.
- Setting up VM consoles.
- Running bootloaders.
- Setting QoS parameters.
- Configuring SMBIOS tables.
- Handling crashes.
- etc.

Check out the [full features list](features.html).

The code is in `ocaml/xenopsd`.

Principles
----------

1. Do no harm: Xenopsd should never touch domains/VMs which it hasn't been
   asked to manage. This means that it can co-exist with other VM managers
   such as 'xl' and 'libvirt'.
2. Be independent: Xenopsd should be able to work in isolation. In particular
   the loss of some other component (e.g. the network) should not by itself
   prevent VMs being managed locally (including shutdown and reboot).
3. Asynchronous by default: Xenopsd exposes task monitoring and offers
   cancellation for all operations. Xenopsd ensures that the system is always
   in a manageable state after an operation has been cancelled.
4. Avoid state duplication: where another component owns some state, Xenopsd
   will always defer to it. We will avoid creating out-of-sync caches of
   this state.
5. Be debuggable: Xenopsd will expose diagnostic APIs and tools to allow
   its internal state to be inspected and modified.
