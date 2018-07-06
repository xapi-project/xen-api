Xenopsd: the developer handbook
===============================

Xenopsd is the [xapi-project](http://github.com/xapi-project) VM manager.
Xenopsd is responsible for
- starting, stopping, suspending, resuming, migrating VMs
- hotplugging and unplugging disks (VBDs)
- hotplugging and unplugging nics (VIFs)
- hotplugging and unplugging PCI devices
- setting up VM consoles
- running bootloaders
- setting QoS parameters
- configuring SMBIOS tables
- handling crashes
etc

Check out the [full features list](features.md).

Principles
----------

1. do no harm: Xenopsd should never touch domains/VMs which it hasn't been
   asked to manage. This means that it can co-exist with other VM managers
   such as 'xl' and 'libvirt'
2. be independent: Xenopsd should be able to work in isolation. In particular
   the loss of some other component (e.g. the network) should not by itself
   prevent VMs being managed locally (including shutdown and reboot).
3. asynchronous by default: Xenopsd exposes task monitoring and offers
   cancellation for all operations. Xenopsd ensures that the system is always
   in a manageable state after an operation has been cancelled.
4. avoid state duplication: where another component owns some state, Xenopsd
   will always defer to it. We will avoid creating out-of-sync caches of
   this state.
5. be debuggable: Xenopsd will expose diagnostic APIs and tools to allow
   its internal state to be inspected and modified.

Contents
--------

- [Architecture](architecture/README.md): read about how Xenopsd fits into
  the overall system; and the major pieces and patterns within Xenopsd.
- [Features](features/README.md): learn about the features supported by Xenopsd and
  how they work.
- [Design](design/README.md): discover the low-level details, formats, protocols,
  concurrency etc.
- [Walk-throughs](walk-throughs/README.md): follow operations end-to-end to
  understand how it all fits together.
- [Futures](futures/README.md): find out how Xenopsd is likely to change and
  how you can help.
