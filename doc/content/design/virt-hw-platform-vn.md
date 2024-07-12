---
title: Virtual Hardware Platform Version
layout: default
design_doc: true
revision: 1
status: released (7.0)
---

### Background and goal

Some VMs can only be run on hosts of sufficiently recent versions.

We want a clean way to ensure that xapi only tries to run a guest VM on a host that supports the "virtual hardware platform" required by the VM.

### Suggested design

* In the datamodel, VM has a new integer field "hardware_platform_version" which defaults to zero.
* In the datamodel, Host has a corresponding new integer-list field "virtual_hardware_platform_versions" which defaults to list containing a single zero element (i.e. `[0]` or `[0L]` in OCaml notation). The zero represents the implicit version supported by older hosts that lack the code to handle the Virtual Hardware Platform Version concept.
* When a host boots it populates its own entry from a hardcoded value, currently `[0; 1]` i.e. a list containing the two integer elements `0` and `1`. (Alternatively this could come from a config file.)
  * If this new version-handling functionality is introduced in a hotfix, at some point the pool master will have the new functionality while at least one slave does not. An old slave-host that does not yet have software to handle this feature will not set its DB entry, which will therefore remain as `[0]` (maintained in the DB by the master).
* The existing test for whether a VM can run on (or migrate to) a host must include a check that the VM's virtual hardware platform version is in the host's list of supported versions.
* When a VM is made to start using a feature that is available only in a certain virtual hardware platform version, xapi must set the VM's hardware_platform_version to the maximum of that version-number and its current value (i.e. raise if needed).

For the version we could consider some type other than integer, but a strict ordering is needed.

### First use-case

Version 1 denotes support for a certain feature:

> When a VM starts, if a certain flag is set in VM.platform then XenServer will provide an emulated PCI device which will trigger the guest Windows OS to seek drivers for the device, or updates for those drivers. Thus updated drivers can be obtained through the standard Windows Update mechanism.

If the PCI device is removed, the guest OS will fail to boot. A VM using this feature must not be migrated to or started on a XenServer that lacks support for the feature.

Therefore at VM start, we can look at whether this feature is being used; if it is, then if the VM's Virtual Hardware Platform Version is less than 1 we should raise it to 1.

### Limitation
Consider a VM that requires version 1 or higher. Suppose it is exported, then imported into an old host that does not support this feature. Then the host will not check the versions but will attempt to run the VM, which will then have difficulties.

The only way to prevent this would be to make a backwards-incompatible change to the VM metadata (e.g. a new item in an enum) so that the old hosts cannot read it, but that seems like a bad idea.
