---
title: PCI passthrough support
layout: default
design_doc: true
revision: 1
status: proposed
---

Introduction
------------

GPU passthrough is already available in XAPI, this document proposes to also
offer passthrough for all PCI devices through XAPI.

Design proposal
---------------

New methods for PCI object:
- `PCI.enable_dom0_access`
- `PCI.disable_dom0_access`
- `PCI.get_dom0_access_status`: compares the outputs of `/opt/xensource/libexec/xen-cmdline`
  and `/proc/cmdline` to produce one of the four values that can be currently contained
  in the `PGPU.dom0_access` field:
  - disabled
  - disabled_on_reboot
  - enabled
  - enabled_on_reboot

  How do determine the expected dom0 access state:
  If the device id is present in both `pciback.hide` of `/proc/cmdline` and `xen-cmdline`: `enabled`
  If the device id is present not in both `pciback.hide` of `/proc/cmdline` and `xen-cmdline`: `disabled`
  If the device id is present in the `pciback.hide` of `/proc/cmdline` but not in the one of `xen-cmdline`: `disabled_on_reboot`
  If the device id is not present in the `pciback.hide` of `/proc/cmdline` but is in the one of `xen-cmdline`: `enabled_on_reboot`

  A function rather than a field makes the data always accurate and even accounts for
  changes made by users outside XAPI, directly through `/opt/xensource/libexec/xen-cmdline`

With these generic methods available, the following field and methods will be *deprecated*:
- `PGPU.enable_dom0_access`
- `PGPU.disable_dom0_access`
- `PGPU.dom0_access` (DB field)

They would still be usable and up to date with the same info as for the PCI methods.

Test cases
----------

- hide a PCI:
  - call `PCI.disable_dom0_access` on an `enabled` PCI
  - check the PCI goes in state `disabled_on_reboot`
  - reboot the host
  - check the PCI goes in state `disabled`


- unhide a PCI:
  - call `PCI.enable_dom0_access` on an `disabled` PCI
  - check the PCI goes in state `enabled_on_reboot`
  - reboot the host
  - check the PCI goes in state `enabled`

- get a PCI dom0 access state:
  - on a `enabled` PCI, make sure the `get_dom0_access_status` returns `enabled`
  - hide the PCI
  - make sure the `get_dom0_access_status` returns `disabled_on_reboot`
  - reboot
  - make sure the `get_dom0_access_status` returns `disabled`
  - unhide the PCI
  - make sure the `get_dom0_access_status` returns `enabled_on_reboot`
  - reboot
  - make sure the `get_dom0_access_status` returns `enabled`

- Check PCI/PGPU dom0 access coherence:
  - hide a PCI belonging to a PGPU and make sure both states remains coherent at every step
  - unhide a PCI belonging to a PGPU and make sure both states remains coherent at every step
  - hide a PGPU and make sure its and its PCI's states remains coherent at every step
  - unhide a PGPU and make sure its and its PCI's states remains coherent at every step
