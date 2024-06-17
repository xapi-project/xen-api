---
title: Multiple device emulators
layout: default
design_doc: true
revision: 1
status: proposed
---

Xen's `ioreq-server` feature allows for several device emulator
processes to be attached to the same domain, each emulating different
sets of virtual hardware.   This makes it possible, for example, to
emulate network devices in a separate process for improved security
and isolation, or to provide special purpose emulators for particular
virtual hardware devices.

`ioreq-server` is currently used in XenServer to support vGPU, where it
is configured via the legacy toolstack interface.  These changes will make
multiple emulators usable in open source Xen via the new libxl interface.

libxl changes
-------------

- The singleton device_model_version, device_model_stubdomain and
  device_model fields in the b_info structure will be replaced by a list of
  (version, stubdomain, model, arguments) tuples, one for each emulator.

- libxl_domain_create_new() will be changed to spawn a new device model
  for each entry in the list.

It may also be useful to spawn the device models separately and only
attach them during domain creation.    This could be supported by
making each device_model entry a union of `pid | parameter_tuple`.
If such an entry specifies a parameter tuple, it is processed as above;
if it specifies a pid, libxl_domain_create_new(), the existing device
model with that pid is attached instead.

QEMU changes
------------

- Patches to make QEMU register with Xen as an ioreq-server have been
  submitted upstream, but not yet applied.

- QEMU's `--machine none` and `--nodefaults` options should make it
  possible to create an empty machine and add just a host bus, PCI bus
  and device.   This has not yet been fully demonstrated, so QEMU changes
  may be required.

Xen changes
-----------

- Until now, `ioreq-server` has only been used to connect one extra
  device model, in addition to the default one.  Multiple emulators
  should work, but there is a chance that bugs will be discovered.

Interfacing with xenopsd
------------------------

This functionality will only be available through the experimental
Xenlight-based xenopsd.

 - the `VM_build` clause in the `atomics_of_operation` function will be
   changed to fill in the list of emulators to be created (or attached)
   in the b_info struct

Host Configuration
------------------

vGPU support is implemented mostly in xenopsd, so no Xapi changes are 
required to support vGPU through the generic device model mechanism.
Changes would be required if we decided to expose the additional device
models through the API, but in the near future it is more likely that
any additional device models will be dealt with entirely by xenopsd.
