---
title: Specifying Emulated PCI Devices
layout: default
design_doc: true
revision: 1
status: proposed
---

### Background and goals

At present (early March 2015) the datamodel defines a VM as having a "platform" string-string map, in which two keys are interpreted as specifying a PCI device which should be emulated for the VM. Those keys are "device_id" and "revision" (with int values represented as decimal strings).

Limitations:
* Hardcoded defaults are used for the the vendor ID and all other parameters except device_id and revision.
* Only one emulated PCI device can be specified.

When instructing qemu to emulate PCI devices, qemu accepts twelve parameters for each device.

Future guest-agent features rely on additional emulated PCI devices.  We cannot know in advance the full details of all the devices that will be needed, but we can predict some.

We need a way to configure VMs such that they will be given additional emulated PCI devices.

### Design

In the datamodel, there will be a new type of object for emulated PCI devices.

Tentative name: "emulated_pci_device"

Fields to be passed through to qemu are the following, all static read-only, and all ints except devicename:
* devicename (string)
* vendorid
* deviceid
* command
* status
* revision
* classcode
* headertype
* subvendorid
* subsystemid
* interruptline
* interruptpin

We also need a "built_in" flag: see below.

Allow creation of these objects through the API (and CLI).

(It would be nice, but by no means essential, to be able to create one by specifying an existing one as a basis, along with one or more altered fields, e.g. "Make a new one just like that existing one except with interruptpin=9.")

Create some of these devices to be defined as standard in XenServer, along the same lines as the VM templates. Those ones should have built_in=true.

Allow destruction of these objects through the API (and CLI), but not if they are in use or if they have built_in=true.

A VM will have a list of zero or more of these emulated-pci-device objects. (OPEN QUESTION: Should we forbid having more than one of a given device?)

Provide API (and CLI) commands to add and remove one of these devices from a VM (identifying the VM and device by uuid or other identifier such as name).

The CLI should allow performing this on multiple VMs in one go, based on a selector or filter for the VMs. We have this concept already in the CLI in commands such as vm-start.

In the function that adds an emulated PCI device to a VM, we must check if this is the first device to be added, and must refuse if the VM's Virtual Hardware Platform Version is too low. (Or should we just raise the version automatically if needed?)

When starting a VM, check its list of emulated pci devices and pass the details through to qemu (via xenopsd).
