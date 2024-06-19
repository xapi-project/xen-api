---
title: XenPrep
layout: default
design_doc: true
revision: 2
status: proposed
---

### Background
Windows guests should have XenServer-specific drivers installed. As of mid-2015 these have been always been installed and upgraded by an essentially manual process involving an ISO carrying the drivers. We have a plan to enable automation through the standard Windows Update mechanism. This will involve a new additional virtual PCI device being provided to the VM, to trigger Windows Update to fetch drivers for the device.

There are many existing Windows guests that have drivers installed already. These drivers must be uninstalled before the new drivers are installed (and ideally before the new PCI device is added). To make this easier, we are planning a XenAPI call that will cause the removal of the old drivers and the addition of the new PCI device.

Since this is only to help with updating old guests, the call may well be removed at some point in the future.

### Brief high-level design
The XenAPI call will be called `VM.xenprep_start`. It will update the VM record to note that the process has started, and will insert a special ISO into the VM's virtual CD drive.

That ISO will contain a tool which will be set up to auto-run (if auto-run is enabled in the guest). The tool will:

1. Lock the CD drive so other Windows programs cannot eject the disc.
2. Uninstall the old drivers.
3. Eject the CD to signal success.
4. Shut down the VM.

XenServer will interpret the ejection of the CD as a success signal, and when the VM shuts down without the special ISO in the drive, XenServer will:

1. Update the VM record:
  * Remove the mark that shows that the xenprep process is in progress
  * Give it the new PCI device: set `VM.auto_update_drivers` to `true`.
  * If `VM.virtual_hardware_platform_version` is less than 2, then set it to 2.
2. Start the VM.

### More details of the xapi-project parts
(The tool that runs in the guest is out of scope for this document.)

#### Start
The XenAPI call `VM.xenprep_start` will throw a power-state error if the VM is not running.
For RBAC roles, it will be available to "VM Operator" and above.

It will:

1. Insert the xenprep ISO into the VM's virtual CD drive.
2. Write `VM.other_config` key `xenprep_progress=ISO_inserted` to record the fact that the xenprep process has been initiated.

If `xenprep_start` is called on a VM already undergoing xenprep, the call will return successfully but will not do anything.

If the VM does not have an empty virtual CD drive, the call will fail with a suitable error.

#### Cancellation
While xenprep is in progress, any request to eject the xenprep ISO (except from inside the guest) will be rejected with a new error "VBD_XENPREP_CD_IN_USE".

There will be a new XenAPI call `VM.xenprep_abort` which will:

1. Remove the `xenprep_progress` entry from `VM.other_config`.
2. Make a best-effort attempt to eject the CD. (The guest might prevent ejection.)

This is not intended for cancellation while the xenprep tool is running, but rather for use before it starts, for example if auto-run is disabled or if the VM has a non-Windows OS.

#### Completion

Aim: when the guest shuts down after ejecting the CD, XenServer will start the guest again with the new PCI device.

Xapi works through the queue of events it receives from xenopsd. It is possible that by the time xapi processes the cd-eject event, the guest might have shut down already.

When the shutdown (not reboot) event is handled, we shall check whether we need to do anything xenprep-related. If
* The VM `other_config` map has `xenprep_progress` as either of `ISO_inserted` or `shutdown`, and
* The xenprep ISO is no longer in the drive

then we must (in the specified order)

1. Update the VM record:
  1. In `VM.other_config` set `xenprep_progress=shutdown`
  2. If `VM.virtual_hardware_platform_version` is less than 2, then set it to 2.
  3. Give it the new PCI device: set `VM.auto_update_drivers` to `true`.
2. Initiate VM start.
3. Remove `xenprep_progress` from `VM.other_config`

The most relevant code is probably the `update_vm` function in `ocaml/xapi/xapi_xenops.ml` in the `xen-api` repo (or in some function called from there).
