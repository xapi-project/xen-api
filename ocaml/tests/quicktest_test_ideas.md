
This document lists unit/component test ideas that need some additional
infrastructure to be created before they can be created.

Ref: CA-83837: when a VBD frontend rejects an unplug we must avoid trying
to deactivate the VDI because it will block, but we must do it later when
the unplug happens.

VBD.plug to helper VM
  signal helper VM to open device
VBD.unplug from helper VM
-- verify that operation fails immediately with an error (no blocking)
  signal helper VM to close device
-- verify that VBD is unplugged and VDI is deactivated and detached

VM.suspend helper VM
-- verify that disks are deactivated and detached

VBD.plug to helper HVM VM using driver domain
open qemu frontend in dom0
VBD.unplug in a background thread should block
in another thread sleep 0.2s and close the frontend
-- verify the VBD.unplug returns
