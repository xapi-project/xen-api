---
title: GPU support evolution
layout: default
design_doc: true
revision: 3
status: released (7.0)
revision_history:
- revision_number: 1
  description: Documented interface changes between xapi and xenopsd for vGPU
- revision_number: 2
  description: Added design for storing vGPU-to-pGPU allocation in xapi database
- revision_number: 3
  description: Marked new xapi DB fields as internal-only
---

Introduction
------------

As of XenServer 6.5, VMs can be provisioned with access to graphics processors
(either emulated or passed through) in four different ways. Virtualisation of
Intel graphics processors will exist as a fifth kind of graphics processing
available to VMs. These five situations all require the VM's device model to be
created in subtly different ways:

__Pure software emulation__

- qemu is launched either with no special parameter, if the basic Cirrus
  graphics processor is required, otherwise qemu is launched with the
  `-std-vga` flag.

__Generic GPU passthrough__

- qemu is launched with the `-priv` flag to turn on privilege separation
- qemu can additionally be passed the `-std-vga` flag to choose the
  corresponding emulated graphics card.

__Intel integrated GPU passthrough (GVT-d)__

- As well as the `-priv` flag, qemu must be launched with the `-std-vga` and
  `-gfx_passthru` flags. The actual PCI passthrough is handled separately
  via xen.

__NVIDIA vGPU__

- qemu is launched with the `-vgpu` flag
- a secondary display emulator, demu, is launched with the following parameters:
  - `--domain` - the VM's domain ID
  - `--vcpus` - the number of vcpus available to the VM
  - `--gpu` - the PCI address of the physical GPU on which the emulated GPU will
    run
  - `--config` - the path to the config file which contains detail of the GPU to
      emulate

__Intel vGPU (GVT-g)__

- here demu is not used, but instead qemu is launched with five parameters:
  - `-xengt`
  - `-vgt_low_gm_sz` - the low GM size in MiB
  - `-vgt_high_gm_sz` - the high GM size in MiB
  - `-vgt_fence_sz` - the number of fence registers
  - `-priv`

xenopsd
-------

To handle all these possibilities, we will add some new types to xenopsd's
interface:

```
module Pci = struct
  type address = {
    domain: int;
    bus: int;
    device: int;
    fn: int;
  }

  ...
end

module Vgpu = struct
  type gvt_g = {
    physical_pci_address: Pci.address;
    low_gm_sz: int64;
    high_gm_sz: int64;
    fence_sz: int;
  }

  type nvidia = {
    physical_pci_address: Pci.address;
    config_file: string
  }

  type implementation =
    | GVT_g of gvt_g
    | Nvidia of nvidia

  type id = string * string

  type t = {
    id: id;
    position: int;
    implementation: implementation;
  }

  type state = {
    plugged: bool;
    emulator_pid: int option;
  }
end

module Vm = struct
  type igd_passthrough of
    | GVT_d

  type video_card =
    | Cirrus
    | Standard_VGA
    | Vgpu
    | Igd_passthrough of igd_passthrough

  ...
end

module Metadata = struct
  type t = {
    vm: Vm.t;
    vbds: Vbd.t list;
    vifs: Vif.t list;
    pcis: Pci.t list;
    vgpus: Vgpu.t list;
    domains: string option;
  }
end
```

The `video_card` type is used to indicate to the function
`Xenops_server_xen.VM.create_device_model_config` how the VM's emulated graphics
card will be implemented. A value of `Vgpu` indicates that the VM needs to be
started with one or more virtualised GPUs - the function will need to look at
the list of GPUs associated with the VM to work out exactly what parameters to
send to qemu.

If `Vgpu.state.emulator_pid` of a plugged vGPU is `None`, this indicates that
the emulation of the vGPU is being done by qemu rather than by a separate
emulator.

n.b. adding the `vgpus` field to `Metadata.t` will break backwards compatibility
with old versions of xenopsd, so some upgrade logic will be required.

This interface will allow us to support multiple vGPUs per VM in future if
necessary, although this may also require reworking the interface between
xenopsd, qemu and demu. For now, xenopsd will throw an exception if it is asked
to start a VM with more than one vGPU.

xapi
----

To support the above interface, xapi will convert all of a VM's non-passthrough
GPUs into `Vgpu.t` objects when sending VM metadata to xenopsd.

In contrast to GVT-d, which can only be run on an Intel GPU which has been
has been hidden from dom0, GVT-g will only be allowed to run on a GPU which has
_not_ been hidden from dom0.

If a GVT-g-capable GPU is detected, and it is not hidden from dom0, xapi will
create a set of VGPU_type objects to represent the vGPU presets which can run on
the physical GPU. Exactly how these presets are defined is TBD, but a likely
solution is via a set of config files as with NVIDIA vGPU.

__Allocation of vGPUs to physical GPUs__

For NVIDIA vGPU, when starting a VM, each vGPU attached to the VM is assigned
to a physical GPU as a result of capacity planning at the pool level. The
resulting configuration is stored in the VM.platform dictionary, under
specific keys:

- `vgpu_pci_id` - the address of the physical GPU on which the vGPU will run
- `vgpu_config` - the path to the vGPU config file which the emulator will use

Instead of storing the assignment in these fields, we will add a new
internal-only database field:

- `VGPU.scheduled_to_be_resident_on (API.ref_PGPU)`

This will be set to the ref of the physical GPU on which the vGPU will run. From
here, xapi can easily obtain the GPU's PCI address. Capacity planning will also
take into account which vGPUs are scheduled to be resident on a physical GPU,
which will avoid races resulting from many vGPU-enabled VMs being started at
once.

The path to the config file is already stored in the `VGPU_type.internal_config`
dictionary, under the key `vgpu_config`. xapi will use this value directly
rather than copying it to VM.platform.

To support other vGPU implementations, we will add another internal-only
database field:

- `VGPU_type.implementation enum(Passthrough|Nvidia|GVT_g)`

For the `GVT_g` implementation, no config file is needed. Instead,
`VGPU_type.internal_config` will contain three key-value pairs, with the keys

- `vgt_low_gm_sz`
- `vgt_high_gm_sz`
- `vgt_fence_sz`

The values of these pairs will be used to construct a value of type
`Xenops_interface.Vgpu.gvt_g`, which will be passed down to xenopsd.
