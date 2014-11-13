GPU pass-through support
========================

This document contains the software design for GPU pass-through. This
code was originally included in the version of Xapi used in XenServer 6.0.

Overview
--------

Rather than modelling GPU pass-through from a PCI perspective, and
having the user manipulate PCI devices directly, we are taking a
higher-level view by introducing a dedicated graphics model. The
graphics model is similar to the networking and storage model, in which
virtual and physical devices are linked through an intermediate
abstraction layer (e.g. the "Network" class in the networking model).

The basic graphics model is as follows:

-   A host owns a number of physical GPU devices (*pGPUs*), each of
    which is available for passing through to a VM.
-   A VM may have a virtual GPU device (*vGPU*), which means it expects
    to have access to a GPU when it is running.
-   Identical pGPUs are grouped across a resource pool in *GPU groups*.
    GPU groups are automatically created and maintained by XS.
-   A GPU group connects vGPUs to pGPUs in the same way as VIFs are
    connected to PIFs by Network objects: for a VM *v* having a vGPU on
    GPU group *p* to run on host *h*, host *h* must have a pGPU in GPU
    group *p* and pass it through to VM *v*.
-   VM start and non-live migration rules are analogous to the network
    API and follow the above rules.
-   In case a VM that has a vGPU is started, while no pGPU available, an
    exception will occur and the VM won't start. As a result, in order
    to guarantee that a VM always has access to a pGPU, the number of
    vGPUs should not exceed the number of pGPUs in a GPU group.

Currently, the following restrictions apply:

-   Hotplug is not supported.
-   Suspend/resume and checkpointing (memory snapshots) are not
    supported.
-   Live migration (XenMotion) is not supported.
-   No more than one GPU per VM will be supported.
-   Only Windows guests will be supported.

XenAPI Changes
--------------

The design introduces a new generic class called *PCI* to capture state
and information about relevant PCI devices in a host. By default, xapi
would not create PCI objects for all PCI devices, but only for the ones
that are managed and configured by xapi; currently only GPU devices.

The PCI class has no fields specific to the type of the PCI device (e.g.
a graphics card or NIC). Instead, device specific objects will contain a
link to their underlying PCI device's object.

The new XenAPI classes and changes to existing classes are detailed
below.

### PCI class

Fields:

|  Name          |  Type                    |  Description                                                                                                                                                   |
|----------------|--------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|
|  uuid          |  string                  |  Unique identifier/object reference.                                                                                                                           |
|  class_id      |  string                  |  PCI class ID (hidden field)                                                                                                                                   |
|  class_name    |  string                  |  PCI class name (GPU, NIC, ...)                                                                                                                                |
|  vendor_id     |  string                  |  Vendor ID (hidden field).                                                                                                                                     |
|  vendor_name   |  string                  |  Vendor name.                                                                                                                                                  |
|  device_id     |  string                  |  Device ID (hidden field).                                                                                                                                     |
|  device_name   |  string                  |  Device name.                                                                                                                                                  |
|  host          |  host ref                |  The host that owns the PCI device.                                                                                                                            |
|  pci_id        |  string                  |  BDF (domain/Bus/Device/Function identifier) of the (physical) PCI function, e.g. "0000:00:1a.1". The format is hhhh:hh:hh.h, where h is a hexadecimal digit.  |
|  functions     |  int                     |  Number of (physical + virtual) functions; currently fixed at 1 (hidden field).                                                                                |
|  attached_VMs  |  VM ref set              |  List of VMs that have this PCI device "currently attached", i.e. plugged, i.e. passed-through to (hidden field).                                              |
|  dependencies  |  PCI ref set             |  List of dependent PCI devices: all of these need to be passed-thru to the same VM (co-location).                                                              |
|  other_config  |  (string -> string) map  |  Additional optional configuration (as usual).                                                                                                                 |

*Hidden fields* are only for use by xapi internally, and not visible to
XenAPI users.

Messages: none.

### PGPU class

A physical GPU device (pGPU).

Fields:

|  Name          |  Type                    |  Description                                       |
|----------------|--------------------------|----------------------------------------------------|
|  uuid          |  string                  |  Unique identifier/object reference.               |
|  PCI           |  PCI ref                 |  Link to the underlying PCI device.                |
|  other_config  |  (string -> string) map  |  Additional optional configuration (as usual).     |
|  host          |  host ref                |  The host that owns the GPU.                       |
|  GPU_group     |  GPU_group ref           |  GPU group the pGPU is contained in. Can be Null.  |

Messages: none.

### GPU\_group class

A group of identical GPUs across hosts. A VM that is associated with a
GPU group can use any of the GPUs in the group. A VM does not need to
install new GPU drivers if moving from one GPU to another one in the
same GPU group.

Fields:

|  Name              |  Type                    |  Description                                                                     |
|--------------------|--------------------------|----------------------------------------------------------------------------------|
|  VGPUs             |  VGPU ref set            |  List of vGPUs in the group.                                                     |
|  uuid              |  string                  |  Unique identifier/object reference.                                             |
|  PGPUs             |  PGPU ref set            |  List of pGPUs in the group.                                                     |
|  other_config      |  (string -> string) map  |  Additional optional configuration (as usual).                                   |
|  name_label        |  string                  |  A human-readable name.                                                          |
|  name_description  |  string                  |  A notes field containing human-readable description.                            |
|  GPU_types         |  string set              |  List of GPU types (vendor+device ID) that can be in this group (hidden field).  |

Messages: none.

### VGPU class

A virtual GPU device (vGPU).

Fields:

|  Name                |  Type                    |  Description                                                                         |
|----------------------|--------------------------|--------------------------------------------------------------------------------------|
|  uuid                |  string                  |  Unique identifier/object reference.                                                 |
|  VM                  |  VM ref                  |  VM that owns the vGPU.                                                              |
|  GPU_group           |  GPU_group ref           |  GPU group the vGPU is contained in.                                                 |
|  currently_attached  |  bool                    |  Reflects whether the virtual device is currently "connected" to a physical device.  |
|  device              |  string                  |  Order in which the devices are plugged into the VM. Restricted to "0" for now.      |
|  other_config        |  (string -> string) map  |  Additional optional configuration (as usual).

Messages:

|  Prototype                                        |  Description                                                                                           |   |
|---------------------------------------------------|--------------------------------------------------------------------------------------------------------|---|
|  VGPU ref create (GPU_group ref, string, VM ref)  |  Manually assign the vGPU device to the VM given a device number, and link it to the given GPU group.  |   |
|  void destroy (VGPU ref)                          |  Remove the association between the GPU group and the VM.                                              |   |

It is possible to assign more vGPUs to a group than number number of
pGPUs in the group. When a VM is started, a pGPU must be available; if
not, the VM will not start. Therefore, to guarantee that a VM has access
to a pGPU at any time, one must manually enforce that the number of
vGPUs in a GPU group does not exceed the number of pGPUs. XenCenter
might display a warning, or simply refuse to assign a vGPU, if this
constraint it violated. This is analogous to the handling of memory
availability in a pool: a VM may not be able to start if there is no
host having enough free memory.

### VM class

Fields:

-   Deprecate (unused) `PCI_bus` field
-   Add field `VGPU ref set VGPUs`: List of vGPUs.
-   Add field `PCI ref set attached_PCIs`: List of PCI devices that are
    "currently attached" (plugged, passed-through) (*hidden field*).

### host class

Fields:

-   Add field `PCI ref set PCIs`: List of PCI devices.
-   Add field `PGPU ref set PGPUs`: List of physical GPU devices.
-   Add field `(string -> string) map chipset_info`, which contains at
    least the key `iommu`. If `"true"`, this key indicates whether the
    host has IOMMU/VT-d support build in, **and** this functionality is
    enabled by Xen; the value will be `"false"` otherwise.

Initialisation and Operations
-----------------------------

### Enabling IOMMU/VT-d

(This may not be needed in Xen 4.1. Confirm with Simon.)

Provide a command that does this:

-   `/opt/xensource/libexec/xen-cmdline --set-xen iommu=1`
-   reboot

### Xapi startup

Definitions:

-   PCI devices are matched on the combination of their `pci_id`,
    `vendor_id`, and `device_id`.

First boot and any subsequent xapi start:

1.  Find out from dmesg whether IOMMU support is present and enabled in
    Xen, and set `host.chipset_info:iommu` accordingly.
2.  Detect GPU devices currently present in the host. For each:
    1.  If there is no matching PGPU object yet, create a PGPU object,
        and add it to a GPU group containing identical PGPUs, or a new
        group.
    2.  If there is no matching PCI object yet, create one, and also
        create or update the PCI objects for dependent devices.

3.  Destroy all existing PCI objects of devices that are not currently
    present in the host (i.e. objects for devices that have been
    replaced or removed).
4.  Destroy all existing PGPU objects of GPUs that are not currently
    present in the host. Send a XenAPI alert to notify the user of this
    fact.
5.  Update the list of `dependencies` on all PCI objects.
6.  Sync `VGPU.currently_attached` on all `VGPU` objects.

### Upgrade

For any VMs that have `VM.other_config:pci` set to use a GPU, create an
appropriate vGPU, and remove the `other_config` option.

### Generic PCI Interface

A generic PCI interface exposed to higher-level code, such as the
networking and GPU management modules within Xapi. This functionality
relies on Xenops.

The PCI module exposes the following functions:

-   Check whether a PCI device has free (unassigned) functions. This is
    the case if the number of assignments in `PCI.attached_VMs` is
    smaller than `PCI.functions`.
-   Plug a PCI function into a running VM.
    1.  Raise exception if there are no free functions.
    2.  Plug PCI device, as well as dependent PCI devices. The PCI
        module must also tell device-specific modules to update the
        `currently_attached` field on dependent `VGPU` objects etc.
    3.  Update `PCI.attached_VMs`.
-   Unplug a PCI function from a running VM.
    1.  Raise exception if the PCI function is not owned by (passed
        through to) the VM.
    2.  Unplug PCI device, as well as dependent PCI devices. The PCI
        module must also tell device-specific modules to update the
        `currently_attached` field on dependent `VGPU` objects etc.
    3.  Update `PCI.attached_VMs`.

### Construction and Destruction

VGPU.create:

1.  Check license. Raise FEATURE\_RESTRICTED if the GPU feature has not
    been enabled.
2.  Raise INVALID\_DEVICE if the given device number is not "0", or
    DEVICE\_ALREADY\_EXISTS if (indeed) the device already exists. This
    is a convenient way of enforcing that only one vGPU per VM is
    supported, for now.
3.  Create `VGPU` object in the DB.
4.  Initialise `VGPU.currently_attached = false`.
5.  Return a ref to the new object.

VGPU.destroy:

1.  Raise OPERATION\_NOT\_ALLOWED if `VGPU.currently_attached = true`
    and the VM is running.
2.  Destroy `VGPU` object.

### VM Operations

VM.start(\_on):

1.  If `host.chipset_info:iommu = "false"`, raise VM\_REQUIRES\_IOMMU.
2.  Raise FEATURE\_REQUIRES\_HVM (carrying the string "GPU passthrough
    needs HVM") if the VM is PV rather than HVM.
3.  For each of the VM's vGPUs:
    1.  Confirm that the given host has a pGPU in its associated GPU
        group. If not, raise VM\_REQUIRES\_GPU.
    2.  Consult the generic PCI module for all pGPUs in the group to
        find out whether a suitable PCI function is available. If a
        physical device is not available, raise VM\_REQUIRES\_GPU.
    3.  Ask PCI module to plug an available pGPU into the VM's domain
        and set `VGPU.currently_attached` to `true`. As a side-effect,
        any dependent PCI devices would be plugged.

VM.shutdown:

1.  Ask PCI module to unplug all GPU devices.
2.  Set `VGPU.currently_attached` to `false` for all the VM's VGPUs.

VM.suspend, VM.resume(\_on):

-   Raise VM\_HAS\_PCI\_ATTACHED if the VM has any plugged `VGPU`
    objects, as suspend/resume for VMs with GPUs is currently not
    supported.

VM.pool\_migrate:

-   Raise VM\_HAS\_PCI\_ATTACHED if the VM has any plugged `VGPU`
    objects, as live migration for VMs with GPUs is currently not
    supported.

VM.clone, VM.copy, VM.snapshot:

-   Copy `VGPU` objects along with the VM.

VM.import, VM.export:

-   Include `VGPU` and `GPU_group` objects in the VM export format.

VM.checkpoint

-   Raise VM\_HAS\_PCI\_ATTACHED if the VM has any plugged `VGPU`
    objects, as checkpointing for VMs with GPUs is currently not
    supported.

### Pool Join and Eject

Pool join:

1.  For each `PGPU`:
    1.  Copy it to the pool.
    2.  Add it to a `GPU_group` of identical PGPUs, or a new one.

2.  Copy each `VGPU` to the pool together with the VM that owns it, and
    add it to the GPU group containing the same `PGPU` as before the
    join.

Step 1 is done automatically by the xapi startup code, and step 2 is
handled by the VM export/import code. Hence, no work needed.

Pool eject:

1.  `VGPU` objects will be automatically GC'ed when the VMs are removed.
2.  Xapi's startup code recreates the `PGPU` and `GPU_group` objects.

Hence, no work needed.

Required Low-level Interface
----------------------------

Xapi needs a way to obtain a list of all PCI devices present on a host.
For each device, xapi needs to know:

-   The PCI ID (BDF).
-   The type of device (NIC, GPU, ...) according to a well-defined and
    stable list of device types (as in `/usr/share/hwdata/pci.ids`).
-   The device and vendor ID+name (currently, for PIFs, xapi looks up
    the name in `/usr/share/hwdata/pci.ids`).
-   Which other devices/functions are required to be passed through to
    the same VM (co-located), e.g. other functions of a compound PCI
    device.

Command-Line Interface (xe)
-----------------------------

- xe pgpu-list
- xe pgpu-param-list/get/set/add/remove/clear
- xe gpu-group-list
- xe gpu-group-param-list/get/set/add/remove/clear
- xe vgpu-list
- xe vgpu-create
- xe vgpu-destroy
- xe vgpu-param-list/get/set/add/remove/clear
- xe host-param-get param-name=chipset-info param-key=iommu

