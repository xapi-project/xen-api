+++
title = "Multi-version drivers"
+++

Linux loads device drivers on boot and every device driver exists in one
version. XAPI extends this scheme such that device drivers may
exist in multiple version plus a mechanism to select the version being
loaded on boot. Such a driver is called a multi-version driver and we
expect only a small subset of drivers, built and distributed by
XenServer, to have this property. The following covers the background,
API, and CLI for multi-version drivers in XAPI.

## Device Drivers in Linux and XAPI

Drivers that are not compiled into the kernel are loaded dynamically
from the file system. They are loaded from the hierarchy

* `/lib/modules/<kernel-version>/`

and we are particularly interested in the hierarchy

* `/lib/modules/<kernel-version>/updates/`

where vendor-supplied ("driver disk") drivers are located and where we
want to support multiple versions. A driver has typically file extension
`.ko` (kernel object).

A presence in the file system does not mean that a driver is loaded as
this happens only on demand. The actually loaded drivers
(or modules, in Linux parlance) can be observed from

* `/proc/modules`

```
netlink_diag 16384 0 - Live 0x0000000000000000
udp_diag 16384 0 - Live 0x0000000000000000
tcp_diag 16384 0 - Live 0x0000000000000000
```

which includes dependencies between modules (the `-` means no dependencies).

## Driver Properties

* A driver name is unique and a driver can be loaded only once. The fact
  that kernel object files are located in a file system hierarchy means
  that a driver may exist multiple times and in different version in the
  file system. From the kernel's perspective a driver has a unique name
  and is loaded at most once. We thus can talk about a driver using its
  name and acknowledge it may exist in different versions in the file
  system.

* A driver that is loaded by the kernel we call *active*.

* A driver file (`name.ko`) that is in a hierarchy searched by the
  kernel is called *selected*. If the kernel needs the driver of that
  name, it would load this object file.

For a driver (`name.ko`) selection and activation are independent
properties:

* *inactive*, *deselected*: not loaded now and won't be loaded on next
  boot.
* *active*, *deselected*: currently loaded but won't be loaded on next
  boot.
* *inactive*, *selected*: not loaded now but will be loaded on demand.
* *active*, *selected*: currently loaded and will be loaded on demand
  after a reboot.

For a driver to be selected it needs to be in the hierarchy searched by
the kernel. By removing a driver from the hierarchy it can be
de-selected. This is possible even for drivers that are already loaded.
Hence, activation and selection are independent.

## Multi-Version Drivers

To support multi-version drivers, XAPI introduces a new
hierarchy in Dom0:

* `/lib/modules/<kernel-version>/updates/` is searched by the kernel for
  drivers.
* The hierarchy is expected to contain symbolic links to the file
  actually containing the driver:
  `/lib/modules/<kernel-version>/xenserver/<driver>/<version>/<name>.ko`

The `xenserver` hierarchy provides drivers in several versions. To
select a particular version, we expect a symbolic link from
`updates/<name>.ko` to `<driver>/<version>/<name>.ko`. At the next boot,
the kernel will search the `updates/` entries and load the linked
driver, which will become active.

Example filesystem hierarchy:
```
/lib/
└── modules
    └── 4.19.0+1 ->
        ├── updates
        │   ├── aacraid.ko
        │   ├── bnx2fc.ko -> ../xenserver/bnx2fc/2.12.13/bnx2fc.ko
        │   ├── bnx2i.ko
        │   ├── cxgb4i.ko
        │   ├── cxgb4.ko
        │   ├── dell_laptop.ko -> ../xenserver/dell_laptop/1.2.3/dell_laptop.ko
        │   ├── e1000e.ko
        │   ├── i40e.ko
        │   ├── ice.ko -> ../xenserver/intel-ice/1.11.17.1/ice.ko
        │   ├── igb.ko
        │   ├── smartpqi.ko
        │   └── tcm_qla2xxx.ko
        └── xenserver
            ├── bnx2fc
            │   ├── 2.12.13
            │   │   └── bnx2fc.ko
            │   └── 2.12.20-dell
            │       └── bnx2fc.ko
            ├── dell_laptop
            │   └── 1.2.3
            │       └── dell_laptop.ko
            └── intel-ice
                ├── 1.11.17.1
                │   └── ice.ko
                └── 1.6.4
                    └── ice.ko

```

Selection of a driver is synonymous with creating a symbolic link to the
desired version.

## Versions

The version of a driver is encoded in the path to its object file but
not in the name itself: for `xenserver/intel-ice/1.11.17.1/ice.ko` the
driver name is `ice` and only its location hints at the version.

The kernel does not reveal the location from where it loaded an active
driver. Hence the name is not sufficient to observe the currently active
version. For this, we use [ELF notes].

The driver file (`name.ko`) is in ELF linker format and may contain
custom [ELF notes]. These are binary annotations that can be compiled
into the file. The kernel reveals these details for loaded drivers
(i.e., modules) in:

* `/sys/module/<name>/notes/`

The directory contains files like

* `/sys/module/xfs/notes/.note.gnu.build-id`

with a specific name (`.note.xenserver`) for our purpose. Such a file contains
in binary encoding a sequence of records, each containing:

* A null-terminated name (string)
* A type (integer)
* A desc (see below)

The format of the description is vendor specific and is used for
a null-terminated string holding the version. The name is fixed to
"XenServer". The exact format is described in [ELF notes].

A note with the name "XenServer" and a particular type then has the version
as a null-terminated string the `desc` field. Additional "XenServer" notes
of a different type may be present.

[ELF notes]: https://www.netbsd.org/docs/kernel/elf-notes.html

## API

XAPI has capabilities to inspect and select multi-version drivers.

The API uses the terminology introduced above:

* A driver is specific to a host
* A driver has a unique name; however, for API purposes a driver is
  identified by a UUID (on the CLI) and reference (programmatically).
* A driver has multiple versions; a version that defines a total order.
  See below for a discussion.
* A driver is active if it is currently used by the kernel (loaded)
* A driver is selected if it will be considered by the kernel (on next
  boot or when loading on demand).
* Only one version can be active, and only one version can be selected
  but these can be the same or different versions.

An example interaction with the API through xe:

```
# xe hostdriver-select uuid=3b3db5f6-3a6d-e668-9fd4-c2a21998dc08 version=3

# xe hostdriver-list uuid=3b3db5f6-3a6d-e668-9fd4-c2a21998dc08
uuid ( RO)                : 3b3db5f6-3a6d-e668-9fd4-c2a21998dc08
                name ( RO): crct10dif_pclmul
           host-uuid ( RO): e51d9f8c-e3d4-42ff-ad9c-c5a66078a096
            versions ( RO): 1; 2; 3
      active-version ( RO): 2
    selected-version ( RO): 3
    requires-reboot  ( RO): true
```

## Class Host_driver

Class `Host_driver` represents an instance of a multi-version driver on a host.

### Fields

All fields are read-only and can't be set directly.

* `host`: reference to the host where the driver is installed.
* `name`: string; name of the driver without ".ko" extension.
* `versions`: string set; set of versions available on the host. These are
  arbitrary strings. This set should have no duplicates.
* `selected_version`: string, possibly empty. Version that is selected,
  i.e. the version of the driver that will be considered by the kernel
  when loading the driver the next time. This string may be empty when
  no version is selected (which is unusual, though).
* `active_version`: string, possibly empty. Version that is currently
  loaded by the kernel.

CLI uses `hostdriver` and a dash instead of an underscore. The
CLI also offers convenience fields. Specifically, whenever selected and
active version are not the same, a reboot is required to activate the
selected driver/version combination. This is synthesized into a
`reboot_required` field.

(We are not using `host-driver` to avoid the impression that this is
 part of a host object.)

### Methods

* All method invocations require `Pool_Operator` rights. "The Pool
  Operator role manages host- and pool-wide resources, including setting
  up storage, creating resource pools and managing patches, high
  availability (HA) and workload balancing (WLB)"

* `select (self, version)`; select `version` of driver `self`. Selecting
  the version (a string) of an existing driver.

* `deselect(self)`: this driver can't be loaded next time the kernel is
  looking for a driver. This is a potentially dangerous operation, so it's
  protected in the CLI with a `--force` flag.

* `rescan (host)`: scan the host and update its driver information.
  Method on `Host`. Called on toolstack restart.

Selecting a version on a host means creating a symbolic link \(aka
symlink\) from `updates/name.ko` to `xenserver/version/name.ko`. Given
that the name of a driver is unique, this means replacing any existing
symlink with the new one.

Any changes to selections need to be followed with:

```
# To regenerate dependencies
depmod -ae -F /boot/System.map-$(uname -r) $(uname -r)
# To generate initrd
dracut -f /boot/initrd-$(uname -r).img $(uname -r)
# If some required drivers weren't loaded on boot, they can be loaded at runtime
udevadm trigger --attr-nomatch=driver
```

Deselecting a driver would mean removing the symlink `updates/name.ko`.
Again, the command sequence above (without `udevadm`, since it won't unload
an already loaded driver) needs to be executed.

### Database

Each `Host_driver` object is represented in the database and data is
persisted over reboots. This means this data will be part of data
collected in a `xen-bugtool` invocation.

### Scan and Rescan

On xapi start-up, xapi updates the `Host_driver` objects belonging to the
host to reflect the actual situation. We do not delete all objects and
re-create them because this would invalidate any reference an API client
would hold. Hence, the implementation uses set arithmetic over driver names
to find:

  1. Drivers removed from the host. These are removed from Xapi.

  2. Drivers added to the host and not yet present in Xapi. These are
     added, including their selecting and active version -- see below.

  3. Updating the driver information in xapi: new versions, changes in active
     selected versions are located and the fields are updated accordingly.

### Version Order

Total order over versions is not yet specified completely. We should break the
version string into segments separated by punctuation characters, and compare
these segments individually.

TODO: weighting
