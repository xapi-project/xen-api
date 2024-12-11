+++
title = "Multi-version drivers"
+++

Linux loads device drivers on boot and every device driver exists in one
version. XAPI extends this scheme such that device drivers may exist in
multiple variants plus a mechanism to select the variant being loaded on
boot. Such a driver is called a multi-version driver and we expect only
a small subset of drivers, built and distributed by XenServer, to have
this property. The following covers the background, API, and CLI for
multi-version drivers in XAPI.

## Variant vs. Version

A driver comes in several variants, each of which has a version. A
variant may be updated to a later version while retaining its identity.
This makes variants and versions somewhat synonymous and is admittedly
confusing.

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

To support multi-version drivers, XenServer introduces a new
hierarchy in Dom0. This is mostly technical background because a
lower-level tool deals with this and not XAPI directly.

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

* A driver is specific to a host.
* A driver has a unique name; however, for API purposes a driver is
  identified by a UUID (on the CLI) and reference (programmatically).
* A driver has multiple variants; each variant has a version.
  Programatically, variants are represented as objects (referenced by
  UUID and a reference) but this is mostly hidden in the CLI for
  convenience.
* A driver variant is active if it is currently used by the kernel
  (loaded).
* A driver variant is selected if it will be considered by the kernel
  (on next boot or when loading on demand).
* Only one variant can be active, and only one variants can be selected.

Inspection and selection of drivers is facilitated by a tool
("drivertool") that is called by xapi. Hence, XAPI does not by itself
manipulate the file system that implements driver selection.

An example interaction with the API through xe:

```
[root@lcy2-dt110 log]# xe hostdriver-list uuid=c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0 params=all
uuid ( RO)                   : c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0
                   name ( RO): cisco-fnic
                   type ( RO): network
            description ( RO): cisco-fnic
                   info ( RO): cisco-fnic
              host-uuid ( RO): 6de288e7-0f82-4563-b071-bcdc083b0ffd
         active-variant ( RO): <none>
       selected-variant ( RO): <none>
               variants ( RO): generic/1.2
    variants-dev-status ( RO): generic=beta
          variants-uuid ( RO): generic=abf5997b-f2ad-c0ef-b27f-3f8a37bf58a6
    variants-hw-present ( RO): 
```

Selection of a variant by name (which is unique per driver); this
variant would become active after reboot.

```
[root@lcy2-dt110 log]# xe hostdriver-select variant-name=generic uuid=c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0
[root@lcy2-dt110 log]# xe hostdriver-list uuid=c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0 params=all
uuid ( RO)                   : c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0
                   name ( RO): cisco-fnic
                   type ( RO): network
            description ( RO): cisco-fnic
                   info ( RO): cisco-fnic
              host-uuid ( RO): 6de288e7-0f82-4563-b071-bcdc083b0ffd
         active-variant ( RO): <none>
       selected-variant ( RO): generic
               variants ( RO): generic/1.2
    variants-dev-status ( RO): generic=beta
          variants-uuid ( RO): generic=abf5997b-f2ad-c0ef-b27f-3f8a37bf58a6
    variants-hw-present ( RO): 
```

The variant can be inspected, too, using it's UUID.

```
[root@lcy2-dt110 log]# xe hostdriver-variant-list uuid=abf5997b-f2ad-c0ef-b27f-3f8a37bf58a6
uuid ( RO)           : abf5997b-f2ad-c0ef-b27f-3f8a37bf58a6
           name ( RO): generic
        version ( RO): 1.2
         status ( RO): beta
         active ( RO): false
       selected ( RO): true
    driver-uuid ( RO): c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0
    driver-name ( RO): cisco-fnic
      host-uuid ( RO): 6de288e7-0f82-4563-b071-bcdc083b0ffd
     hw-present ( RO): false
```

## Class Host_driver

Class `Host_driver` represents an instance of a multi-version driver on
a host. It references `Driver_variant` objects for the details of the
available and active variants. A variant has a version.

### Fields

All fields are read-only and can't be set directly. Be aware that names
in the CLI and the API may differ.

* `host`: reference to the host where the driver is installed.
* `name`: string; name of the driver without ".ko" extension.
* `variants`: string set; set of variants available on the host for this
  driver.  The name of each variant of a driver is unique and used in
  the CLI for selecting it.
* `selected_varinat`: variant, possibly empty. Variant that is selected,
  i.e. the variant of the driver that will be considered by the kernel
  when loading the driver the next time. May be null when none is
  selected.
* `active_variant`: variant, possibly empty. Variant that is currently
  loaded by the kernel.
* `type`, `info`, `description`: strings providing background
  information.

The CLI uses `hostdriver` and a dash instead of an underscore. The CLI
also offers convenience fields. Whenever selected and
active variant are not the same, a reboot is required to activate the
selected driver/variant combination.

(We are not using `host-driver` in the CLI to avoid the impression that
this is part of a host object.)

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
   Called on toolstack restart and may be invoked from the CLI for
   development.

## Class `Driver_variant`

An object of this class represents a variant of a driver on a host,
i.e., it is specific to both.

* `name`: unique name
* `driver`: what host driver this belongs to
* `version`: string; a driver variant has a version
* `status`: string: development status, like "beta"
* `hardware_present`: boolean, true if the host has the hardware
   installed supported by this driver

The only method available is `select(self)` to select a variant. It has
the same effect as the `select` method on the `Host_driver` class.

The CLI comes with corresponding `xe hostdriver-variant-*` commands to
list and select a variant. 

```
[root@lcy2-dt110 log]# xe hostdriver-variant-list uuid=abf5997b-f2ad-c0ef-b27f-3f8a37bf58a6
uuid ( RO)           : abf5997b-f2ad-c0ef-b27f-3f8a37bf58a6
           name ( RO): generic
        version ( RO): 1.2
         status ( RO): beta
         active ( RO): false
       selected ( RO): true
    driver-uuid ( RO): c0fe459d-5f8a-3fb1-3fe5-3c602fafecc0
    driver-name ( RO): cisco-fnic
      host-uuid ( RO): 6de288e7-0f82-4563-b071-bcdc083b0ffd
     hw-present ( RO): false
```

### Database

Each `Host_driver` and `Driver_variant` object is represented in the
database and data is persisted over reboots. This means this data will
be part of data collected in a `xen-bugtool` invocation.

### Scan and Rescan

On XAPI start-up, XAPI updates the `Host_driver` objects belonging to the
host to reflect the actual situation. This can be initiated from the
CLI, too, mostly for development.


