---
title: Add supported image formats in sm-list
layout: default
design_doc: true
revision: 3
status: proposed
---

# Introduction

At XCP-ng, we are enhancing support for QCOW2 images in SMAPI. The primary
motivation for this change is to overcome the 2TB size limitation imposed
by the VHD format. By adding support for QCOW2, a Storage Repository (SR) will
be able to host disks in VHD and/or QCOW2 formats, depending on the SR type.
In the future, additional formats—such as VHDx—could also be supported.

We need a mechanism to expose to end users which image formats are supported
by a given SR. The proposal is to extend the SM API object with a new field
that clients (such as XenCenter, XenOrchestra, etc.) can use to determine the
available formats.

# Design Proposal

To expose the available image formats to clients (e.g., XenCenter, XenOrchestra, etc.),
we propose adding a new field called `supported_image_formats` to the Storage Manager
(SM) module. This field will be included in the output of the `SM.get_all_records` call.

- With this new information, listing all parameters of the SM object will return:

```bash
# xe sm-list params=all
```

Output of the command will look like (notice that CLI uses hyphens):

```
uuid ( RO)                         : c6ae9a43-fff6-e482-42a9-8c3f8c533e36
name-label ( RO)                   : Local EXT3 VHD
name-description ( RO)             : SR plugin representing disks as VHD files stored on a local EXT3 filesystem, created inside an LVM volume
type ( RO)                         : ext
vendor ( RO)                       : Citrix Systems Inc
copyright ( RO)                    : (C) 2008 Citrix Systems Inc
required-api-version ( RO)         : 1.0
capabilities ( RO) [DEPRECATED]     : SR_PROBE; SR_SUPPORTS_LOCAL_CACHING; SR_UPDATE; THIN_PROVISIONING; VDI_ACTIVATE; VDI_ATTACH; VDI_CLONE; VDI_CONFIG_CBT; VDI_CREATE; VDI_DEACTIVATE; VDI_DELETE; VDI_DETACH; VDI_GENERATE_CONFIG; VDI_MIRROR; VDI_READ_CACHING; VDI_RESET_ON_BOOT; VDI_RESIZE; VDI_SNAPSHOT; VDI_UPDATE
features (MRO)                    : SR_PROBE: 1; SR_SUPPORTS_LOCAL_CACHING: 1; SR_UPDATE: 1; THIN_PROVISIONING: 1; VDI_ACTIVATE: 1; VDI_ATTACH: 1; VDI_CLONE: 1; VDI_CONFIG_CBT: 1; VDI_CREATE: 1; VDI_DEACTIVATE: 1; VDI_DELETE: 1; VDI_DETACH: 1; VDI_GENERATE_CONFIG: 1; VDI_MIRROR: 1; VDI_READ_CACHING: 1; VDI_RESET_ON_BOOT: 2; VDI_RESIZE: 1; VDI_SNAPSHOT: 1; VDI_UPDATE: 1
configuration ( RO)               : device: local device path (required) (e.g. /dev/sda3)
driver-filename ( RO)              : /opt/xensource/sm/EXTSR
required-cluster-stack ( RO)       :
supported-image-formats ( RO)       : vhd, raw, qcow2
```

## Implementation details

The `supported_image_formats` field will be populated by retrieving information
from the SMAPI drivers. Specifically, each driver will update its `DRIVER_INFO`
dictionary with a new key, `supported_image_formats`, which will contain a list
of strings representing the supported image formats
(for example: `["vhd", "raw", "qcow2"]`).

### Driver behavior without `supported_image_formats`

If a driver does not provide this information (as is currently the case with
existing drivers), the default value will be an empty array (`[]`). This signifies
that the driver determines which format to use when creating VDI. During a migration,
the destination driver will choose the format of the VDI if none is explicitly
specified. This ensures backward compatibility with both current and future drivers.

### Specifying image formats for VDIs creation

If the supported image format is exposed to the client, then when creating new VDI,
user can specify the desired format via the `sm_config` parameter `type=qcow2` (or
any format that is supported). If no format is specified, the dirver will use its
preferred default format. If the specified format is not supported, an error will be
generated indicating that the SR does not support it.

```bash
# xe vdi-create \
    sr-uuid=cbe2851e-9f9b-f310-9bca-254c1cf3edd8 \
    name-label="A new VDI" \
    virtual-size=10240 \
    sm-config:type=vhd
```

### Specifying image formats for VDIs migration

When migrating a VDI, an API client may need to specify the desired image format if
the destination SR supports multiple storage formats.

#### VDI pool migrate

To support this, a new parameter, `destination_image_format`, is introduced to
`VDI.pool_migrate`. This field accepts a string specifying the desired format (e.g., *qcow2*),
ensuring that the VDI is migrated in the correct format.

If the specified format is not supported or cannot be used (e.g., due to size limitations),
an error will be generated. Validation will be performed as early as possible to prevent
disruptions during migration. These checks can be performed by examining the XAPI database
to determine whether the SR provided as the destination has a corresponding SM object with
the expected format. If this is not the case, a `format not found` error will be returned.
If no format is specified by the client, the destination driver will determine the appropriate
format.

```bash
# xe vdi-pool-migrate \
    uuid=44b3fe1a-d19a-4132-8513-52f5c3087ecc \
    sr-uuid=656bc0aa-a4ef-e378-0c67-1e2ce004989a \
    destination-image-format=qcow2
```

#### VM migration to remote host

A VDI migration can also occur during an intra-pool VM migration. In this case, we also need
to be able to specify the expected destination format. The following parameters are used
for migration: `vdi:<source-vdi-uuid>=<dest-sr-uuid>`.

We propose to add a new parameter that allows specifying the desired format on the destination
side for a given source VDI. Similar to the source VDI to SR UUID mapping, we propose the
following format: `vdi_format:<source-vdi-uuid>=<destination-image-format>`. The destination
image format will be a string such as `vhd`, `qcow2`, or any other supported format.

# Impact

It should have no impact on existing storage repositories that do not provide any information
about the supported image format.

This change impacts the SM data model, and as such, the XAPI database version will
be incremented.

- **Data Model:** A new field (`supported_image_formats`) is added to the SM records.
- **Client Awareness:** Clients like the `xe` CLI will now be able to query and display the supported image formats for a given SR.
- **Database Versioning:** The XAPI database version will be updated to reflect this change.

