---
title: Add supported image formats in sm-list
layout: default
design_doc: true
revision: 1
status: proposed
---

# Introduction

At XCP-ng, we are enhancing support for QCOW2 images in SMAPIv1. The main
motivation behind this change is to overcome the 2TB size limitation imposed
by the VHD format. By adding support for QCOW2, a Storage Repository (SR) will
be able to host disks in VHD and/or QCOW2 formats, depending on the SR type.
This flexibility allows end users to choose storage solutions that best meet
their capacity requirements.

# Design Proposal

To expose the available image formats to clients (e.g., the `xe` CLI), we propose
adding a new field called `supported-image-formats` to the Storage Manager (SM)
module. This field will be included in the output of the `SM.get_all_records` call.

The `supported-image-formats` field is populated by retrieving the information
from the SMAPIv1 driver. Specifically, the driver will update its `DRIVER_INFO`
dictionary with a new key, `supported_image_formats`, which will contain a list
of strings representing the supported image formats
(for example: `["vhd", "raw", "qcow2"]`).

For example, after this change, running:

```bash
# xe sm-list params=all
```

will output something like:

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

This change impacts the SM data model, and as such, the XAPI database version will
be incremented.

# Impact

- **Data Model:** A new field (`supported-image-formats`) is added to the SM records.
- **Client Awareness:** Clients like the `xe` CLI will now be able to query and display the supported image formats for a given SR.
- **Database Versioning:** The XAPI database version will be updated to reflect this change.

