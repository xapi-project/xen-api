---
title: VGPU type identifiers
layout: default
design_doc: true
revision: 1
status: released (7.0)
design_review: 156
revision_history:
- revision_number: 1
  description: Initial version
---

Introduction
------------

When xapi starts, it may create a number of VGPU_type objects. These act as
VGPU presets, and exactly which VGPU_type objects are created depends on the
installed hardware and in certain cases the presence of certain files in dom0.

When deciding which VGPU_type objects need to be created, xapi needs to
determine whether a suitable VGPU_type object already exists, as there should
never be duplicates. At the moment the combination of vendor name and model name
is used as a primary key, but this is not ideal as these values are subject to
change. We therefore need a way of creating a primary key to uniquely identify
VGPU_type objects.

Identifier
----------

We will add a new read-only field to the database:

- `VGPU_type.identifier (string)`

This field will contain a string representation of the parameters required to
uniquely identify a VGPU_type. The parameters required can be summed up with the
following OCaml type:

```
type nvidia_id = {
  pdev_id : int;
  psubdev_id : int option;
  vdev_id : int;
  vsubdev_id : int;
}

type gvt_g_id = {
  pdev_id : int;
  low_gm_sz : int64;
  high_gm_sz : int64;
  fence_sz : int64;
  monitor_config_file : string option;
}

type t =
  | Passthrough
  | Nvidia of nvidia_id
  | GVT_g of gvt_g_id
```

When converting this type to a string, the string will always be prefixed with
`0001:` enabling future versioning of the serialisation format.

For passthrough, the string will simply be:

`0001:passthrough`

For NVIDIA, the string will be `nvidia` followed by the four device IDs
serialised as four-digit hex values, separated by commas. If `psubdev_id` is
`None`, the empty string will be used e.g.

```
Nvidia {
  pdev_id = 0x11bf;
  psubdev_id = None;
  vdev_id = 0x11b0;
  vsubdev_id = 0x109d;
}
```

would map to

`0001:nvidia,11bf,,11b0,109d`

For GVT-g, the string will be `gvt-g` followed by the physical device ID encoded
as four-digit hex, followed by `low_gm_sz`, `high_gm_sz` and `fence_sz` encoded
as hex, followed by `monitor_config_file` (or the empty string if it is `None`)
e.g.

```
GVT_g {
  pdev_id = 0x162a;
  low_gm_sz = 128L;
  high_gm_sz = 384L;
  fence_sz = 4L;
  monitor_config_file = None;
}
```

would map to

`0001:gvt-g,162a,80,180,4,,`

Having this string in the database will allow us to do a simple lookup to test
whether a certain VGPU_type already exists. Although it is not currently
required, this string can also be converted back to the type from which it was
generated.

When deciding whether to create VGPU_type objects, xapi will generate the
identifier string and use it to look for existing VGPU_type objects in the
database. If none are found, xapi will look for existing VGPU_type objects with
the tuple of model name and vendor name. If still none are found, xapi will
create a new VGPU_type object.
