---
title: libxenguest
description: Xen Guest library for building Xen Guest domains
---
## Introduction

`libxenguest` is a library written in C provided for the Xen Hypervisor in Dom0.

For example, it used as the low-level interface building Xen Guest domains.

Its source is located in the folder
[tools/libs/guest](https://github.com/xen-project/xen/tree/master/tools/libs/guest)
of the Xen repository.

## Responsibilities

### Allocating the boot memory for new & migrated VMs

One important responsibility of `libxenguest` is creating the memory layout
of new and migrated VMs.

The [boot memory setup](../../../xenopsd/walkthroughs/VM.build/xenguest/setup_mem)
of `xenguest` and `libxl` (used by the `xl` CLI command) call
[xc_dom_boot_mem_init()](xc_dom_boot_mem_init) which dispatches the
call to
[meminit_hvm()](https://github.com/xen-project/xen/blob/de0254b9/tools/libs/guest/xg_dom_x86.c#L1348-L1649)
and
[meminit_pv()](https://github.com/xen-project/xen/blob/de0254b9/tools/libs/guest/xg_dom_x86.c#L1183-L1333) which layout, allocate and populate the boot memory of domains.

## Functions

{{% children description=true %}}