---
title: Add qcow tool to allow VDI import/export
layout: default
design_doc: true
revision: 1
status: proposed
---

# Introduction

At XCP-ng, we are working on overcoming the 2TiB limitation for VM disks while
preserving essential features such as snapshots, copy-on-write capabilities, and
live migration.

To achieve this, we are introducing Qcow2 support in SMAPI and the blktap driver.
With the alpha release, we can:
    - Create a VDI
    - Snapshot it
    - Export and import it to/from XVA
    - Perform full backups

However, we currently cannot export a VDI to a Qcow2 file, nor import one.

The purpose of this design proposal is to outline a solution for implementing VDI
import/export in Qcow2 format.

# Design Proposal

The import and export of VHD-based VDIs currently rely on *vhd-tool*, which is
responsible for streaming data between a VDI and a file. It supports both Raw and
VHD formats, but not Qcow2.

There is an existing tool called [qcow-tool](https://opam.ocaml.org/packages/qcow-tool/)
originally packaged by MirageOS. It is no longer actively maintained, but it can
produce Qcow files readable by QEMU.

Currently, *qcow-tool* does not support streaming, but we propose to add this
capability. This means replicating the approach used in *vhd-tool*, where data is
pushed to a socket.

We have contacted the original developer, David Scott, and there are no objections
to us maintaining the tool if needed.

Therefore, the most appropriate way to enable Qcow2 import/export in XAPI is to
add streaming support to `qcow-tool`.

# XenAPI changes

## The workflow

- The export and import of VDIs are handled by the XAPI HTTP server:
  - `GET /export_raw_vdi`
  - `PUT /import_raw_vdi`
- The corresponding handlers are `Export_raw_vdi.handler` and
  `Import_raw_vdi.handler`.
- Since the format is checked in the handler, we need to add support for `Qcow2`,
  as currently only `Raw`, `Tar`, and `Vhd` are supported.
- This requires adding a new type in the `Importexport.Format` module and a new
  content type: `"application/qcow2"`.
- This allows the format to be properly decoded. Currently, all formats use a
  wrapper called `Vhd_tool_wrapper`, which sets up parameters for `vhd-tool`.
  We need to add a new wrapper for the Qcow2 format, which will instead use
  `qcow-tool`, a tool that we will package (see the section below).
- The new wrapper will be responsible for setting up parameters (source,
  destination, etc.). Since it only manages Qcow2 files, we don’t need to pass
  additional format information.
- The format (`qcow2`) will be specified in the URI. For example:
  - `/import_raw_vdi?session_id=<OpaqueRef>&task_id=<OpaqueRef>&vdi=<OpaqueRef>&format=qcow2`

## Adding and modifying qcow-tool

- We need to package [qcow-tool](https://opam.ocaml.org/packages/qcow-tool).
- This new tool will be called from `ocaml/xapi/qcow_tool_wrapper.ml`, as
  described in the previous section.

- To export a VDI to a Qcow2 file, we need to add functionality similar to
  `Vhd_tool_wrapper.send`, which calls `vhd-tool stream`.
  - It writes data from the source to a destination. Unlike `vhd-tool`, which
    supports multiple destinations, we will only support Qcow2 files.

- To import a VDI from a Qcow2 file, we need to implement functionality similar
  to `Vhd_tool_wrapper.receive`, which calls `vhd-tool serve`.
  - This is the reverse of the export process. As with export, we will only
    support a single type of import: from a Qcow2 file.
