---
title: Add qcow tool to allow VDI import/export
layout: default
design_doc: true
revision: 1
status: proposed
---

# Introduction

At XCP-ng, we are working on overcoming the 2TiB limitation for VM disks while preserving essential
features such as snapshots, copy-on-write capabilities, and live migration.

To achieve this, we are introducing Qcow2 support in SMAPI and the blktap driver. With the alpha
release, we can:
    - Create a VDI
    - Snapshot it
    - Export and import it to/from XVA
    - Perform full backups

However, we currently cannot export a VDI to a Qcow2 file, nor import one.

The purpose of this design proposal is to outline a solution for implementing VDI import/export in
Qcow2 format.

# Design Proposal

The import and export of VHD-based VDIs currently rely on *vhd-tool*, which is responsible for streaming
data between a VDI and a file. It supports both Raw and VHD formats, but not Qcow2.

There is an existing tool called [qcow-tool](https://opam.ocaml.org/packages/qcow-tool/) originally
packaged by MirageOS. It is no longer actively maintained, but it can produce files readable by QEMU.

Currently, *qcow-tool* does not support streaming, but we propose to add this capability. This means
replicating the approach used in *vhd-tool*, where data is pushed to a socket.

We have contacted the original developer, David Scott, and there are no objections to us maintaining
the tool if needed.

Therefore, the most appropriate way to enable Qcow2 import/export in XAPI is to add streaming support
to qcow-tool.

# XenAPI changes

## The workflow

- The export and import of VDIS are handled by the XAPI HTTP server:
  - `GET /export_raw_vdi`
  - `PUT /import_raw_vdi`
- The corresponding handlers are `Export_raw_vdi.handler` and `Import_raw_vdi.handler`.
- As the format is checked in there handler we need to add support for `Qcow2` format, as currently
only `Raw`, `Tar` and `Vhd` are supported.
- Once the format is decoded, a wrapper `Vhd_tool_wrapper` is called to set up parameters for 
`vhd-tool`.
  - We will add a new wrapper for `Qcow2` that instead calls the newly added `qcow-tool`
  (See section below).
  - `vhd-tool` streams data between the file and the unix file descriptor of the HTTP server.
  We will need to implement a similar mechanism in `qcow-tool` 

## Adding and modifying qcow-tool

- We need to package [qcow-tool](https://opam.ocaml.org/packages/qcow-tool)
- Add support in *qcow-tool* of the equivalent of `vhd-tool stream` and `vhd-tool serve` commands.
  - the *serve* command will read the content of a VDI and write it to a file.
  - the *stream* command will read the contents of a file and steam it into the VDI.
