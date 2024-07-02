---
title: VM import/export
layout: default
---

VMs can be exported to a file and later imported to any Xapi host. The export
protocol is a simple HTTP(S) GET, which should be sent to the Pool master.
Authorization is either via a pre-created `session_id` or by HTTP basic
authentication (particularly useful on the command-line).
The VM to export is specified either by UUID or by reference. To keep track of
the export, a task can be created and passed in using its reference. Note that
Xapi may send an HTTP redirect if a different host has better access to the
disk data.

The following arguments are passed as URI query parameters or HTTP cookies:

Argument        | Description
----------------|---------------------------------------------------------
session_id      | the reference of the session being used to authenticate; required only when not using HTTP basic authentication
task_id         | the reference of the task object with which to keep track of the operation; optional, required only if you have created a task object to keep track of the export
ref             | the reference of the VM; required only if not using the UUID
uuid            | the UUID of the VM; required only if not using the reference
use_compression | an optional boolean "true" or "false" (defaulting to "false"). If "true" then the output will be gzip-compressed before transmission.


For example, using the Linux command line tool cURL:

```sh
$ curl http://root:foo@myxenserver1/export?uuid=<vm_uuid> -o <exportfile>
```

will export the specified VM to the file `exportfile`.

To export just the metadata, use the URI `http://server/export_metadata`.

The import protocol is similar, using HTTP(S) PUT. The `session_id` and `task_id` arguments are as for the export. The `ref` and `uuid` are not used; a new reference and uuid will be generated for the VM. There are some additional parameters:

Argument    | Description
------------|---------------------------------------------------------
restore     | if `true`, the import is treated as replacing the original VM - the implication of this currently is that the MAC addresses on the VIFs are exactly as the export was, which will lead to conflicts if the original VM is still being run.
force       | if `true`, any checksum failures will be ignored (the default is to destroy the VM if a checksum error is detected)
sr_id       | the reference of an SR into which the VM should be imported. The default behavior is to import into the `Pool.default_SR`

Note there is no need to specify whether the export is compressed, as Xapi
will automatically detect and decompress gzip-encoded streams.

For example, again using cURL:

```sh
curl -T <exportfile> http://root:foo@myxenserver2/import
```

will import the VM to the default SR on the server.

> **Note**
>
> Note that if no default SR has been set, and no `sr_uuid` is specified, the error message `DEFAULT_SR_NOT_FOUND` is returned.

Another example:

```sh
curl -T <exportfile> http://root:foo@myxenserver2/import?sr_id=<ref_of_sr>
```

will import the VM to the specified SR on the server.

To import just the metadata, use the URI `http://server/import_metadata`

Legacy VM Import Format
=======================

This section describes the legacy VM import/export format and is for historical
interest only. It should be updated to describe the current format, see
[issue 64](https://github.com/xapi-project/xapi-project.github.io/issues/64)


Xapi supports a human-readable legacy VM input format called XVA. This section describes the syntax and structure of XVA.

An XVA consists of a directory containing XML metadata and a set of disk images. A VM represented by an XVA is not intended to be directly executable. Data within an XVA package is compressed and intended for either archiving on permanent storage or for being transmitted to a VM server - such as a XenServer host - where it can be decompressed and executed.

XVA is a hypervisor-neutral packaging format; it should be possible to create simple tools to instantiate an XVA VM on any other platform. XVA does not specify any particular runtime format; for example disks may be instantiated as file images, LVM volumes, QCoW images, VMDK or VHD images. An XVA VM may be instantiated any number of times, each instantiation may have a different runtime format.

XVA does not:

-   specify any particular serialization or transport format

-   provide any mechanism for customizing VMs (or templates) on install

-   address how a VM may be upgraded post-install

-   define how multiple VMs, acting as an appliance, may communicate

These issues are all addressed by the related Open Virtual Appliance specification.

An XVA is a directory containing, at a minimum, a file called `ova.xml`. This file describes the VM contained within the XVA and is described in Section 3.2. Disks are stored within sub-directories and are referenced from the ova.xml. The format of disk data is described later in Section 3.3.

The following terms will be used in the rest of the chapter:

-   HVM: a mode in which unmodified OS kernels run with the help of virtualization support in the hardware.

-   PV: a mode in which specially modified "paravirtualized" kernels run explicitly on top of a hypervisor without requiring hardware support for virtualization.

The "ova.xml" file contains the following elements:

```xml
<appliance version="0.1">
```

The number in the attribute "version" indicates the version of this specification to which the XVA is constructed; in this case version 0.1. Inside the \<appliance\> there is exactly one \<vm\>: (in the OVA specification, multiple \<vm\>s are permitted)

```xml
<vm name="name">
```

Each `<vm>` element describes one VM. The "name" attribute is for future internal use only and must be unique within the ova.xml file. The "name" attribute is permitted to be any valid UTF-8 string. Inside each \<vm\> tag are the following compulsory elements:

```xml
<label>... text ... </label>
```

A short name for the VM to be displayed in a UI.

```xml
<shortdesc> ... description ... </shortdesc>
```

A description for the VM to be displayed in the UI. Note that for both `<label>` and `<shortdesc>` contents, leading and trailing whitespace will be ignored.

```xml
<config mem_set="268435456" vcpus="1"/>
```

The `<config>` element has attributes which describe the amount of memory in bytes (`mem_set`) and number of CPUs (VCPUs) the VM should have.

Each `<vm>` has zero or more `<vbd>` elements representing block devices which look like the following:

```xml
<vbd device="sda" function="root" mode="w" vdi="vdi_sda"/>
```

The attributes have the following meanings:

* `device`: name of the physical device to expose to the VM. For linux guests
   we use "sd[a-z]" and for windows guests we use "hd[a-d]".
* `function`: if marked as "root", this disk will be used to boot the guest.
   (NB this does not imply the existence of the Linux root i.e. / filesystem)
   Only one device should be marked as "root". See Section 3.4 describing VM
   booting. Any other string is ignored.
* `mode`: either "w" or "ro" if the device is to be read/write or read-only
* `vdi`: the name of the disk image (represented by a `<vdi>` element) to which
  this block device is connected

Each `<vm>` may have an optional `<hacks>` section like the following:

```xml
<hacks is_hvm="false" kernel_boot_cmdline="root=/dev/sda1 ro"/>
```

The `<hacks>` element will be removed in future. The attribute `is_hvm` is
either `true` or `false`, depending on whether the VM should be booted in HVM or not.
The `kernel_boot_cmdline` contains additional kernel commandline arguments when
booting a guest using pygrub.

In addition to a `<vm>` element, the `<appliance>` will contain zero or more
`<vdi>` elements like the following:

```xml
<vdi name="vdi_sda" size="5368709120" source="file://sda" type="dir-gzipped-chunks">
```

Each `<vdi>` corresponds to a disk image. The attributes have the following meanings:

* `name`: name of the VDI, referenced by the vdi attribute of `<vbd>`elements.
   Any valid UTF-8 string is permitted.
* `size`: size of the required image in bytes
* `source`: a URI describing where to find the data for the image, only
   file:// URIs are currently permitted and must describe paths relative to the
   directory containing the ova.xml
* `type`: describes the format of the disk data

A single disk image encoding is specified in which has type "dir-gzipped-chunks": Each image is represented by a directory containing a sequence of files as follows:

```sh
-rw-r--r-- 1 dscott xendev 458286013    Sep 18 09:51 chunk000000000.gz
-rw-r--r-- 1 dscott xendev 422271283    Sep 18 09:52 chunk000000001.gz
-rw-r--r-- 1 dscott xendev 395914244    Sep 18 09:53 chunk000000002.gz
-rw-r--r-- 1 dscott xendev 9452401      Sep 18 09:53 chunk000000003.gz
-rw-r--r-- 1 dscott xendev 1096066      Sep 18 09:53 chunk000000004.gz
-rw-r--r-- 1 dscott xendev 971976       Sep 18 09:53 chunk000000005.gz
-rw-r--r-- 1 dscott xendev 971976       Sep 18 09:53 chunk000000006.gz
-rw-r--r-- 1 dscott xendev 971976       Sep 18 09:53 chunk000000007.gz
-rw-r--r-- 1 dscott xendev 573930       Sep 18 09:53 chunk000000008.gz
```

Each file (named "chunk-XXXXXXXXX.gz") is a gzipped file containing exactly 1e9 bytes (1GB, not 1GiB) of raw block data. The small size was chosen to be safely under the maximum file size limits of several filesystems. If the files are gunzipped and then concatenated together, the original image is recovered.

Because the import and export of VMs can take some time to complete, an
asynchronous HTTP interface to the import and export operations is
provided. To perform an export using the XenServer API, construct
an HTTP GET call providing a valid session ID, task ID and VM UUID, as
shown in the following pseudo code:

    task = Task.create()
    result = HTTP.get(
      server, 80, "/export?session_id=&task_id=&ref=");

For the import operation, use an HTTP PUT call as demonstrated in the
following pseudo code:

    task = Task.create()
    result = HTTP.put(
      server, 80, "/import?session_id=&task_id=&ref=");
