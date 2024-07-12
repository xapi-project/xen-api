---
title: patches in VDIs
layout: default
design_doc: true
revision: 1
status: proposed
---

"Patches" are signed binary blobs which can be queried and applied.
They are stored in the dom0 filesystem under `/var/patch`. Unfortunately
the patches can be quite large -- imagine a repo full of RPMs -- and
the dom0 filesystem is usually quite small, so it can be difficult
to upload and apply some patches.

Instead of writing patches to the dom0 filesystem, we shall write them
to disk images (VDIs) instead. We can then take advantage of features like

- shared storage
- cross-host `VDI.copy`

to manage the patches.

XenAPI changes
==============

1. Add a field `pool_patch.VDI` of type `Ref(VDI)`. When a new patch is
   stored in a VDI, it will be referenced here. Older patches and cleaned
   patches will have invalid references here.

2. The HTTP handler for uploading patches will choose an SR to stream the
   patch into. It will prefer to use the `pool.default_SR` and fall back
   to choosing an SR on the master whose driver supports the `VDI_CLONE`
   capability: we want the ability to fast clone patches, one per host
   concurrently installing them. A VDI will be created whose size is 4x
   the apparent size of the patch, defaulting to 4GiB if we have no size
   information (i.e. no `content-length` header)

3. `pool_patch.clean_on_host` will be deprecated. It will still try to
   clean a patch *from the local filesystem* but this is pointless for
   the new VDI patch uploads.

4. `pool_patch.clean` will be deprecated. It will still try to clean a patch
   from the *local filesystem* of the master but this is pointless for the
   new VDI patch uploads.

4. `pool_patch.pool_clean` will be deprecated. It will destroy any associated
   patch VDI. Users will be encouraged to call `VDI.destroy` instead.



Changes beneath the XenAPI
==========================

1. `pool_patch` records will only be deleted if both the `filename` field
   refers to a missing file on the master *and* the `VDI` field is a dangling
   reference

2. Patches stored in VDIs will be stored within a filesystem, like we used
   to do with suspend images. This is needed because (a) we want to execute
   the patches and block devices cannot be executed; and (b) we can use
   spare space in the VDI as temporary scratch space during the patch
   application process. Within the VDI we will call patches `patch` rather
   than using a complicated filename.

3. When a host wishes to apply a patch it will call `VDI.copy` to duplicate
   the VDI to a locally-accessible SR, mount the filesystem and execute it.
   If the patch is still in the master's dom0 filesystem then it will fall
   back to the HTTP handler.


Summary of the impact on the admin
==================================

- There will nolonger be a size limit on hotfixes imposed by the mechanism
  itself.
- There must be enough free space in an SR connected to the host to be able
  to apply a patch on that host.
