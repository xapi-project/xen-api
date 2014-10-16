Suspend image considerations
============================

We are currently (Dec 2013) undergoing a transition from the 'classic' xenopsd
backend (built upon calls to libxc) to the 'xenlight' backend built on top of
the officially supported libxl API.

During this work, we have come across an incompatibility between the suspend
images created using the 'classic' backend and those created using the new
libxl-based backend. This needed to be fixed to enable RPU to any new version
of XenServer.

Historic 'classic' stack
------------------------
Prior to this work, xenopsd was involved in the construction of the suspend
image and we ended up with an image with the following format:

    +-----------------------------+
    | "XenSavedDomain\n"          |  <-- added by xenopsd-classic
    |-----------------------------|
    |  Memory image dump          |  <-- libxc
    |-----------------------------|
    | "QemuDeviceModelRecord\n"   |
    |  <size of following record> |  <-- added by xenopsd-classic
    |  (a 32-bit big-endian int)  |
    |-----------------------------|
    | "QEVM"                      |  <-- libxc/qemu
    |  Qemu device record         |
    +-----------------------------+

We have also been carrying a patch in the Xen patchqueue against
xc_domain_restore. This patch (revert_qemu_tail.patch) stopped
xc_domain_restore from attempting to read past the memory image dump. At which
point xenopsd-classic would just take over and restore what it had put there.

Requirements for new stack
--------------------------
For xenopsd-xenlight to work, we need to operate without the
revert_qemu_tail.patch since libxl assumes it is operating on top of an
upstream libxc.

We need the following relationship between suspend images created on one
backend being able to be restored on another backend. Where the backends are
old-classic (OC), new-classic (NC) and xenlight (XL). Obviously all suspend
images created on any backend must be able to be restored on the same backend:

                    OC _______ NC _______ XL
                     \  >>>>>      >>>>>  /
                      \__________________/
                        >>>>>>>>>>>>>>>>

It turns out this was not so simple. After removing the patch against
xc_domain_restore and allowing libxc to restore the hvm_buffer_tail, we found
that supsend images created with OC (detailed in the previous section) are not
of a valid format for two reasons:

    i. The "XenSavedDomain\n" was extraneous;
   ii. The Qemu signature section (prior to the record) is not of valid form.

It turns out that the section with the Qemu signature can be one of the
following:

    a. "QemuDeviceModelRecord" (NB. no newline) followed by the record to EOF;
    b. "DeviceModelRecord0002" then a uint32_t length followed by record;
    c. "RemusDeviceModelState" then a uint32_t length followed by record;

The old-classic (OC) backend not only uses an invalid signature (since it
contains a trailing newline) but it also includes a length, _and_ the length is
in big-endian when the uint32_t is seen to be little-endian.

We considered creating a proxy for the fd in the incompatible cases but since
this would need to be a 22-lookahead byte-by-byte proxy this was deemed
impracticle. Instead we have made patched libxc with a much simpler patch to
understand this legacy format.

Because peek-ahead is not possible on pipes, the patch for (ii) needed to be
applied at a point where the hvm tail had been read completely. We piggy-backed
on the point after (a) had been detected. At this point the remainder of the fd
is buffered (only around 7k) and the magic "QEVM" is expected at the head of
this buffer. So we simply added a patch to check if there was a pesky newline
and the buffer[5:8] was "QEVM" and if it was we could discard the first
5 bytes:

                                  0    1    2    3    4    5   6   7   8
    Legacy format from OC:  [...| \n | \x | \x | \x | \x | Q | E | V | M |...]

    Required at this point: [...|  Q |  E |  V |  M |...]

Changes made
------------
To make the above use-cases work, we have made the following changes:

    1. Make new-classic (NC) not restore Qemu tail (let libxc do it)
        xenopsd.git:ef3bf4b

    2. Make new-classic use valid signature (b) for future restore images
        xenopsd.git:9ccef3e

    3. Make xc_domain_restore in libxc understand legacy xenopsd (OC) format
        xen-4.3.pq.hg:libxc-restore-legacy-image.patch

    4. Remove revert-qemu-tail.patch from Xen patchqueue
        xen-4.3.pq.hg:3f0e16f2141e

    5. Make xenlight (XL) use "XenSavedDomain\n" start-of-image signature
        xenopsd.git:dcda545

This has made the required use-cases work as follows:

                    OC __134__ NC __245__ XL
                     \  >>>>>      >>>>>  /
                      \_______345________/
                        >>>>>>>>>>>>>>>>

And the suspend-resume on same backends work by virtue of:

    OC --> OC : Just works
    NC --> NC : By 1,2,4
    XL --> XL : By 4 (5 is used but not required)

New components
--------------
The output of the changes above are:
    * A new xenops-xc binary for NC
    * A new xenops-xl binary for XL
    * A new libxenguest.4.3 for both of NC and XL

Future considerations
---------------------
This should serve as a useful reference when considering making changes to the
suspend image in any way.
