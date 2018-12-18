Suspend image format
--------------------

Example suspend image layout:

    +----------------------------+
    | 1. Suspend image signature |
    +============================+
    | 2.0 Xenops header          |
    | 2.1 Xenops record          |
    +============================+
    | 3.0 Libxc header           |
    | 3.1 Libxc record           |
    +============================+
    | 4.0 Qemu header            |
    | 4.1 Qemu save record       |
    +============================+
    | 5.0 End_of_image footer    |
    +----------------------------+

A suspend image is now constucted as a series of header-record pairs. The
initial signature (1.) is used to determine whether we are dealing with the
unstructured, "legacy" suspend image or the new, structured format.

Each header is two 64-bit integers: the first identifies the header type and
the second is the length of the record that follows in bytes. The following
types have been defined (the ones marked with a (*) have yet to be
implemented):

    * Xenops       : Metadata for the suspend image
    * Libxc        : The result of a xc_domain_save
    * Libxl*       : Not implemented
    * Libxc_legacy : Marked as a libxc record saved using pre-Xen-4.5
    * Qemu_trad    : The qemu save file for the Qemu used in XenServer
    * Qemu_xen*    : Not implemented
    * Demu*        : Not implemented
    * Varstored*   : Not implemented
    * End_of_image : A footer marker to denote the end of the suspend image

Some of the above types do not have the notion of a length since they cannot be
known upfront before saving and also are delegated to other layers of the stack
on restoring. Specifically these are the memory image sections, libxc and
libxl.

