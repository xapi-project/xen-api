This library implements support for efficient handling of I/O memory pages on
Unix and Xen.

IO pages are page-aligned, and wrapped in the `Cstruct` library to avoid
copying the data contained within the page.
