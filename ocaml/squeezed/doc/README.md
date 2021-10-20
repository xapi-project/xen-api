Squeezed: the developer handbook
===============================

Squeezed is the [xapi-project](http://github.com/xapi-project) host
memory manager (aka balloon driver driver). Squeezed uses ballooning
to move memory between running VMs, to avoid wasting host memory.

Principles
----------

1. avoid wasting host memory: unused memory should be put to use by returning
   it to VMs
2. memory should be shared in proportion to the configured policy
3. operate entirely at the level of domains (not VMs), and be independent of
   Xen toolstack

Contents
--------
- [Architecture](architecture/README.md): a high-level overview of Squeezed.
- [Design](design/README.md): discover the low-level details, formats, protocols,
  concurrency etc.
