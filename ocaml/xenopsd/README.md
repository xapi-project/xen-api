[![Build Status](https://travis-ci.org/xapi-project/xenopsd.svg?branch=master)](https://travis-ci.org/xapi-project/xenopsd)
[![Lines of Code](https://tokei.rs/b1/github/xapi-project/xenopsd)](https://github.com/xapi-project/xenopsd)

xenopsd: a simple VM manager
============================

xenopsd manages VMs running

  * on Xen, via direct libxc calls
  * on Xen/KVM via libvirt [experimental]
  * on KVM via qemu directly [experimental]

and provides a simple RPC control interface to the layer above (typically xapi).

## Coverage Profiling

This code can be profiled for coverage. See [COVERAGE.md].


[COVERAGE.md]:    ./COVERAGE.md

squeezed: a xen host memory ballooning daemon
---------------------------------------------

Squeezed uses [ballooning](http://static.usenix.org/events/osdi02/tech/full_papers/waldspurger/waldspurger_html/node6.html)
to move memory between running VMs. It is able to:

  1. avoid wasting host memory: unused memory can be gifted to VMs
  2. share memory according to a configured policy, so some VMs will use more than others
  3. "squeeze" existing VMs to make room to start new VMs.

Squeezed is an optional component of the [xapi toolstack](http://wiki.xen.org/wiki/Choice_of_Toolstacks).

documentation
-------------

- [Architecture](squeezed/doc/architecture/README.md): a high-level overview of Squeezed.
- [Design](squeezed/doc/design/README.md): discover the low-level details, formats, protocols,
  concurrency etc.
