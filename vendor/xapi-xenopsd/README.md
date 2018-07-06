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

