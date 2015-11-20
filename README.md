Shared-memory protocols for transmitting RRD data
=================================================

[![Build status](https://travis-ci.org/xapi-project/rrd-transport.png?branch=master)](https://travis-ci.org/xapi-project/rrd-transport)
[![Coverage Status](https://coveralls.io/repos/xapi-project/rrd-transport/badge.svg?branch=master&service=github)](https://coveralls.io/github/xapi-project/rrd-transport?branch=master)

Allows writing and reading of RRD datasources to and from files and shared Xen
pages.

Build dependencies:

* [cmdliner](https://github.com/dbuenzli/cmdliner) (for test executables only)
* [cstruct](https://github.com/avsm/ocaml-cstruct)
* [xcp](https://github.com/xapi-project/xcp-idl)
* [xcp-rrd](https://github.com/xen-org/xcp-rrd)
* [xen-gnt](https://github.com/mirage/ocaml-gnt)
* [crc](https://github.com/xapi-project/ocaml-crc)
