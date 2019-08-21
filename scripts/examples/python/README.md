Usage
=====

To install the package, enable the virtual environment where it's going to be used and run
`$ pip install XenAPI`

Examples
--------

The [examples](https://github.com/xapi-project/xen-api/tree/master/scripts/examples/python) will not work unless they have been placed in the same directory as `XenAPI.py` or `XenAPI` package from PyPI has been installed (`pip install XenAPI`)

Packaging
=========

`setup.py` is generated using an ocaml binary that fetches the api version string from xapi. An opam switch with the [xs-opam](https://github.com/xapi-project/xs-opam) repository is needed in order to build the binary.

To build the package `setuptools>=38.6.0` and `wheel` need to be installed in the system or in the active python virtualenv.

To build, use the command `make`.
