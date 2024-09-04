Packaging
=========

`setup.py` is generated using an ocaml binary that fetches the api version string from xapi. An opam switch with the [xs-opam](https://github.com/xapi-project/xs-opam) repository is needed in order to build the binary.

To build the package `setuptools>=38.6.0` and `wheel` need to be installed in the system or in the active python virtualenv.

To build, use the command `make`.
