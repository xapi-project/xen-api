ocaml-pci
=========

[![Build Status](https://travis-ci.org/simonjbeaumont/ocaml-pci.svg?branch=master)](https://travis-ci.org/simonjbeaumont/ocaml-pci)
[![Coverage Status](https://coveralls.io/repos/simonjbeaumont/ocaml-pci/badge.svg?branch=master)](https://coveralls.io/r/simonjbeaumont/ocaml-pci?branch=master)

An OCaml library exposing an API over bindings to `libpci`.

This library uses Ctypes to generate type-safe bindings to `libpci` which is
part of "[The PCI Utilities](http://mj.ucw.cz/sw/pciutils/)" package shipped
with most operating systems.

Rather than being a completely transparent set of bindings, this library
exposes an API to wrap some of the composite functions to ensure correct memory
allocation and cleanup. This should reduce memory leaks and segfaults which are
possible with incorrect use of `libpci`.

## Installation

The easiest way to install this library is to use `opam`:

    opam install pci

However, if you don't use `opam`, you can compile from source by cloning this
repo and executing the usual dance:

    ./configure
    make
    make install

## Documentation

The API documentation for this library is automatically generated from source
using `ocamldoc` and is available
[online](http://simonjbeaumont.github.io/ocaml-pci).

## Usage

There is an example of a minimal `lspci` clone in the `examples/` directory.
The interface is intended to be as simple as possible. Here is an example of
listing the devices on your system:

```ocaml
open Pci
let devs = with_access get_devices in
List.iter (fun d ->
  let open Pci_dev in
  Printf.printf "Device: %04x:%02x:%02x.%d\n" d.domain d.bus d.dev d.func
) devs
```
