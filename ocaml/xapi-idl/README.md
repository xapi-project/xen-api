# xapi-idl

[![Coverage Status](https://coveralls.io/repos/github/xapi-project/xcp-idl/badge.svg)](https://coveralls.io/github/xapi-project/xcp-idl)

This repository contains

  1. interface definitions for xapi toolstack services
  2. common boilerplate for toolstack clients and servers, including
     * configuration file parsing
     * argument parsing
     * RPCs
  3. The following CLI tools for debugging:
     * memory/memory_cli.exe -- a squeezed debugging CLI
     * v6/v6_cli.exe -- a V6d debugging CLI
     * cluster/cluster_cli.exe -- a xapi-clusterd debugging CLI

      To build these, run: `jbuilder build path/to/exec.exe`.
      To run: `./_build/default/path/to/exec.exe`.

## Tests

When changes are made to the interface, one needs to update the corresponding generated `lib_test/test_data` as well.
This can be done by running `dune test` first and then find the corresponding `test_data_path` in the `GenPath` module
inside the appropriate test file. For example, the `cluster_interface_test.ml` has a `test_data_path="cluster_gen"`. 
Find such file inside `_build/default/ocaml/xapi-idl/lib-test/<test_data_path>` and copy it over into the `test_data_path` 
defined by the `OldPath` module in your test ml file.
