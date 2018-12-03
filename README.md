# xapi-idl

[![Build Status](https://travis-ci.org/xapi-project/xcp-idl.svg?branch=master)](https://travis-ci.org/xapi-project/xcp-idl)
[![Coverage Status](https://coveralls.io/repos/github/xapi-project/xcp-idl/badge.svg)](https://coveralls.io/github/xapi-project/xcp-idl)

This repository contains

  1. interface definitions for xapi toolstack services
  2. common boilerplate for toolstack clients and servers, including
     * configuration file parsing
     * argument parsing
     * RPCs
  3. The following CLI tools for debugging:
     * lib/channel_helper.exe -- a channel passing helper CLI
     * memory/memory_cli.exe -- a squeezed debugging CLI
     * v6/v6_cli.exe -- a V6d debugging CLI
     * cluster/cluster_cli.exe -- a xapi-clusterd debugging CLI

      To build these, run: `jbuilder build path/to/exec.exe`.
      To run: `./_build/default/path/to/exec.exe`.
