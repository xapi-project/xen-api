# XenServer SDK for Go

Copyright (c) 2023-2024 Cloud Software Group, Inc. All Rights Reserved.

XenServer SDK for Go is a complete SDK for XenServer, exposing the XenServer
API as Go module. It is written in Go.

XenServer SDK for Go includes a struct for every API class, and a method for each API
call, so API documentation and examples written for other languages will apply
equally well to Go. In particular, the SDK Guide and the Management API Guide
are ideal for developers wishing to use XenServer SDK for Go.

XenServer SDK for Go is free software. You can redistribute and modify it under the
terms of the BSD 2-Clause license. See LICENSE.txt for details.

## Reference

For XenServer documentation see <https://docs.xenserver.com>

The XenServer Management API Reference is available at
<https://docs.xenserver.com/en-us/xenserver/8/developer/management-api>

The XenServer Software Development Kit Guide is available at
<https://docs.xenserver.com/en-us/xenserver/8/developer/sdk-guide>

A number of examples to help you get started with the SDK is available at
<https://github.com/xenserver/xenserver-samples>

For community content, blogs, and downloads, visit
<https://www.xenserver.com/blogs> and <https://www.citrix.com/community>

To network with other developers using XenServer visit
<https://discussions.citrix.com/forum/101-hypervisor-formerly-xenserver>

## Prerequisites

This library requires Go 1.22 or greater.

## Folder Structure

This archive contains the following folders that are relevant to Go developers:

- `XenServerGo\src`: contains the Go source files can be used as the local module in a Go project.

## Getting Started

Extract the contents of this archive.

A. To set up the local go module:

  1. Create a new folder in your Go project, eg. `XenServerGo`
  2. Copy all files in `XenServerGo\src` to the new folder

B. To use the XenServer module for Go in your Go project:

  1. Add the following lines to your go.mod file:

  ```
  replace <project-package-name>/XenServerGo => ./XenServerGo
  ```

  2. Run the command: 

  ```
  go mod tidy
  ```

  3. Use the XenServer module for Go in file as follows: 

  ```
  import (
	xenapi "<project-package-name>/XenServerGo"
   )
  ```
