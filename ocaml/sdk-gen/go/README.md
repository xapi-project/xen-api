# XenServer SDK for Go

Copyright (c) 2023-2024 Cloud Software Group, Inc. All Rights Reserved.

XenServer SDK for Go is a complete SDK for XenServer, exposing the XenServer
API as Go module. It is written in Go.

XenServer SDK for Go includes a struct for every API class, and a method for each API
call, so API documentation and examples written for other languages will apply
equally well to Go. In particular, the SDK Guide and the Management API Guide
are ideal for developers wishing to use XenServer SDK for Go.

XenServer SDK for Go is free software. You can redistribute and modify it under the
terms of the BSD 2-Clause license. See LICENSE for details.

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

- `XenServerGo\src`: contains all Go source files which can be used as a local module for other Go projects. Every API object is associated with one Go file.

## Getting Started

Extract the contents of this archive.

A. Navigate to the extracted `XenServer-SDK\XenServerGo\src` directory and copy the whole folder `src` into your Go project directory.

B. To use the XenServer SDK for Go as a local Go module, include the following line into the `go.mod` file under your Go project:

```
replace xenapi => ./src
```
You can then import the XenServer SDK for Go with the following command:

```
import "xenapi"
```

C. Before building your project, run the following Go commands.

```
go get -u all
go mod tidy
 ```
