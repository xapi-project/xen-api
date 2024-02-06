# XenServer.NET

Copyright (c) 2007-2024 Cloud Software Group, Inc. All Rights Reserved.

XenServer.NET is a complete SDK for XenServer, exposing the XenServer
API as .NET classes. It is written in C#.

XenServer.NET includes a class for every API class, and a method for each API
call, so API documentation and examples written for other languages will apply
equally well to .NET. In particular, the SDK Guide and the Management API Guide
are ideal for developers wishing to use XenServer.NET.

XenServer.NET is free software. You can redistribute and modify it under the
terms of the BSD 2-Clause license. See LICENSE.txt for details.


## Reference

For XenServer documentation see https://docs.xenserver.com

The XenServer Management API Reference is available at
https://docs.xenserver.com/en-us/xenserver/8/developer/management-api

The XenServer Software Development Kit Guide is available at
https://docs.xenserver.com/en-us/xenserver/8/developer/sdk-guide

A number of examples to help you get started with the SDK is available at
https://github.com/xenserver/xenserver-samples

For community content, blogs, and downloads, visit
https://www.xenserver.com/blogs and https://www.citrix.com/community

To network with other developers using XenServer visit
https://discussions.citrix.com/forum/101-hypervisor-formerly-xenserver


## Prerequisites

This library requires .NET Standard 2.0.


## Dependencies

XenServer.NET is dependent upon the following libraries:

- Newtonsoft JSON.NET by James Newton-King (see https://www.newtonsoft.com).
  JSON.NET is licensed under the MIT license.


## Downloads


This archive contains the following folders that are relevant to .NET developers:
- XenServer.NET: contains the ready compiled binaries in the form of a NuGet package.
- XenServer.NET\src: contains the source code shipped as a Visual Studio project.


## Getting Started


Extract the contents of this archive.

A. To build the source code:
  1. Open the project XenServer.csproj in Visual Studio.
  2. You should now be ready to build the source code.

B. To use the NuGet package in your code (offline):
  1. Add the location of the package as a NuGet source:

  ```pwsh
  nuget sources Add -Name "Offline Package" -Source "/full/path/to/package/directory"
  ```

  2. Install the package

  ```pwsh
  nuget install XenServer.NET
  ```
