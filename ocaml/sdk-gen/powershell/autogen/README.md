# XenServer PowerShell Module

Copyright (c) 2013-2024 Cloud Software Group, Inc. All Rights Reserved.

The XenServer PowerShell Module is a complete SDK for XenServer,
exposing the XenServer API as Windows PowerShell cmdlets.

The XenServer PowerShell Module includes a cmdlet for each API call,
so API documentation and examples written for other languages will apply equally
well to PowerShell. In particular, the SDK Guide and the Management API Guide
are ideal for developers wishing to use this module.

This module is free software. You can redistribute and modify it under the
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
<https://www.xenserver.com/blogs> and <https://www.citrix.com/community/>

To network with other developers using XenServer visit
<https://discussions.citrix.com/forum/101-hypervisor-formerly-xenserver/>

## Prerequisites

This library requires .NET 6.0 and PowerShell 7.2 or greater.

## Dependencies

The XenServer PowerShell Module is dependent upon the following libraries:

- Newtonsoft JSON.NET by James Newton-King (see <https://www.newtonsoft.com/>).
  JSON.NET is licensed under the MIT license.

- XenServer.NET by Cloud Software Group, Inc.
  XenServer.NET is a complete SDK for XenServer, exposing the XenServer
  API as .NET classes. It is written in C#.

## Folder Structure

This archive contains the following folders that are relevant to PowerShell users:

- `XenServerPowerShell\XenServerPSModule`: this is the XenServer PowerShell
  Module
- `XenServerPowerShell\src`: contains the C# source code for the XenServer
  cmdlets shipped as a Visual Studio project.

## Getting Started

1. Extract the contents of this archive.

    Note that some web browsers may mark the SDK ZIP file as "blocked" during
    the download. To import the module successfully you will need to unblock the
    archive before extracting its contents. To unblock the archive, right-click
    on it and launch the Properties dialog. Click the Unblock button, then the
    Apply or OK button.

2. Navigate to the extracted XenServerPowerShell directory and copy the whole
    folder XenServerPSModule into your PowerShell modules directory.

    - On Windows this will normally be `$env:UserProfile\Documents\PowerShell\Modules`
      for per-user configuration, or `$env:ProgramFiles\PowerShell\7\Modules` for
      system-wide configuration (note that `$env:ProgramFiles\WindowsPowerShell\Modules`
      is also acceptable).

    - On Linux this will be `~/.local/share/powershell/Modules` for per-user
      configuration, or `/usr/local/share/powershell/Modules` for system-wide
      configuration.

    For more information see PowerShell's documentation on module paths:

        PS> Get-Help about_PSModulePath

3. Open a PowerShell 7 prompt as administrator.

    To do this, open the Windows Start menu by clicking the Start icon, find
    the item PowerShell 7, right click it and select Run as administrator.

4. Determine the current execution policy:

```ps
        PS> Get-ExecutionPolicy
```

  If the current policy is Restricted, you need to set it to `RemoteSigned`:

```ps
        PS> Set-ExecutionPolicy RemoteSigned
```

  You should understand the security implications of this change. If you are
  unsure, see PowerShell's documentation on execution policies:

```ps
        PS> Get-Help about_Execution_Policies
```

  If the current policy is AllSigned, you will be able to use the XenServer
  PowerShell module, but it will be inconvenient because this policy requires
  even scripts that you write on the local computer to be signed. You may want
  to change it to `RemoteSigned`, as above.

  If the current policy is `RemoteSigned`, `ByPass`, or `Unrestricted` there is
  nothing to do.

5. Exit the privileged instance of PowerShell.

6. Open a PowerShell 7 prompt as a regular user (click Start > PowerShell 7)
    and import the XenServer PowerShell Module:

```ps
        PS> Import-Module XenServerPSModule
```

7. If you wish to load specific environment settings when the XenServer
    PowerShell module is loaded, create the file `XenServerProfile.ps1` and put it
    in the folder containing your `$PROFILE` file for per-user configuration, or
    in `$PSHOME` for system-wide configuration.

    - On Windows these will normally be `$env:UserProfile\Documents\PowerShell`
      for per-user configuration, or `$env:ProgramFiles\PowerShell\7` for
      system-wide configuration.

    - On Linux these will be `~/.config/powershell` for per-user configuration,
      or `/opt/microsoft/powershell/7` for system-wide configuration.

8. For an overview of the XenServer PowerShell Module type:

```ps
        PS> Get-Help about_XenServer
```

   You can obtain a list of all available cmdlets by typing:

```ps
        PS> Get-Command -Module XenServerPSModule
```

   For help with a specific command use:

```ps
        PS> Get-Help [CommandName]
```

9. Here is a quick example of opening a session and making a call to a server:

```ps
        PS> Connect-XenServer -Url https://<servername>
        PS> Get-XenVM
        PS> Disconnect-XenServer
```

## Building and Debugging the Source Code

1. Open the project `XenServerPowerShell.csproj` in Visual Studio 2022. You should
   now be ready to build the source code.

2. If in Debug mode, clicking Start will launch a PowerShell 7 prompt as an
   external process, and import the compiled `XenServerPowerShell.dll` as a module
   (without, however, processing the scripts, types, and formats shipped within
   the `XenServerPSModule`). You should now be ready to debug the cmdlets.
