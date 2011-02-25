
INTRODUCTION

   This README describes the Software Development Kit (SDK) for the XenServer
   4.0 product family. The SDK provides programmatic access to the extensive
   set of XenServer management features and tools.

SYSTEM REQUIREMENTS

   The SDK is provided in the form of a Xen Virtual Appliance (XVA). The XVA
   can be imported into XenServer 4.0 or above. In order to use the Windows
   CLI, you must first install the [1].NET Version 2.0 runtime on your Windows
   client system.

INSTRUCTIONS FOR INSTALLING THE SDK

     * Download the zip file and unzip it on your client system where you have
       XenCenter installed
     * On the XenCenter File menu, click on "Import VM" and browse to the
       unzipped directory tree.
     * Double-click on the "sdk" and then click OK.
     * When the import has finished click on the "Network" tab of the XenVM and
       inspect the networking configuration. If the SDK is to be accessed
       remotely or you intend to run the examples to access a host system, then
       you must use XenCenter to add a network interface to the VM. Once you
       have done this, the VM will attempt to acquire an IP address by DHCP
       each time it boots.

USING THE SDK

     * Start the SDK VM (labelled in the UI as "XenServer SDK #.#.#-####") in
       XenCenter.
     * On first boot the VM will prompt for a password for the "root" account.
       This password allows remote logins via SSH as well as via the Text
       Console in XenCenter.

SDK CONTENT MAP

   The following is a overview of the contents of the /SDK directory tree.
   Where necessary, subdirectories have their own individual README files.
 /SDK/README.txt

   This file

 /SDK/docs/pdf/xenenterpriseapi.pdf

   PDF reference for the API

 /SDK/docs/html/index.html

   html reference for the API

 /SDK/windows-cli

   CLI ('xe.exe') for Windows systems

 /SDK/client-examples/c

   C examples and a Makefile to build them

 /SDK/client-examples/c/test

   Test program

 /SDK/client-examples/csharp/XenSdk.net

   Microsoft Visual Studio 2005 solution which includes the C#
   language bindings (which compile to a .dll) and several example
   projects

 /SDK/client-examples/bash-cli

   Simple bash scripts which use the CLI ('xe')

 /SDK/client-examples/python

   Several example python programs

BUILDING THE EXAMPLES

   The SDK VM comes complete with the tools necessary to build the C examples
   and  also  to  run the bash scripts (which use the CLI) and the python
   programs. By contrast, the C# examples in
   /SDK/client-examples/csharp/XenSdk.net ([2]zip) must first be copied to a
   Windows machine with Visual Studio and .NET 2.0 installed.

QUESTIONS AND FEEDBACK

   Please  voice your questions and feedback about the SDK VM, as well as
   development issues, at [3]http://community.citrix.com/cdn/xs.

References

   1. http://www.microsoft.com/downloads/details.aspx?familyid=0856eacb-4362-4b0d-8edd-aab15c5e04f5&displaylang=en
   2. file://localhost/SDK/XenSdk.net.zip
   3. http://community.citrix.com/cdn/xs
