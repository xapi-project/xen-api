XenServerJava Samples
=====================

This library consists of a number of test programs that can be used as pedagogical
examples accompanying XenServerJava (com.citrix.hypervisor.xen-api).

XenServerJava is a complete SDK for Citrix Hypervisor, exposing the Citrix
Hypervisor API as Java classes.

XenServerJava includes a class for every API class, and a method for each API
call, so API documentation and examples written for other languages will apply
equally well to Java. In particular, the SDK Guide and the Management API Guide
are ideal for developers wishing to use XenServerJava.

XenServerJava Samples and XenServerJava are free software. You can redistribute
and modify them under the terms of the BSD 2-Clause license. See LICENSE for
details.


Reference
---------

For Citrix Hypervisor documentation see https://docs.citrix.com/en-us/citrix-hypervisor/

The Citrix Hypervisor Management API Reference is available at
https://developer-docs.citrix.com/projects/citrix-hypervisor-management-api/en/latest/

The Citrix Hypervisor Software Development Kit Guide is available at
https://developer-docs.citrix.com/projects/citrix-hypervisor-sdk/en/latest/

For community content, blogs, and downloads, visit
https://www.citrix.com/community/citrix-developer/

To network with other developers using Citrix Hypervisor visit
https://discussions.citrix.com/forum/101-hypervisor-formerly-xenserver/


Dependencies
------------

XenServerJava Samples depend on XenServerJava, which in turns depends upon Apache
XML-RPC by the Apache Software Foundation, licensed under the Apache Software
License 2.0.


To run the tests
----------------

Once you compile the library run:

RunTests <host> <username> <password> [nfs server] [nfs path]
