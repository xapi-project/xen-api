+++
title = "XAPI Toolstack Developer Guide"
archetype = "home"
+++

The **XAPI Toolstack**:

- Forms the control plane of both [XenServer](http://xenserver.com) as well as
[xcp-ng](http://xcp-ng.org),
- manages clusters of Xen hosts with shared storage and networking,
- has a full-featured [API](http://xapi-project.github.io/xen-api), used by clients such as
[XenCenter](https://github.com/xenserver/xenadmin) and [Xen Orchestra](https://xen-orchestra.com).

The XAPI Toolstack is an open-source project developed by the [xapi
project](http://www.xenproject.org/developers/teams/xapi.html), a sub-project of the Linux
Foundation Xen Project.

The source code is available on [Github under the xapi-project](https://github.com/xapi-project/). the main repository is [xen-api](https://github.com/xapi-project/xen-api).

This developer guide documents the internals of the Toolstack to help developers understand the code, fix bugs and add new features. It is a work-in-progress, with new documents added when ready and updated whenever needed.