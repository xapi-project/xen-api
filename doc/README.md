Xapi: the developer handbook
============================

Xapi is the [xapi-project](http://github.com/xapi-project) host and cluster manager.
Xapi is responsible for
- aggregating hosts into a single entity (a "resource pool")
- allocating VMs to hosts
- managing shared storage
- recoverying from host failure ("High-Availability")
- preparing for site failure ("Disaster Recovery")
- providing the XenAPI XMLRPC interface
- upgrading pools
- hotfixing hosts
- reporting bugs
etc

Principles
----------

1. The XenAPI interface must remain backwards compatible, allowing older
   clients to continue working
2. Xapi delegates all Xenstore/libxc/libxl access to Xenopsd, so Xapi could
   be run in an unprivileged helper domain
3. Xapi delegates the low-level storage manipulation to SM plugins.

Contents
--------

- [Architecture](architecture/README.md): read about how Xapi fits into
  the overall system; and the major pieces and patterns within Xapi.
- [Features](features/README.md): learn about the features supported by Xapi and
  how they work.
- [Design](design/README.md): discover the low-level details, formats, protocols,
  concurrency etc.
- [Walk-throughs](walk-throughs/README.md): follow operations end-to-end to
  understand how it all fits together.
- [Futures](futures/README.md): find out how Xapi is likely to change and
  how you can help.
