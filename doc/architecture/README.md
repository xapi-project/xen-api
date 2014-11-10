Xapi architecture
=================

Xapi is responsible for
- providing a stable interface (the XenAPI)
- hosting the "xe" CLI
- allowing one client to manage multiple hosts
- authenticating users and applying role-based access control
- locking resources (in particular disks)
- allowing storage to be managed through plugins
- planning and coping with host failures ("High Availability")
- storing VM and host configuration
- generating alerts
- managing software patching

Note that Xapi delegates core functions such as
- starting VMs to [Xenopsd](https://github.com/xapi-project/xenopsd)
- monitoring performance counters to [Xcp-rrdd](https://github.com/xapi-project/xcp-rrdd)
- setting up host networking to [Xcp-networkd](https://github.com/xapi-project/xcp-networkd)

The following diagram shows the internals of Xapi:

![Internals of xapi](http://xapi-project.github.io/xen-api/doc/architecture/xapi.png)

