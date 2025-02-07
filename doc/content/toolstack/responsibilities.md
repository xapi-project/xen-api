+++
title = "Responsibilities"
weight = 10
+++

The XAPI Toolstack forms the main control plane of a pool of XenServer hosts. It allows the administrator to:

- Configure the hardware resources of XenServer hosts: storage, networking, graphics, memory.
- Create, configure and destroy VMs and their virtual resources.
- Control the lifecycle of VMs.
- Monitor the status of hosts, VMs and related resources.

To this, the Toolstack:

- Exposes an API that can be accessed by external clients over HTTP(s).
- Exposes a CLI.
- Ensures that physical resources are configured when needed, and VMs receive the resources they require.
- Implements various features to help the administrator manage their systems.
- Monitors running VMs.
- Records metrics about physical and virtual resources.