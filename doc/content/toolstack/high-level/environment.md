+++
title = "Environment"
weight = 10
+++

The Toolstack runs in an environment on a server (host) that has:

- Physical hardware.
- The Xen hypervisor.
- The control domain (domain 0): the privileged domain that the Toolstack runs in.
- Other, mostly unprivileged domains, usually for guests (VMs).

The Toolstack relies on various bits of software inside the control domain, and directly communicates with most of these:

- Linux kernel including drivers for hardware and Xen paravirtualised devices (e.g. `netback` and `blkback`).
  - Interacts through `/sys` and `/proc`, udev scripts, xenstore, ...
- CentOS distribution including userspace tools and libraries.
  - systemd, networking tools, ...
- Xen-specific libraries, especially `libxenctrl` (a.k.a. `libxc`)
- `xenstored`: a key-value pair configuration database
  - Accessible from all domains on a host, which makes it useful for inter-domain communication.
  - The control domain has access to the entire xenstore database, while other domains only see sub-trees that are specific to that domain.
  - Used for connecting VM disks and network interfaces, and other VM configuration options.
  - Used for VM status reporting, e.g. the capabilities of the PV drivers (if installed), the IP address, etc.
- [SM](https://github.com/xapi-project/sm): Storage Manager
  plugins which connect xapi's internal storage interfaces to the control
  APIs of external storage systems.
- `stunnel`: a daemon which decodes TLS and forwards traffic to xapi (and the other way around).
- Open vSwitch (OVS): a virtual network switch, used to connect VMs to network interfaces. The OVS offers several networking features that xapi takes advantage of.
- QEMU: emulation of various bits of hardware
- DEMU: emulation of Nvidia vGPUs
- `xenguest`
- `emu-manager`
- `pvsproxy`
- `xenconsoled`: allows access to guest consoles. This is common to all Xen
  hosts.

The Toolstack also interacts with software that runs inside the guests:

- PV drivers
- The guest agent
