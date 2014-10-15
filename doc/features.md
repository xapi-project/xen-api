General
-------

- Pluggable backends including
  - xc: drives Xen via libxc and libxenguest
  - xenlight: drives Xen via libxenlight and libxc
  - libvirt: drives Xen via libvirt
  - qemu: drives KVM by running qemu processes
  - simulator: simulates operations for component-testing
- Supports running multiple instances and backends on the same host, looking
  after different sets of VMs
- Distribution agnostic, known to work on
  - XenServer
  - CentOS 6.*
  - Ubuntu 14.04
  - Fedora 21
- Extensive configuration via command-line (see manpage) and config
  file
- Command-line tool for easy VM administration and troubleshooting
- User-settable degree of concurrency to get VMs started quickly

VMs
---
- VM start/shutdown/reboot
- VM suspend/resume/checkpoint/migrate
- VM pause/unpause
- VM s3suspend/s3resume 
- customisable SMBIOS tables for OEM-locked VMs
- hooks for 3rd party extensions:
  - pre-start
  - pre-destroy
  - post-destroy
  - pre-reboot
- per-VM xenguest replacement
- suppression of VM reboot loops
- live vCPU hotplug and unplug
- vCPU to pCPU affinity setting
- vCPU QoS settings (weight and cap for the Xen credit2 scheduler)
- DMC memory-ballooning support
- support for storage driver domains
- live update of VM shadow memory
- guest-initiated disk/nic hotunplug
- guest-initiated disk eject
- force disk/nic unplug
- support for 'surprise-removable' devices
- disk QoS configuration
- nic QoS configuration
- persistent RTC
- two-way guest agent communication for monitoring and control
- network carrier configuration
- port-locking for nics
- text and VNC consoles over TCP and Unix domain sockets
- PV kernel and ramdisk whitelisting
- configurable VM videoram
- programmable action-after-crash behaviour including: shutting down
  the VM, taking a crash dump or leaving the domain paused for inspection
- ability to move nics between bridges/switches
- advertises the VM memory footprints
- PCI passthrough
- support for discrete emulators (e.g. 'demu')
- PV keyboard and mouse
- qemu stub domains
- cirrus and stdvga graphics cards
- HVM serial console (useful for debugging)
- support for vGPU
- workaround for 'spurious page faults' kernel bug
- workaround for 'machine address size' kernel bug

Hosts
-----
- CPUid masking for heterogenous pools: reports true features and current
  features
- Host console reading
- Hypervisor version and capabilities reporting
- Host CPU querying

APIs
----
- versioned json-rpc API with feature advertisements
- clients can disconnect, reconnect and easily resync with the latest
  VM state without losing updates
- all operations have task control including
  - asychronous cancellation: for both subprocesses and xenstore watches
  - progress updates
  - subtasks
  - per-task debug logs
- asynchronous event watching API
- advertises VM metrics
  - memory usage
  - balloon driver co-operativeness
  - shadow memory usage
  - domain ids
- channel passing (via sendmsg(2)) for efficent memory image copying
