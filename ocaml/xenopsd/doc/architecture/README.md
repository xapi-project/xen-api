Architecture
============

Xenopsd instances run on a host and manage VMs on behalf of clients. This
picture shows 3 different Xenopsd instances: 2 named "xenopsd-xc" and 1 named
"xenopsd-xenlight".

![Where xenopsd fits on a host](http://djs55.github.io/xenopsd/doc/architecture/host.svg)

Each instance is responsible for managing a disjoint set of VMs. Clients should
never ask more than one Xenopsd to manage the same VM.
Managing a VM means:
- handling start/shutdown/suspend/resume/migrate/reboot
- allowing devices (disks, nics, PCI cards, vCPUs etc) to be manipulated
- providing updates to clients when things change (reboots, console becomes
  available, guest agent says something etc).
For a full list of features, consult the [features list](../features/README.md).

Each Xenopsd instance has a unique name on the host. A typical name is
- org.xen.xcp.xenops.classic
- org.xen.xcp.xenops.xenlight

A higher-level tool, such as [xapi](https://github.com/xapi-project/xen-api)
will associate VMs with individual Xenopsd names.

Running multiple Xenopsds is necessary because
- The virtual hardware supported by different technologies (libxc, libxl, qemu)
  is expected to be different. We can guarantee the virtual hardware is stable
  across a rolling upgrade by running the VM on the old Xenopsd. We can then switch
  Xenopsds later over a VM reboot when the VM admin is happy with it. If the
  VM admin is unhappy then we can reboot back to the original Xenopsd again.
- The suspend/resume/migrate image formats will differ across technologies
  (again libxc vs libxl) and it will be more reliable to avoid switching
  technology over a migrate.
- In the future different security domains may have different Xenopsd instances
  providing even stronger isolation guarantees between domains than is possible
  today.

Communication with Xenopsd is handled through a Xapi-global library:
[xcp-idl](https://github.com/xapi-project/xcp-idl). This library supports
- message framing: by default using HTTP but a binary framing format is
  available
- message encoding: by default we use JSON but XML is also available
- RPCs over Unix domain sockets and persistent queues.

This library allows the communication details to be changed without having to
change all the Xapi clients and servers.

Xenopsd has a number of "backends" which perform the low-level VM operations
such as (on Xen) "create domain" "hotplug disk" "destroy domain". These backends
contain all the hypervisor-specific code including
- connecting to Xenstore
- opening the libxc /proc/xen/privcmd interface
- initialising libxl contexts

The following diagram shows the internal structure of Xenopsd:

![Inside xenopsd](http://djs55.github.io/xenopsd/doc/architecture/xenopsd.svg)

At the top of the diagram two client RPC have been sent: one to start a VM
and the other to fetch the latest events. The RPCs are all defined in
[xcp-idl/xen/xenops_interface.ml](https://github.com/xapi-project/xcp-idl/blob/master/xen/xenops_interface.ml).
The RPCs are received by the Xenops_server module and decomposed into
"micro-ops" (labelled "μ op"). These micro ops represent actions like

- create a Xen domain (recall a Xen domain is an empty shell with no memory)
- build a Xen domain: this is where the kernel or hvmloader is copied in
- launch a device model: this is where a qemu instance is started (if one is
  required)
- hoplug a device: this involves writing the frontend and backend trees to
  Xenstore
- unpause a domain (recall a Xen domain is created in the paused state)

Each of these micro-ops is represented by a function call in a "backend plugin"
interface. The micro-ops are enqueued in queues, one queue per VM. There is a
thread pool (whose size can be changed dynamically by the admin) which pulls
micro-ops from the VM queues and calls the corresponding backend function.

The active backend (there can only be one backend per Xenopsd instance)
executes the micro-ops. The Xenops_server_xen backend in the picture above
talks to libxc, libxl and qemu to create and destroy domains. The backend
also talks to other Xapi services, in particular

- it registers datasources with xcp-rrdd, telling xcp-rrdd to measure I/O
  throughput and vCPU utilisation
- it reserves memory for new domains by talking to squeezed
- it makes disks available by calling SMAPIv2 VDI.{at,de}tach, VDI.{,de}activate
- it launches subprocesses by talking to forkexecd (avoiding problems with
  accidental fd capture)

Xenopsd backends are also responsible for monitoring running VMs. In the
Xenops_server_xen backend this is done by watching Xenstore for

- @releaseDomain watch events
- device hotplug status changes

When such an event happens (for example: @releaseDomain sent when a domain
requests a reboot) the corresponding operation does not happen inline. Instead
the event is rebroadcast upwards to Xenops_server as a signal ("for example:
VM <id> needs some attention") and a "VM_stat" micro-op is queued in the
appropriate queue. Xenopsd does not allow operations to run on the same VM
in parallel and enforces this by:

- pushing all operations pertaining to a VM to the same queue
- associating each VM queue to at-most-one worker pool thread

The event takes the form "VM <id> needs some attention" and not "VM <id> needs
to be rebooted" because, by the time the queue is flushed, the VM may well now
be in a different state. Perhaps rather than being rebooted it now needs to
be shutdown; or perhaps the domain is now in a good state because the reboot
has already happened. The signals sent by the backend to the Xenops_server are
a bit like event channel notifications in the Xen ring protocols: they are
requests to ask someone to perform work, the don't themselves describe the work
that needs to be done.

An implication of this design is that it should always be possible to answer
the question, "what operation should be performed to get the VM into a valid state?".
If an operation is cancelled half-way through or if Xenopsd is suddenly restarted,
it will ask the question about all the VMs and perform the necessary operations.
The operations must be designed carefully to make this work. For example if Xenopsd
is restarted half-way through starting a VM, it must be obvious on restart that
the VM should either be forcibly shutdown or rebooted to make it a valid state
again. Note: we don't demand that operations are performed as transactions;
we only demand that the state they leave the system be "sensible" in the sense
that the admin will recognise it and be able to continue their work.

Sometimes this can be achieved through careful ordering of side-effects
within the operations, taking advantage of artifacts of the system such as:

- a domain which has not been fully created will have total vCPU time = 0 and
  will be paused. If we see one of these we should reboot it because it may
  not be fully intact.

In the absense of "tells" from the system, operations are expected to journal
their intentions and support restart after failure.

There are three categories of metadata associated with VMs:

1. system metadata: this is created as a side-effect of starting VMs. This
   includes all the information about active disks and nics stored in Xenstore
   and the list of running domains according to Xen.
2. VM: this is the configuration to use when the VM is started or rebooted.
   This is like a "config file" for the VM.
3. VmExtra: this is the runtime configuration of the VM. When VM configuration
   is changed it often cannot be applied immediately; instead the VM continues
   to run with the previous configuration. We need to track the runtime
   configuration of the VM in order for suspend/resume and migrate to work. It
   is also useful to be able to tell a client, "on next reboot this value will
   be <x> but currently it is <x-1>".

VM and VmExtra metadata is stored by Xenopsd in the domain 0 filesystem, in
a simple directory hierarchy.
