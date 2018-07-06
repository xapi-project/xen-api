Walk-through: starting a VM
===========================

A Xenopsd client wishes to start a VM. They must first tell Xenopsd the VM
configuration to use. A VM configuration is broken down into objects:

- VM: A device-less Virtual Machine
- VBD: A virtual block device for a VM
- VIF: A virtual network interface for a VM
- PCI: A virtual PCI device for a VM

Treating devices as first-class objects is convenient because we wish to expose
operations on the devices such as hotplug, unplug, eject (for removable media),
carrier manipulation (for network interfaces) etc.

The "add" functions in the Xenopsd interface cause Xenopsd to create the
objects:

- [VM.add](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L420)
- [VBD.add](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L464)
- [VIF.add](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L475)
- [PCI.add](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L457)

In the case of [xapi](https://github.com/xapi-project/xen-api), there are a set
of functions which
[convert between the XenAPI objects and the Xenopsd objects](https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L380).
The two interfaces are slightly different because they have different expected
users:

- the XenAPI has many clients which are updated on long release cycles. The
  main property needed is backwards compatibility, so that new release of xapi
  remain compatible with these older clients. Quite often we will chose to
  "grandfather in" some poorly designed interface simply because we wish to
  avoid imposing churn on 3rd parties.
- the Xenopsd API clients are all open-source and are part of the xapi-project.
  These clients can be updated as the API is changed. The main property needed
  is to keep the interface clean, so that it properly hides the complexity
  of dealing with Xen from other components.

The Xenopsd "VM.add" function has code like this:
```
	let add' x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.write x.id x;
		let module B = (val get_backend () : S) in
		B.VM.add x;
		x.id
```
This function does 2 things:
- it stores the VM configuration in the "database"
- it tells the "backend" that the VM exists

The Xenopsd database is really a set of config files in the filesystem. All
objects belonging to a VM (recall we only have VMs, VBDs, VIFs, PCIs and not
stand-alone entities like disks) and are placed into a subdirectory named after
the VM e.g.:
```
# ls /run/nonpersistent/xenopsd/xenlight/VM/7b719ce6-0b17-9733-e8ee-dbc1e6e7b701
config	vbd.xvda  vbd.xvdb
# cat /run/nonpersistent/xenopsd/xenlight/VM/7b719ce6-0b17-9733-e8ee-dbc1e6e7b701/config
{"id": "7b719ce6-0b17-9733-e8ee-dbc1e6e7b701", "name": "fedora",
 ...
}
```
Xenopsd doesn't have as persistent a notion of a VM as xapi, it is expected that
all objects are deleted when the host is rebooted. However the objects should
be persisted over a simple Xenopsd restart, which is why the objects are stored
in the filesystem.

Aside: it would probably be more appropriate to store the metadata in Xenstore
since this has the exact object lifetime we need. This will require a more
performant Xenstore to realise.

Every running Xenopsd process is linked with a single backend. Currently backends
exist for:
- Xen via libxc, libxenguest and xenstore
- Xen via libxl, libxc and xenstore
- Xen via libvirt
- KVM by direct invocation of qemu
- Simulation for testing

From here we shall assume the use of the "Xen via libxc, libxenguest and xenstore" (a.k.a.
"Xenopsd classic") backend.

The backend [VM.add](https://github.com/xapi-project/xenopsd/blob/2a476c132c0b5732f9b224316b851a1b4d57520b/xc/xenops_server_xen.ml#L719)
function checks whether the VM we have to manage already exists -- and if it does
then it ensures the Xenstore configuration is intact. This Xenstore configuration
is important because at any time a client can query the state of a VM with
[VM.stat](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L438)
and this relies on certain Xenstore keys being present.

Once the VM metadata has been registered with Xenopsd, the client can call
[VM.start](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L443).
Like all potentially-blocking Xenopsd APIs, this function returns a Task id.
Please refer to the [Task handling design](../design/Tasks.md) for a general
overview of how tasks are handled.

Clients can poll the state of a task by calling [TASK.stat](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L404)
but most clients will prefer to use the event system instead.
Please refer to the [Event handling design](../design/Events.md) for a general
overview of how events are handled.

The event model is similar to the XenAPI: clients call a blocking
[UPDATES.get](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L487)
passing in a token which represents the point in time when the last UPDATES.get
returned. The call blocks until some objects have changed state, and these object
ids are returned (NB in the XenAPI the current object states are returned)
The client must then call the relevant "stat" function, in this
case [TASK.stat](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L404)

The client will be able to see the task make progress and use this to -- for example --
populate a progress bar in a UI. If the client needs to cancel the task then it
can call the [TASK.cancel](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L405);
again see the [Task handling design](../design/Tasks.md) to understand how this is
implemented.

When the Task has completed successfully, then calls to *.stat will show:
- the power state is Paused
- exactly one valid Xen domain id
- all VBDs have active = plugged = true
- all VIFs have active = plugged = true
- all PCI devices have plugged = true
- at least one active console
- a valid start time
- valid "targets" for memory and vCPU

Note: before a Task completes, calls to *.stat will show partial updates e.g.
the power state may be Paused but none of the disks may have become plugged.
UI clients must choose whether they are happy displaying this in-between state
or whether they wish to hide it and pretend the whole operation has happened
transactionally. If a particular client wishes to perform side-effects in
response to Xenopsd state changes -- for example to clean up an external resource
when a VIF becomes unplugged -- then it must be very careful to avoid responding
to these in-between states. Generally it is safest to passively report these
values without driving things directly from them. Think of them as status lights
on the front panel of a PC: fine to look at but it's not a good idea to wire
them up to actuators which actually do things.

Note: the Xenopsd implementation guarantees that, if it is restarted at any point
during the start operation, on restart the VM state shall be "fixed" by either
(i) shutting down the VM; or (ii) ensuring the VM is intact and running.

In the case of [xapi](https://github.com/xapi-project/xen-api) every Xenopsd
Task id bound one-to-one with a XenAPI task by the function
[sync_with_task](https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L1831).
The function [update_task](https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L1450)
is called when xapi receives a notification that a Xenopsd Task has changed state,
and updates the corresponding XenAPI task.
Xapi launches exactly one thread per Xenopsd instance ("queue") to monitor for
background events via the function
[events_watch](https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L1467)
while each thread performing a XenAPI call waits for its specific Task to complete
via the function
[event_wait](https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L30).

It is the responsibility of the client to call
[TASK.destroy](https://github.com/xapi-project/xcp-idl/blob/2e5c3dd79c63e3711227892271a6bece98eb0fa1/xen/xenops_interface.ml#L406)
when the Task is nolonger needed. Xenopsd won't destroy the task because it contains
the success/failure result of the operation which is needed by the client.

What happens when a Xenopsd receives a VM.start request?

When Xenopsd receives the request it adds it to the appropriate per-VM queue
via the function
[queue_operation](https://github.com/xapi-project/xenopsd/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/lib/xenops_server.ml#L1737).
To understand this and other internal details of Xenopsd, consult the
[architecture description](../architecture/README.md).
The [queue_operation_int](https://github.com/xapi-project/xenopsd/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/lib/xenops_server.ml#L1451)
function looks like this:
```
let queue_operation_int dbg id op =
	let task = Xenops_task.add tasks dbg (fun t -> perform op t; None) in
	Redirector.push id (op, task);
	task
```
The "task" is a record containing Task metadata plus a "do it now" function
which will be executed by a thread from the thread pool.  The
[module Redirector](https://github.com/xapi-project/xenopsd/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/lib/xenops_server.ml#L395)
takes care of:
- pushing operations to the right queue
- ensuring at most one worker thread is working on a VM's operations
- reducing the queue size by coalescing items together
- providing a diagnostics interface

Once a thread from the worker pool becomes free, it will execute the "do it now"
function. In the example above this is ```perform op t``` where ```op``` is
```VM_start vm``` and ```t``` is the Task. The function
[perform](https://github.com/xapi-project/xenopsd/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/lib/xenops_server.ml#L1194)
has fragments like this:
```
		| VM_start id ->
			debug "VM.start %s" id;
			perform_atomics (atomics_of_operation op) t;
			VM_DB.signal id
```

Each "operation" (e.g. ```VM_start vm```) is decomposed into "micro-ops" by the
function
[atomics_of_operation](https://github.com/xapi-project/xenopsd/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/lib/xenops_server.ml#L736)
where the micro-ops are small building-block actions common to the higher-level
operations. Each operation corresponds to a list of "micro-ops", where there is
no if/then/else. Some of the "micro-ops" may be a no-op depending on the VM
configuration (for example a PV domain may not need a qemu). In the case of
```VM_start vm``` this decomposes into the sequence:

1. run the "VM_pre_start" scripts
---------------------------------

The ```VM_hook_script``` micro-op runs the corresponding "hook" scripts. The
code is all in the
[Xenops_hooks](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/lib/xenops_hooks.ml)
module and looks for scripts in the hardcoded path ```/etc/xapi.d```.

2. create a Xen domain
----------------------

The ```VM_create``` micro-op calls the ```VM.create``` function in the backend.
In the classic Xenopsd backend the
[VM.create_exn](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L633)
function must

1. check if we're creating a domain for a fresh VM or resuming an existing one:
   if it's a resume then the domain configuration stored in the VmExtra database
   table must be used
2. ask *squeezed* to create a memory "reservation" big enough to hold the VM
   memory. Unfortunately the domain cannot be created until the memory is free
   because domain create often fails in low-memory conditions. This means the
   "reservation" is associated with our "session" with squeezed; if Xenopsd
   crashes and restarts the reservation will be freed automatically.
3. create the Domain via the libxc hypercall
4. "transfer" the squeezed reservation to the domain such that squeezed will
   free the memory if the domain is destroyed later
5. compute and set an initial balloon target depending on the amount of memory
   reserved (recall we ask for a range between *dynamic_min* and *dynamic_max*)
6. apply the "suppress spurious page faults" workaround if requested
7. set the "machine address size"
8. "hotplug" the vCPUs. This operates a lot like memory ballooning -- Xen creates
   lots of vCPUs and then the guest is asked to only use some of them. Every VM
   therefore starts with the "VCPUs_max" setting and co-operative hotplug is
   used to reduce the number. Note there is no enforcement mechanism: a VM which
   cheats and uses too many vCPUs would have to be caught by looking at the
   performance statistics.

3. build the domain
-------------------

On a Xen system a domain is created empty, and memory is actually allocated
from the host in the "build" phase via functions in *libxenguest*. The
[VM.build_domain_exn](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L994)
function must

1. run pygrub (or eliloader) to extract the kernel and initrd, if necessary
2. invoke the *xenguest* binary to interact with libxenguest.
3. apply the ```cpuid``` configuration
4. store the current domain configuration on disk -- it's important to know
   the difference between the configuration you started with and the configuration
   you would use after a reboot because some properties (such as maximum memory
   and vCPUs) as fixed on create.

The xenguest binary was originally
a separate binary for two reasons: (i) the libxenguest functions weren't
threadsafe since they used lots of global variables; and (ii) the libxenguest
functions used to have a different, incompatible license, which prevent us
linking. Both these problems have been resolved but we still shell out to
the xenguest binary.

The xenguest binary has also evolved to configure more of the initial domain
state. It also [reads Xenstore](https://github.com/xapi-project/ocaml-xen-lowlevel-libs/blob/master/xenguest-4.4/xenguest_stubs.c#L42)
and configures

- the vCPU affinity
- the vCPU credit2 weight/cap parameters
- whether the NX bit is exposed
- whether the viridian CPUID leaf is exposed
- whether the system has PAE or not
- whether the system has ACPI or not
- whether the system has nested HVM or not
- whether the system has an HPET or not

4. mark each VBD as "active"
----------------------------

VBDs and VIFs are said to be "active" when they are intended to be used by a
particular VM, even if the backend/frontend connection hasn't been established,
or has been closed. If someone calls ```VBD.stat``` or ```VIF.stat``` then
the result includes both "active" and "plugged", where "plugged" is true if
the frontend/backend connection is established.
For example xapi will
set [VBD.currently_attached](https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L1300)
to "active || plugged". The "active" flag is conceptually very similar to the
traditional "online" flag (which is not documented in the upstream Xen tree
as of Oct/2014 but really should be) except that on unplug, one would set
the "online" key to "0" (false) *first* before initiating the hotunplug. By
contrast the "active" flag is set to false *after* the unplug i.e. "set_active"
calls bracket plug/unplug. If the "active" flag was set before the unplug
attempt then as soon as the frontend/backend connection is removed clients
would see the VBD as completely dissociated from the VM -- this would be misleading
because Xenopsd will not have had time to use the storage API to release locks
on the disks. By doing all the cleanup before setting "active" to false, clients
can be assured that the disks are now free to be reassigned.

5. handle non-persistent disks
------------------------------

A non-persistent disk is one which is reset to a known-good state on every
VM start. The ```VBD_epoch_begin``` is the signal to perform any necessary reset.

6. plug VBDs
------------

The ```VBD_plug``` micro-op will plug the VBD into the VM. Every VBD is plugged
in a carefully-chosen order.
Generally, plug order is important for all types of devices. For VBDs, we must
work around the deficiency in the storage interface where a VDI, once attached
read/only, cannot be attached read/write. Since it is legal to attach the same
VDI with multiple VBDs, we must plug them in such that the read/write VBDs
come first. From the guest's point of view the order we plug them doesn't
matter because they are indexed by the Xenstore device id (e.g. 51712 = xvda).

The function
[VBD.plug](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L1631)
will

- call ```VDI.attach``` and ```VDI.activate``` in the storage API to make the
  devices ready (start the tapdisk processes etc)
- add the Xenstore frontend/backend directories containing the block device
  info
- add the extra xenstore keys returned by the ```VDI.attach``` call that are
  needed for SCSIid passthrough which is needed to support VSS
- write the VBD information to the Xenopsd database so that future calls to
  *VBD.stat* can be told about the associated disk (this is needed so clients
  like xapi can cope with CD insert/eject etc)
- if the qemu is going to be in a different domain to the storage, a frontend
  device in the qemu domain is created.

The Xenstore keys are written by the functions
[Device.Vbd.add_async](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/device.ml#L486)
and
[Device.Vbd.add_wait](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/device.ml#L550).
In a Linux domain (such as dom0) when the backend directory is created, the kernel
creates a "backend device". Creating any device will cause a kernel UEVENT to fire
which is picked up by udev. The udev rules run a script whose only job is to
stat(2) the device (from the "params" key in the backend) and write the major
and minor number to Xenstore for blkback to pick up. (Aside: FreeBSD doesn't do
any of this, instead the FreeBSD kernel module simply opens the device in the
"params" key). The script also writes the backend key "hotplug-status=connected".
We currently wait for this key to be written so that later calls to *VBD.stat*
will return with "plugged=true". If the call returns before this key is written
then sometimes we receive an event, call *VBD.stat* and conclude erroneously
that a spontaneous VBD unplug occurred.

7. mark each VIF as "active"
----------------------------

This is for the same reason as VBDs are marked "active".

8. plug VIFs
------------

Again, the order matters. Unlike VBDs,
there is no read/write read/only constraint and the devices
have unique indices (0, 1, 2, ...) *but* Linux kernels have often (always?)
ignored the actual index and instead relied on the order of results from the
```xenstore-ls``` listing. The order that xenstored returns the items happens
to be the order the nodes were created so this means that (i) xenstored must
continue to store directories as ordered lists rather than maps (which would
be more efficient); and (ii) Xenopsd must make sure to plug the vifs in
the same order. Note that relying on ethX device numbering has always been a
bad idea but is still common. I bet if you change this lots of tests will
suddenly start to fail!

The function
[VIF.plug_exn](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L1945)
will

- compute the port locking configuration required and write this to a well-known
  location in the filesystem where it can be read from the udev scripts. This
  really should be written to Xenstore instead, since this scheme doesn't work
  with driver domains.
- add the Xenstore frontend/backend directories containing the network device
  info
- write the VIF information to the Xenopsd database so that future calls to
  *VIF.stat* can be told about the associated network
- if the qemu is going to be in a different domain to the storage, a frontend
  device in the qemu domain is created.

Similarly to the VBD case, the function
[Device.Vif.add](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/device.ml#L642)
will write the Xenstore keys and wait for the "hotplug-status=connected" key.
We do this because we cannot apply the port locking rules until the backend
device has been created, and we cannot know the rules have been applied
until after the udev script has written the key. If we didn't wait for it then
the VM might execute without all the port locking properly configured.

9. create the device model
--------------------------

The ```VM_create_device_model``` micro-op will create a qemu device model if
- the VM is HVM; or
- the VM uses a PV keyboard or mouse (since only qemu currently has backend
  support for these devices).

The function
[VM.create_device_model_exn](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L1090)
will
- (if using a qemu stubdom) it will create and build the qemu domain
- compute the necessary qemu arguments and launch it.

Note that qemu (aka the "device model") is created after the VIFs and VBDs have
been plugged but before the PCI devices have been plugged. Unfortunately qemu
traditional infers the needed emulated hardware by inspecting the Xenstore
VBD and VIF configuration and assuming that we want one emulated device per
PV device, up to the natural limits of the emulated buses (i.e. there can be
at most 4 IDE devices: {primary,secondary}{master,slave}). Not only does this
create an ordering dependency that needn't exist -- and which impacts migration
downtime -- but it also completely ignores the plain fact that, on a Xen system,
qemu can be in a different domain than the backend disk and network devices.
This hack only works because we currently run everything in the same domain.
There is an option (off by default) to list the emulated devices explicitly
on the qemu command-line. If we switch to this by default then we ought to be
able to start up qemu early, as soon as the domain has been created (qemu will
need ot know the domain id so it can map the I/O request ring).

10. plug PCI devices
--------------------

PCI devices are treated differently to VBDs and VIFs.
If we are attaching the device to an
HVM guest then instead of relying on the traditional Xenstore frontend/backend
state machine we instead send RPCs to qemu requesting they be hotplugged. Note
the domain is paused at this point, but qemu still supports PCI hotplug/unplug.
The reasons why this doesn't follow the standard Xenstore model are known only
to the people who contributed this support to qemu.
Again the order matters because it determines the position of the virtual device
in the VM.

Note that Xenopsd doesn't know anything about the PCI devices; concepts such
as "GPU groups" belong to higher layers, such as xapi.

11. mark the domain as alive
----------------------------

A design principle of Xenopsd is that it should tolerate failures such as being
suddenly restarted. It guarantees to always leave the system in a valid state,
in particular there should never be any "half-created VMs". We achieve this for
VM start by exploiting the mechanism which is necessary for reboot. When a VM
wishes to reboot it causes the domain to exist (via SCHEDOP_shutdown) with a
"reason code" of "reboot". When Xenopsd sees this event ```VM_check_state```
operation is queued. This operation calls
[VM.get_domain_action_request](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L1443)
to ask the question, "what needs to be done to make this VM happy now?". The
implementation checks the domain state for shutdown codes and also checks a
special Xenopsd Xenstore key. When Xenopsd creates a Xen domain it sets this
key to "reboot" (meaning "please reboot me if you see me") and when Xenopsd
finishes starting the VM it clears this key. This means that if Xenopsd crashes
while starting a VM, the new Xenopsd will conclude that the VM needs to be rebooted
and will clean up the current domain and create a fresh one.

12. unpause the domain
----------------------

A Xenopsd VM.start will always leave the domain paused, so strictly speaking
this is a separate "operation" queued by the client (such as xapi) after the
VM.start has completed. The function
[VM.unpause](https://github.com/xapi-project/xenopsd/blob/b33bab13080cea91e2fd59d5088622cd68152339/xc/xenops_server_xen.ml#L808)
is reassuringly simple:
```
		if di.Xenctrl.total_memory_pages = 0n then raise (Domain_not_built);
		Domain.unpause ~xc di.Xenctrl.domid;
		Opt.iter
			(fun stubdom_domid ->
				Domain.unpause ~xc stubdom_domid
			) (get_stubdom ~xs di.Xenctrl.domid)
```
