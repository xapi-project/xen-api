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
[queue_operation](https://github.com/xapi-project/xenopsd/blob/master/lib/xenops_server.ml#L1737).
To understand this and other internal details of Xenopsd, consult the
[architecture description](../architecture/README.md).
The [queue_operation_int](https://github.com/xapi-project/xenopsd/blob/master/lib/xenops_server.ml#L1451)
function looks like this:
```
let queue_operation_int dbg id op =
	let task = Xenops_task.add tasks dbg (fun t -> perform op t; None) in
	Redirector.push id (op, task);
	task
```
The `"task" is a record containing Task metadata plus a "do it now" function
which will be executed by a thread from the thread pool. The function "Redirector.push"
takes care of pushing the operation to the right queue. The
[module Redirector](https://github.com/xapi-project/xenopsd/blob/master/lib/xenops_server.ml#L395)
takes care of:
- pushing operations to the right queue
- ensuring at most one worker thread is working on a VM's operations
- reducing the queue size by coalescing items together
- providing a diagnostics interface
