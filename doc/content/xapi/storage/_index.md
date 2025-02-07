+++
title = "XAPI's Storage Layers"
linkTitle = "Storage"
+++

{{% notice info %}}
The links in this page point to the source files of xapi
[v1.127.0](https://github.com/xapi-project/xen-api/tree/v1.127.0), and xcp-idl
[v1.62.0](https://github.com/xapi-project/xcp-idl/tree/v1.62.0), not to the
latest source code.

In the beginning of 2023, significant changes have been made in the layering.
In particular, the wrapper code from `storage_impl.ml` has been pushed down the
stack, below the mux, such that it only covers the SMAPIv1 backend and not
SMAPIv3. Also, all of the code (from xcp-idl etc) is now present in this repo
(xen-api).
{{% /notice %}}

Xapi directly communicates only with the SMAPIv2 layer. There are no
plugins directly implementing the SMAPIv2 interface, but the plugins in
other layers are accessed through it:

{{<mermaid>}}
graph TD
A[xapi] --> B[SMAPIv2 interface]
B --> C[SMAPIv2 <-> SMAPIv1 translation: storage_access.ml]
B --> D[SMAPIv2 <-> SMAPIv3 translation: xapi-storage-script]
C --> E[SMAPIv1 plugins]
D --> F[SMAPIv3 plugins]
{{< /mermaid >}}

## SMAPIv1

These are the files related to SMAPIv1 in `xen-api/ocaml/xapi/`:

-   [sm.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/sm.ml):
    OCaml "bindings" for the SMAPIv1 Python "drivers" (SM)
-   [sm_exec.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/sm_exec.ml):
    support for implementing the above "bindings". The
    parameters are converted to XML-RPC, passed to the relevant python
    script ("driver"), and then the standard output of the program is
    parsed as an XML-RPC response (we use
    `xen-api-libs-transitional/http-svr/xMLRPC.ml` for parsing XML-RPC).
    When adding new functionality, we can modify `type call` to add parameters,
    but when we don't add any common ones, we should just pass the new
    parameters in the args record.
-   `smint.ml`: Contains types, exceptions, ... for the SMAPIv1 OCaml
    interface

## SMAPIv2

These are the files related to SMAPIv2, which need to be modified to
implement new calls:

-   [xcp-idl/storage/storage\_interface.ml](https://github.com/xapi-project/xcp-idl/blob/v1.62.0/storage/storage_interface.ml):
    Contains the SMAPIv2 interface
-   [xcp-idl/storage/storage\_skeleton.ml](https://github.com/xapi-project/xcp-idl/blob/v1.62.0/storage/storage_skeleton.ml):
    A stub SMAPIv2 storage server implementation that matches the
    SMAPIv2 storage server interface (this is verified by
    [storage\_skeleton\_test.ml](https://github.com/xapi-project/xcp-idl/blob/v1.62.0/storage/storage_skeleton_test.ml)),
    each of its function just raise a `Storage_interface.Unimplemented`
    error. This skeleton is used to automatically fill the unimplemented
    methods of the below storage servers to satisfy the interface.
-   [xen-api/ocaml/xapi/storage\_access.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml):
    [module SMAPIv1](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml#L104):
    a SMAPIv2 server that does SMAPIv2 -&gt; SMAPIv1 translation.
    It passes the XML-RPC requests as the first command-line argument to the
    corresponding Python script, which returns an XML-RPC response on standard
    output.
-   [xen-api/ocaml/xapi/storage\_impl.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_impl.ml):
    The
    [Wrapper](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_impl.ml#L302)
    module wraps a SMAPIv2 server (Server\_impl) and takes care of
    locking and datapaths (in case of multiple connections (=datapaths)
    from VMs to the same VDI, it will use the superstate computed by the
    [Vdi_automaton](https://github.com/xapi-project/xcp-idl/blob/v1.62.0/storage/vdi_automaton.ml)
    in xcp-idl). It also implements some functionality, like the `DP`
    module, that is not implemented in lower layers.
-   [xen-api/ocaml/xapi/storage\_mux.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_mux.ml):
    A SMAPIv2 server, which multiplexes between other servers. A
    different SMAPIv2 server can be registered for each SR. Then it
    forwards the calls for each SR to the "storage plugin" registered
    for that SR.

### How SMAPIv2 works:

We use [message-switch] under the hood for RPC communication between
[xcp-idl](https://github.com/xapi-project/xcp-idl) components. The
main `Storage_mux.Server` (basically `Storage_impl.Wrapper(Mux)`) is
[registered to
listen](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml#L1279)
on the "`org.xen.xapi.storage`" queue [during xapi's
startup](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/xapi.ml#L801),
and this is the main entry point for incoming SMAPIv2 function calls.
`Storage_mux` does not really multiplex between different plugins right
now: [earlier during xapi's
startup](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/xapi.ml#L799),
the same SMAPIv1 storage server module [is
registered](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml#L934)
on the various "`org.xen.xapi.storage.<sr type>`" queues for each
supported SR type. (This will change with SMAPIv3, which is accessed via
a SMAPIv2 plugin outside of xapi that translates between SMAPIv2 and
SMAPIv3.) Then, in
[Storage\_access.create\_sr](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml#L1531),
which is called
[during SR.create](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/xapi_sr.ml#L326),
and also
[during PBD.plug](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/xapi_pbd.ml#L121),
the relevant "`org.xen.xapi.storage.<sr type>`" queue needed for that
PBD is [registered with Storage_mux in
Storage\_access.bind](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml#L1107)
for the SR of that PBD.\
So basically what happens is that xapi registers itself as a SMAPIv2
server, and forwards incoming function calls to itself through
`message-switch`, using its `Storage_mux` module. These calls are
forwarded to xapi's `SMAPIv1` module doing SMAPIv2 -&gt; SMAPIv1
translation.

#### Registration of the various storage servers

{{<mermaid>}}
sequenceDiagram
participant q as message-switch
participant v1 as Storage_access.SMAPIv1
participant svr as Storage_mux.Server

Note over q, svr: xapi startup, "Starting SMAPIv1 proxies"
q ->> v1:org.xen.xapi.storage.sr_type_1
q ->> v1:org.xen.xapi.storage.sr_type_2
q ->> v1:org.xen.xapi.storage.sr_type_3

Note over q, svr: xapi startup, "Starting SM service"
q ->> svr:org.xen.xapi.storage 

Note over q, svr: SR.create, PBD.plug
svr ->> q:org.xapi.storage.sr_type_2
{{< /mermaid >}}

#### What happens when a SMAPIv2 "function" is called

{{<mermaid>}}
graph TD

call[SMAPIv2 call] --VDI.attach2--> org.xen.xapi.storage

subgraph message-switch
org.xen.xapi.storage
org.xen.xapi.storage.SR_type_x
end

org.xen.xapi.storage --VDI.attach2--> Storage_impl.Wrapper

subgraph xapi
subgraph Storage_mux.server
Storage_impl.Wrapper --> Storage_mux.mux
end
Storage_access.SMAPIv1
end

Storage_mux.mux --VDI.attach2--> org.xen.xapi.storage.SR_type_x
org.xen.xapi.storage.SR_type_x --VDI.attach2--> Storage_access.SMAPIv1

subgraph SMAPIv1
driver_x[SMAPIv1 driver for SR_type_x]
end

Storage_access.SMAPIv1 --vdi_attach--> driver_x
{{< /mermaid >}}

### Interface Changes, Backward Compatibility, & SXM

During SXM, xapi calls SMAPIv2 functions on a remote xapi. Therefore it
is important to keep all those SMAPIv2 functions backward-compatible
that we call remotely (e.g. Remote.VDI.attach), otherwise SXM from an
older to a newer xapi will break.

### Functionality implemented in SMAPIv2 layers

The layer between SMAPIv2 and SMAPIv1 is much fatter than the one between
SMAPIv2 and SMAPIv3. The latter does not do much, apart from simple
translation. However, the former has large portions of code in its intermediate
layers, in addition to the basic SMAPIv2 <-> SMAPIv1 translation in
`storage_access.ml`.

These are the three files in xapi that implement the SMAPIv2 storage interface,
from higher to lower level:

-   [xen-api/ocaml/xapi/storage\_impl.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_impl.ml):
-   [xen-api/ocaml/xapi/storage\_mux.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_mux.ml):
-   [xen-api/ocaml/xapi/storage\_access.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml):

Functionality implemented by higher layers is not implemented by the layers below it.

#### Extra functionality in `storage_impl.ml`

In addition to its usual functions, `Storage_impl.Wrapper` also implements the
`UPDATES` and `TASK` SMAPIv2 APIs, without calling the wrapped module.

These are backed by the `Updates`, `Task_server`, and `Scheduler` modules from
xcp-idl, instantiated in xapi's `Storage_task` module. Migration code in
`Storage_mux` will interact with these to update task progress. There is also
an event loop in xapi that keeps calling `UPDATES.get` to keep the tasks in
xapi's database in sync with the storage manager's tasks.

`Storage_impl.Wrapper` also implements the legacy `VDI.attach` call by simply
calling the newer `VDI.attach2` call in the same module. In general, this is a
good place to implement a compatibility layer for deprecated functionality
removed from other layers, because this is the first module that intercepts a
SMAPIv2 call.

#### Extra functionality in `storage_mux.ml`

`Storage_mux` implements storage motion (SXM): it implements the `DATA` and
`DATA.MIRROR` modules. Migration code will use the `Storage_task` module to run
the operations and update the task's progress.

It also implements the `Policy` module from the SMAPIv2 interface.

## SMAPIv3

[SMAPIv3](https://xapi-project.github.io/xapi-storage/) has a slightly
different interface from SMAPIv2.The
[xapi-storage-script](https://github.com/xapi-project/xapi-storage-script)
daemon is a SMAPIv2 plugin separate from xapi that is doing the SMAPIv2
↔ SMAPIv3 translation. It keeps the plugins registered with xcp-idl
(their message-switch queues) up to date as their files appear or
disappear from the relevant directory.

### SMAPIv3 Interface

The SMAPIv3 interface is defined using an OCaml-based IDL from the
[ocaml-rpc](https://github.com/mirage/ocaml-rpc) library, and is in this
repo: <https://github.com/xapi-project/xapi-storage>

From this interface we generate

-   OCaml RPC client bindings used in
    [xapi-storage-script](https://github.com/xapi-project/xapi-storage-script)
-   The [SMAPIv3 API
    reference](https://xapi-project.github.io/xapi-storage)
-   Python bindings, used by the SM scripts that implement the SMAPIv3
    interface.
    -   These bindings are built by running "`make`" in the root
        [xapi-storage](https://github.com/xapi-project/xapi-storage),
        and appear in the` _build/default/python/xapi/storage/api/v5`
        directory.
    -   On a XenServer host, they are stored in the
        `/usr/lib/python3.6/site-packages/xapi/storage/api/v5/`
        directory

### SMAPIv3 Plugins

For [SMAPIv3](https://xapi-project.github.io/xapi-storage/) we have
volume plugins to manipulate SRs and volumes (=VDIs) in them, and
datapath plugins for connecting to the volumes. Volume plugins tell us
which datapath plugins we can use with each volume, and what to pass to
the plugin. Both volume and datapath plugins implement some common
functionality: the SMAPIv3 [plugin
interface](https://xapi-project.github.io/xapi-storage/#plugin).

### How SMAPIv3 works:

The `xapi-storage-script` daemon detects volume and datapath plugins
stored in subdirectories of the
`/usr/libexec/xapi-storage-script/volume/` and
`/usr/libexec/xapi-storage-script/datapath/` directories, respectively.
When it finds a new datapath plugin, it adds the plugin to a lookup table and
uses it the next time that datapath is required. When it finds a new volume
plugin, it binds a new [message-switch] queue named after the plugin's
subdirectory to a new server instance that uses these volume scripts.

To invoke a SMAPIv3 method, it executes a program named
`<Interface name>.<function name>` in the plugin's directory, for
example
`/usr/libexec/xapi-storage-script/volume/org.xen.xapi.storage.gfs2/SR.ls`.
The inputs to each script can be passed as command-line arguments and
are type-checked using the generated Python bindings, and so are the
outputs. The URIs of the SRs that xapi-storage-script knows about are
stored in the `/var/run/nonpersistent/xapi-storage-script/state.db`
file, these URIs can be used on the command line when an sr argument is
expected.` `

#### Registration of the various SMAPIv3 plugins

{{<mermaid>}}
sequenceDiagram
participant q as message-switch
participant v1 as (Storage_access.SMAPIv1)
participant svr as Storage_mux.Server
participant vol_dir as /../volume/
participant dp_dir as /../datapath/
participant script as xapi-storage-script

Note over script, vol_dir: xapi-storage-script startup
script ->> vol_dir: new subdir org.xen.xapi.storage.sr_type_4
q ->> script: org.xen.xapi.storage.sr_type_4
script ->> dp_dir: new subdir sr_type_4_dp

Note over q, svr: xapi startup, "Starting SMAPIv1 proxies"
q -->> v1:org.xen.xapi.storage.sr_type_1
q -->> v1:org.xen.xapi.storage.sr_type_2
q -->> v1:org.xen.xapi.storage.sr_type_3

Note over q, svr: xapi startup, "Starting SM service"
q ->> svr:org.xen.xapi.storage 

Note over q, svr: SR.create, PBD.plug
svr ->> q:org.xapi.storage.sr_type_4
{{< /mermaid >}}

#### What happens when a SMAPIv3 "function" is called

{{<mermaid>}}
graph TD

call[SMAPIv2 call] --VDI.attach2--> org.xen.xapi.storage

subgraph message-switch
org.xen.xapi.storage
org.xen.xapi.storage.SR_type_x
end

org.xen.xapi.storage --VDI.attach2--> Storage_impl.Wrapper

subgraph xapi
subgraph Storage_mux.server
Storage_impl.Wrapper --> Storage_mux.mux
end
Storage_access.SMAPIv1
end

Storage_mux.mux --VDI.attach2--> org.xen.xapi.storage.SR_type_x

org.xen.xapi.storage.SR_type_x -."VDI.attach2".-> Storage_access.SMAPIv1

subgraph SMAPIv1
driver_x[SMAPIv1 driver for SR_type_x]
end

Storage_access.SMAPIv1 -.vdi_attach.-> driver_x

subgraph SMAPIv3
xapi-storage-script --Datapath.attach--> v3_dp_plugin_x
subgraph SMAPIv3 plugins
v3_vol_plugin_x[volume plugin for SR_type_x]
v3_dp_plugin_x[datapath plugin for SR_type_x]
end
end

org.xen.xapi.storage.SR_type_x --VDI.attach2-->xapi-storage-script
{{< /mermaid >}}

## Error reporting

In our SMAPIv1 OCaml "bindings" in xapi
([xen-api/ocaml/xapi/sm\_exec.ml](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/sm_exec.ml)),
[when we inspect the error codes returned from a call to
SM](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/sm_exec.ml#L199),
we translate some of the SMAPIv1/SM error codes to XenAPI errors, and
for others, we just [construct an error
code](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/sm_exec.ml#L214)
of the form `SR_BACKEND_FAILURE_<SM error number>`.

The file
[xcp-idl/storage/storage\_interface.ml](https://github.com/xapi-project/xcp-idl/blob/v1.62.0/storage/storage_interface.ml#L362)
defines a number of SMAPIv2 errors, ultimately all errors from the various
SMAPIv2 storage servers in xapi will be returned as one of these. Most of the
errors aren't converted into a specific exception in `Storage_interface`, but
are simply wrapped with `Storage_interface.Backend_error`.

The
[Storage\_access.transform\_storage\_exn](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/storage_access.ml#L29)
function is used by the client code in xapi to translate the SMAPIv2
errors into XenAPI errors again, this unwraps the errors wrapped with
`Storage_interface.Backend_error`.

## Message Forwarding

In the message forwarding layer, first we check the validity of VDI
operations using `mark_vdi` and `mark_sr`. These first check that the
operation is valid operations,
using [Xapi\_vdi.check\_operation\_error](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/xapi_vdi.ml#L57),
for `mark_vdi`, which also inspects the current operations of the VDI,
and then, if the operation is valid, it is added to the VDI's current
operations, and update\_allowed\_operations is called. Then we forward
the VDI operation to a suitable host that has a PBD plugged for the
VDI's SR.

### Checking that the SR is attached

For the VDI operations, we check at two different places whether the SR
is attached: first, at the Xapi level, [in
Xapi\_vdi.check\_operation\_error](https://github.com/xapi-project/xen-api/blob/v1.127.0/ocaml/xapi/xapi_vdi.ml#L98),
for the resize operation, and then, at the SMAPIv1 level, in
`Sm.assert_pbd_is_plugged`. `Sm.assert_pbd_is_plugged` performs the
same checks, plus it checks that the PBD is attached to the localhost,
unlike Xapi\_vdi.check\_operation\_error. This behaviour is correct,
because `Xapi_vdi.check_operation_error` is called from the message
forwarding layer, which forwards the call to a host that has the SR
attached.

## VDI Identifiers and Storage Motion

-   VDI "location": this is the VDI identifier used by the SM backend.
    It is usually the UUID of the VDI, but for ISO SRs it is the name of
    the ISO.
-   VDI "content\_id": this is used for storage motion, to reduce the
    amount of data copied. When we copy over a VDI, the content\_id will
    initially be the same. However, when we attach a VDI as read-write,
    and then detach it, then we will blank its content\_id (set it to a
    random UUID), because we may have written to it, so the content
    could be different. .

[message-switch]: https://github.com/xapi-project/message-switch

