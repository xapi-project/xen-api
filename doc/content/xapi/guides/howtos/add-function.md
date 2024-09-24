+++
title = "Adding a function to the API"
+++
This page describes how to add a function to XenAPI.

Add message to API
------------------
The file `idl/datamodel.ml` is a description of the API, from which the
marshalling and handler code is generated.

In this file, the `create_obj` function is used to define a class which may
contain fields and support operations (known as "messages"). For example, the
identifier host is defined using create_obj to encapsulate the operations which
can be performed on a host.

In order to add a function to the API, we need to add a message to an existing
class. This entails adding a function in `idl/datamodel.ml` or one of the other datamodel files to describe the new
message and adding it to the class's list of messages. In this example, we are adding to `idl/datamodel_host.ml`.

The function to describe the new message will look something like the following:

    let host_price_of = call ~flags:[`Session]
        ~name:"price_of"
        ~in_oss_since:None
        ~lifecycle:[]
        ~params:[(Ref _host, "host", "The host containing the price information");
                 (String, "item", "The item whose price is queried")]
        ~result:(Float, "The price of the item")
        ~doc:"Returns the price of a named item."
        ~allowed_roles:_R_POOL_OP
        ()

By convention, the name of the function is formed from the name of the class
and the name of the message: host and price_of, in the example. An entry for
host_price_of is added to the messages of the host class:

    let host =
        create_obj ...
            ~messages: [...
                        host_price_of;
                       ]
    ...

The parameters passed to call are all optional (except ~name and ~lifecycle).

- The ~flags parameter is used to set conditions for the use of the message.
  For example, `Session is used to indicate that the call must be made in the
  presence of an existing session.

- The value of the `~lifecycle` parameter should be `[]` in new code, with dune
  automatically generating appropriate values (`datamodel_lifecycle.ml`)

- The ~params parameter describes a list of the formal parameters of the message.
  Each parameter is described by a triple. The first component of the triple is
  the type (from type ty in `idl/datamodel_types.ml`); the second is the name
  of the parameter, and the third is a human-readable description of the parameter.
  The first triple in the list is conventionally the instance of the class on
  which the message will operate. In the example, this is a reference to the host.

- Similarly, the ~result describes the message's return type, although this is
  permitted to merely be a single value rather than a list of values. If no
  ~result is specified, the default is unit.

- The ~doc parameter describes what the message is doing.

- The bool ~hide_from_docs parameter prevents the message from being included in the documentation when generated.

- The bool ~pool_internal parameter is used to indicate if the message should be callable by external systems or only internal hosts.

- The ~errs parameter is a list of possible exceptions that the message can raise.

- The parameter ~lifecycle takes in an array of (Status, version, doc) to indicate the lifecycle of the message type. This takes over from ~in_oss_since which indicated the release that the message type was introduced. NOTE: Leave this parameter empty, it will be populated on build.

- The ~allowed_roles parameter is used for access control (see below).


Compiling `xen-api.(hg|git)` will cause the code corresponding to this message
to be generated and output in `ocaml/xapi/server.ml`. In the example above, a
section handling an incoming call host.price_of appeared in `ocaml/xapi/server.ml`.
However, after this was generated, the rest of the build failed because this
call expects a price_of function in the Host object.

Update expose_get_all_messages_for list
---------------------------------------

If you are adding a new class, do not forget to add your new class \_name to
the expose_get_all_messages_for list, at the bottom of datamodel.ml, in
order to have automatically generated get_all and get_all_records functions
attached to it.

Update the RBAC field containing the roles expected to use the new API call
---------------------------------------------------------------------------

After the RBAC integration, Xapi provides by default a set of static roles
associated to the most common subject tasks.

The api calls associated with each role are defined by a new `~allowed_roles`
parameter in each api call, which specifies the list of static roles that
should be able to execute the call. The possible roles for this list is one of
the following names, defined in `datamodel.ml`:

- role_pool_admin
- role_pool_operator
- role_vm_power_admin
- role_vm_admin
- role_vm_operator
- role_read_only

So, for instance,

    ~allowed_roles:[role_pool_admin,role_pool_operator] (* this is not the recommended usage, see example below *)

would be a valid list (though it is not the recommended way of using
allowed_roles, see below), meaning that subjects belonging to either
role_pool_admin or role_pool_operator can execute the api call.

The RBAC requirements define a policy where the roles in the list above are
supposed to be totally-ordered by the set of api-calls associated with each of
them. That means that any api-call allowed to role_pool_operator should also be
in role_pool_admin; any api-call allowed to role_vm_power_admin should also be
in role_pool_operator and also in role_pool_admin; and so on. Datamodel.ml
provides shortcuts for expressing these totally-ordered set of roles policy
associated with each api-call:

- \_R_POOL_ADMIN, equivalent to [role_pool_admin]
- \_R_POOL_OP, equivalent to [role_pool_admin,role_pool_operator]
- \_R_VM_POWER_ADMIN, equivalent to [role_pool_admin,role_pool_operator,role_vm_power_admin]
- \_R_VM_ADMIN, equivalent to [role_pool_admin,role_pool_operator,role_vm_power_admin,role_vm_admin]
- \_R_VM_OP, equivalent to [role_pool_admin,role_pool_operator,role_vm_power_admin,role_vm_admin,role_vm_op]
- \_R_READ_ONLY, equivalent to [role_pool_admin,role_pool_operator,role_vm_power_admin,role_vm_admin,role_vm_op,role_read_only]

The `~allowed_roles` parameter should use one of the shortcuts in the list above,
instead of directly using a list of roles, because the shortcuts above make sure
that the roles in the list are in a total order regarding the api-calls
permission sets. Creating an api-call with e.g.
allowed_roles:[role_pool_admin,role_vm_admin] would be wrong, because that
would mean that a pool_operator cannot execute the api-call that a vm_admin can,
breaking the total-order policy expected in the RBAC 1.0 implementation.
In the future, this requirement might be relaxed.

So, the example above should instead be used as:

    ~allowed_roles:_R_POOL_OP  (* recommended usage via pre-defined totally-ordered role lists *)

and so on.

How to determine the correct role of a new api-call:
----------------------------------------------------

- if only xapi should execute the api-call, ie. it is an internal call: _R_POOL_ADMIN
- if it is related to subject, role, external-authentication: _R_POOL_ADMIN
- if it is related to accessing Dom0 (via console, ssh, whatever): _R_POOL_ADMIN
- if it is related to the pool object: R_POOL_OP
- if it is related to the host object, licenses, backups, physical devices: _R_POOL_OP
- if it is related to managing VM memory, snapshot/checkpoint, migration: _R_VM_POWER_ADMIN
- if it is related to creating, destroying, cloning, importing/exporting VMs: _R_VM_ADMIN
- if it is related to starting, stopping, pausing etc VMs or otherwise accessing/manipulating VMs: _R_VM_OP
- if it is related to being able to login, manipulate own tasks and read values only: _R_READ_ONLY

Update message forwarding
-------------------------

The "message forwarding" layer describes the policy of whether an incoming API
call should be forwarded to another host (such as another member of the pool)
or processed on the host which receives the call. This policy may be
non-trivial to describe and so cannot be auto-generated from the data model.

In `xapi/message_forwarding.ml`, add a function to the relevant module to
describe this policy. In the running example, we add the following function to
the Host module:

    let price_of ~__context ~host ~item =
        info "Host.price_of for item %s" item;
        let local_fn = Local.Host.price_of ~host ~item in
        do_op_on ~local_fn ~__context ~host
          (fun session_id rpc -> Client.Host.price_of ~rpc ~session_id ~host ~item)

After the ~__context parameter, the parameters of this new function should
match the parameters we specified for the message. In this case, that is the
host and the item to query the price of.

The do_op_on function takes a function to execute locally and a function to
execute remotely and performs one of these operations depending on whether the
given host is the local host.

The local function references Local.Host.price_of, which is a function we will
write in the next step.

Implement the function
----------------------

Now we write the function to perform the logic behind the new API call.
For a host-based call, this will reside in `xapi/xapi_host.ml`. For other
classes, other files with similar names are used.

We add the following function to `xapi/xapi_host.ml`:

    let price_of ~__context ~host ~item =
        if item = "fish" then 3.14 else 0.00

We also need to add the function to the interface `xapi/xapi_host.mli`:

    val price_of :
        __context:Context.t -> host:API.ref_host -> item:string -> float

Congratulations, you've added a function to the API!

Add the operation to the CLI
----------------------------

Edit `xapi-cli-server/cli_frontend.ml`. Add a block to the definition of cmdtable_data as
in the following example:

    "host-price-of",
    {
      reqd=["host-uuid"; "item"];
      optn=[];
      help="Find out the price of an item on a certain host.";
      implementation= No_fd Cli_operations.host_price_of;
      flags=[];
    };

Include here the following:

- The names of required (*reqd*) and optional (*optn*) parameters.
- A description to be displayed when calling *xe help \<cmd\>* in the help field.
- The *implementation* should use *With_fd* if any communication with the
  client is necessary (for example, showing the user a warning, sending the
  contents of a file, etc.) Otherwise, *No_fd* can be used as above.
- The *flags* field can be used to set special options:

    - *Vm_selectors*: adds a "vm" parameter for the name of a VM (rather than a UUID)
    - *Host_selectors*: adds a "host" parameter for the name of a host (rather than a UUID)
    - *Standard*: includes the command in the list of common commands displayed by *xe help*
    - *Neverforward:*
    - *Hidden:*
    - *Deprecated of string list:*

Now we must implement `Cli_operations.host_price_of`. This is done in
`xapi-cli-server/cli_operations.ml`. This function typically extracts the parameters and
forwards them to the internal implementation of the function. Other arbitrary
code is permitted. For example:

    let host_price_of printer rpc session_id params =
      let host = Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params) in
      let item = List.assoc "item" params in
      let price = string_of_float (Client.Host.price_of ~rpc ~session_id ~host ~item) in
      printer (Cli_printer.PList [price])

Tab Completion in the CLI
-------------------------

The CLI features tab completion for many of its commands' parameters.
Tab completion is implemented in the file `ocaml/xe-cli/bash-completion`, which
is installed on the host as `/etc/bash_completion.d/cli`, and is done on a
parameter-name rather than on a command-name basis. The main portion of the
bash-completion file is a case statement that contains a section for each of
the parameters that benefit from completion. There is also an entry that
catches all parameter names ending at -uuid, and performs an automatic lookup
of suitable UUIDs. The host-uuid parameter of our new host-price-of command
therefore automatically gains completion capabilities.

Executing the CLI operation
---------------------------

Recompile `xapi` with the changes described above and install it on a test machine.

Execute the following command to see if the function exists:

    xe help host-price-of

Invoke the function itself with the following command:

    xe host-price-of host-uuid=<tab> item=fish

and you should find out the price of fish.
