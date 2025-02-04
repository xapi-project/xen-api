+++
title = "Generated Parts of Xapi"
+++

## Introduction

Many parts of `xapi` are auto-generated during the build process.
This article aims to document some of these modules and how they relate to each
other. The intention of this article is to serve as a developer resource and,
as such, its contents are prone to change (or become inaccurate) as the
codebase evolves. The ultimate source of truth remains the codebase itself.

## Interface Description

All of XenAPI's data model is described within the `ocaml/idl` subdirectory.
The data model itself describes the classes that make up the API.
The classes themselves comprise fields and messages (methods), relating
functionality together.

## API Types (`aPI.ml`)

The internal representation of each object is a record whose type is specified
within the generated `API` module, as part of building `xapi-types`. For
example, the `task` object's structure is defined by the type `task_t`.
Similarly, `API` includes the internal type representation used to represent
fields.  For example, a type such as `(string -> vdi_operations) map`, used by
the data model, is defined as `(string * vdi_operations) list` within `API`
(where `vdi_operations` itself is a polymorphic variant also defined by
`API`).

Note that the all the type definitions within `API` are annotated with
`[@@deriving rpc]`. This ensures that the final module, after preprocessing,
also contains functions to marshal each type to/from `Rpc.t` values (which is
important for `Server` - described later - which receives that format as
input).

## Database Actions (`db_actions.ml`)

The majority of XenAPI consists of methods used to read and modify the fields
of objects in the database. These methods are automatically generated and
their implementations are placed within the generated `Db_actions` module.

The `Db_actions` module consists of various, related, parts: type definitions
for a subset of objects' fields, marshallers for converting API types to/from
strings, and database action handlers. Briefly, the role of each of these is
described below:

- Type definitions for XenAPI objects are redefined in `Db_actions` in order to
  exclude internal fields. If a field is marked as "internal only" within the
  data model, then it should only exist within internal representations (those
  defined by `API`, described above). For example, `task_t` (describing a
  `task` object) - as described by `Db_actions` - notably omits the field
  `task_session` (of type `session ref`) so as to not leak sensitive information
  to clients (in this case, the reference to the session that created the task
  object).

- Fields of the database are internally stored as strings and must be
  marshalled to typed values for use in OCaml code using DB actions. To this
  end, submodules `String_to_DM` and `DM_to_String` are generated to include code
  for doing these conversions. These submodules consist of the inverse
  operations of the other. For example, for the data model type `Observer ref
  set`, the function `ref_Observer_set` exists in both `String_to_DM`
  (as ``string -> [`Observer] API.Ref.t list``) and
  in `DM_to_String` (inversely, as ``[`Observer] API.Ref.t list -> string``).

- Handlers for actions that read/write fields of database objects are
  implemented by `Db_actions`. Each handler uses the relevant marshallers to
  marshal inputs and outputs. Note that `Db_actions` generates two variants of
  `get_record` for each class: a normal `get_record` which returns the public
  class representation as described by the types defined in `Db_actions`, and a
  `get_record_internal`, which returns the full class representation (including
  internal fields) as described by the `API` module.

### Registering for Snapshots

The `Db_actions` module also generates modules that register callbacks
for Xapi's event mechanisms. These modules are named in the format
`Class_init` and consist only of top-level code, evaluated for its
effect.

As each event must provide a snapshot of the related object, the event
mechanism must be able to read records from the database. To do this,
`Eventgen` exposes an API that `Db_actions` uses to register callbacks
(one for each type of object in the database).

For example, within `Db_actions`, there is a module `VM_init`,
consisting of:

```ocaml
module VM_init = struct
  let _  =
    Hashtbl.add Eventgen.get_record_table "VM"
      (fun ~__context ~self -> (fun () -> API.rpc_of_vM_t (VM.get_record ~__context ~self:(Ref.of_string self))))
end
```

As snapshots are served to external clients, the functions use the
public `get_record` functions - returning types defined by `API` -
which omits internal fields.

Notice that the type of values being mapped to is `__context:Context.t -> self:string -> unit -> Rpc.t`.

The presence of `unit` in the type is to permit partial application
(of `__context` and `self`) to create thunks. The type `unit -> Rpc.t`
is lossy, it says nothing about the context or object reference being
used to fetch the snapshot; these details are captured by the closure
arising from partial application. This means that code can arbitrarily
delay the fetching of a snapshot and then "force" it (on demand)
later. In practice, these snapshots are not delayed for long, see
`Eventgen` for more information.

## Custom Actions (`custom_actions.ml`)

The API operations that require a custom implementation (i.e. are not
automatically generated) are grouped together into a signature called
`CUSTOM_ACTIONS` within `custom_actions.ml`.

```ocaml
open API

module type CUSTOM_ACTIONS = sig

  module Session : sig
    val login_with_password : __context:Context.t -> uname:string -> pwd:string -> version:string -> originator:string -> ref_session
    val logout : __context:Context.t -> unit
(* ... *)
```

The isolation of these methods into their own signature is important for
ensuring implementations exist for them.

The `Actions` submodule of `Api_server_common` can be ascribed
this signature. The purpose of the `Actions` sub-module is to group custom
implementations together whilst renaming them using module aliases. For
example, the custom implementations of messages associate with the `task` class
exist within `xapi_task.ml` - the module arising from this file is aliased,
within `Actions`, to rename it and satisfy the `CUSTOM_ACTIONS` signature (e.g.
`module Task = Xapi_task`).

The signature is not explicitly ascribed to `Actions` at its definition site,
but is used by functors which are parameterised by modules satisfying
`CUSTOM_ACTIONS`. The important modules of note are `Actions` itself (which
comprise the concrete implementations of custom messages in Xapi) and the
`Forwarder` sub-module (of `Api_server_common`) which uses the
`Message_forwarding.Forward` functor to potentially override (via shadowing)
the implementations of custom actions, in order to define policies around
forwarding (e.g. whether the coordinator should handle a custom action by
appealing to a subordinate host, usually via the `Client` module - described
later).

## Server (`server.ml`)

The `Server` modules contains the logic to handle incoming calls from Xapi's
HTTP server. At the top level, it contains a functor (parameterised by `Local`
and `Forward` - both satisfying the `CUSTOM_ACTIONS` signature), that contains a
large pattern match on the name of the supplied message (e.g.
`Host.get_record`). Then, dependent on the semantics of the message itself, the
body of each handler differs in slight ways when doing dispatch.

### The top-level `dispatch_call` function

The top-level dispatch function, `dispatch_call`, has the following header:

```ocaml
let dispatch_call (http_req: Http.Request.t) (fd: Unix.file_descr) (call: Rpc.call) =
```

The incoming HTTP request (`http_req`) and - related - socket's file descriptor
(`Unix.file_descr)` are forwarded - within handler code - to
`Server_helpers.do_dispatch`. The HTTP request is important because task and
tracing-related metadata can be propagated using fields within the request
header. The file descriptor can be used to determine the origin of the request
(whether it's local or not) but also can permit flexibility in upgrading
protocols (as is done in other parts of Xapi, such as the `/cli` handler, where
the connection starts off as HTTP but continues as something else).

### The Anatomy of a Handler

A typical handler, within `dispatch_call`, looks like the following:

```ocaml
    | "task.get_name_label" | "task_get_name_label" ->
        begin match __params with
        | [session_id_rpc; self_rpc] ->
            (* has no side-effect; should be handled by DB action *)
            (* has no asynchronous mode *)
            let session_id = ref_session_of_rpc session_id_rpc in
            let self = ref_task_of_rpc self_rpc in
            Session_check.check ~intra_pool_only:false ~session_id ~action:"task.get_name_label";
            let arg_names_values = [("session_id", session_id_rpc); ("self", self_rpc)] in
            let key_names = [] in
            let rbac __context fn = Rbac.check session_id __call ~args:arg_names_values ~keys:key_names ~__context ~fn in
            let marshaller = (fun x -> rpc_of_string x) in
            let local_op = fun ~__context ->(rbac __context (fun()->(Db_actions.DB_Action.Task.get_name_label ~__context:(Context.check_for_foreign_database ~__context)  ~self))) in
            let supports_async = false in
            let generate_task_for = false in
            ApiLogRead.debug "task.get_name_label";
            let resp = Server_helpers.do_dispatch ~session_id  supports_async __call local_op marshaller fd http_req __label __sync_ty generate_task_for in
            resp
        | _ ->
            Server_helpers.parameter_count_mismatch_failure __call "1" (string_of_int ((List.length __params) - 1))
        end
```

The start of each handler contains calls to unmarshal arguments from their
`Rpc.t` representation to that defined by the `API` module. These functions are
automatically generated during preprocessing of the `aPI.ml` file (`API`
modules from `xapi-types`, described above). The conversion from the incoming
XML-RPC (or JSON-RPC) to the `Rpc.t` encoding is handled by `Api_server` before
it calls `dispatch_call`.

In the example above, the "local" operation (`local_op`) uses handlers
generated within `Db_actions` (described above). This is typical of handlers
for DB-related actions (the most common type of action): they have no
forwarding logic (thus, no entry in the `CUSTOM_ACTIONS` signature) as they can
only be carried out on the coordinator host (which maintains the database). If
a subordinate host wishes to change the database, it must use a custom endpoint
and protocol (not described here).

---

To see more about how the `CUSTOM_ACTIONS` signature is used in practice, you
can look at the "local" and "forward" operations for a message with custom
handling. For example, in `Pool_patch.apply`:

```ocaml
(* ... *)
let local_op = fun ~__context ->(rbac __context (fun()->(Custom.Pool_patch.apply ~__context:(Context.check_for_foreign_database ~__context)  ~self ~host))) in
(* ...  *)
let forward_op = fun ~local_fn ~__context -> (rbac __context (fun()-> (Forward.Pool_patch.apply ~__context:(Context.check_for_foreign_database ~__context)  ~self ~host) )) in
(* ...  *)
```

As mentioned above, the `Custom` and `Forward` modules are both inputs
to `Server`'s `Make` functor. The difference lies in how they are
instantiated: `Api_server` ensures that `Custom` is referring to local
implementations (such as that arising from modules defined by files
named `xapi_*.ml`) and `Forward` is referring to the module derived by
`Message_forwarding` (but shadowed with implementations that may apply
different handling to the call).

### RBAC Checking and Auditing

In order to implement RBAC (Role Based Access Control) checking for individual
messages, each handler contains logic that wraps an action (as a callback)
within code that calls into the `Rbac` module (specifically, the `Rbac.check`
function).

In the typical case, `Rbac.check` compares the name of a call against the list
of RBAC permissions granted to the role associated with the originator's
session/context. There is more involved logic for key-related RBAC checks
(explained later).

For an accessible listing of each (static) RBAC permission, Xapi auto-generates
a CSV file containing this information in a tabular format (within
`rbac_static.csv`). The information in that file is consistent with the
auto-generated `Rbac_static` module described in this document.

Along with providing authorisation checking, the `Rbac.check` function also
appends to an audit log which contains a (sanitised) list of actions (alongside
their RBAC check outcome).

### RBAC Checking of Keys

In auto-generated handlers for `add_to` and `remove_from` messages (e.g.
`pool.add_to_other_config`), the RBAC check may cite a list of key
descriptors. For example:

```ocaml
(* pool.add_to_other_config ...  *)
let arg_names_values = [("session_id", session_id_rpc); ("self", self_rpc); ("key", key_rpc); ("value", value_rpc)] in
let key_names = ["folder"; "XenCenter.CustomFields.*"; "EMPTY_FOLDERS"] in
let rbac __context fn = Rbac.check session_id __call ~args:arg_names_values ~keys:key_names ~__context ~fn in
(* ... *)
```

These keys are specified within the data model as being tied to specific roles,
in order to apply role-based exclusions to specific keys.  The usual situation
is that the setter for such a `(string -> string) map` field (e.g.
`pool.set_other_config`) requires a more privileged role than the roles
specified for individual keys.

The mechanism that enforces this check is somewhat brittle at present: the
`Rbac.check` function is provided the list of key descriptors and the
(association) list of (unmarshalled) arguments. If the key descriptor list is
non-empty, it will consult the argument listing for the cited key (i.e. the key
name mapped to by "key" in the argument listing) and then attempt to match that
against a descriptor. If there is a match, it will check the current session
against the list of RBAC permissions. The key-related RBAC permissions are
encoded in the format `action/key:key` (all lowercase) - for example,
`pool.add_to_other_config/key:xencenter.customfields.*`.

### Alternative Wire Names

In order to support languages that have keywords that collide with message
names within Xapi, an alternative wire format is also cased upon within
`dispatch_call`.

```ocaml
| "task.get_name_label" | "task_get_name_label" ->
(* ... *)
```

For example, Python uses the keyword `from` to handle imports and, so, an API
call (using `xmlrpc.client`) - rendered as `event.from` - is a syntactic error.
To get around this, the API permits an underscore to be substituted in place of
the period (`.`) that separates the class name from the message name (e.g.
`event_from`).

This apparent duplication of cases does not amount to a concrete duplication of
matching code within the compiled module (due to how OCaml special cases the
compilation of pattern matching over constant strings). However, in future, we
could avoid casing on both of them by normalising the name of the incoming call
(i.e. transform `event.from` to `event_from` prior to matching).

## Client (`client.ml`)

The `Client` module serves as the main module of the `xapi-client` library. The
primary consumer of this library is Xapi itself, for use when a host may call
into another host (or itself).

For example, when defining a message forwarding policy, the implementation of a
handler may use the `Client` module to invoke a function on another host. For
instance, the message forwarding of `Pool_patch.apply` (from
`xapi/message_forwarding.ml`):

```ocaml
let apply ~__context ~self ~host =
  info "Pool_patch.apply: pool patch = '%s'; host = '%s'"
    (pool_patch_uuid ~__context self)
    (host_uuid ~__context host) ;
  let local_fn = Local.Pool_patch.apply ~self ~host in
  do_op_on ~local_fn ~__context ~host (fun session_id rpc ->
    Client.Pool_patch.apply ~rpc ~session_id ~self ~host
  )
```

The `do_op_on` machinery provides the `rpc` transport (`Rpc.call ->
Rpc.response`) to the callback which passes it to `Client`'s implementation
(which just performs the relevant marshalling). The RPC transport itself is
XML-RPC over HTTP (as implemented by the internal `http-lib` library -
`ocaml/libs/http-lib`).

### Client Internals

Internally, the `Client` module contains a few functors. The top-level functor,
`ClientF` is parameterised by a signature describing an arbitrary monad. The
intention is to permit users to instantiate clients defined in terms of an RPC
transport that may be asynchronous (for example, within the context of a
program using `Lwt` or `Async` for its networking).

There is also a sub-functor `AsyncF` (within `ClientF`) that is parameterised
by a module that provides a qualifier string to be prepended to calls' method
names. A few messages in Xapi can be qualified with async qualifiers (in
particular, `Async` and `InternalAsync`). The `AsyncF` functor provides
handling of those calls and is used to define the sub-modules (within
`ClientF`) `Async` and `InternalAsync`. (the former prepending `Async` and the
latter further prepending `Internal`). Code at the top-level of `Server`'s
`dispatch_call` function is used to parse (and remove) this async qualifier
from the provided message name.

```ocaml
module ClientF = functor(X : IO) ->struct
  (* ... *)
  module AsyncF = functor(AQ: AsyncQualifier) ->struct
    (* handling of messages with asynchronous modes *)
    module Session = struct
      let create_from_db_file ~rpc ~session_id ~filename =
        let session_id = rpc_of_ref_session session_id in
        let filename = rpc_of_string filename in
        rpc_wrapper rpc (Printf.sprintf "%sAsync.session.create_from_db_file" AQ.async_qualifier) [ session_id; filename ] >>= fun x -> return (ref_task_of_rpc  x)
        (* ... *)
    end
    (* ...  *)
  end
  (* handling of messages with synchronous modes; similar to above, but without prefixing of "Async" *)

  module Async = AsyncF(struct let async_qualifier = "" end)
  module InternalAsync = AsyncF(struct let async_qualifier = "Internal" end)
end
(* instantiate Client with the identity monad *)
module Client = ClientF(Id)
```

The usual `Client` module used by users of `xapi-client` is the `Client`
sub-module, defined in terms of the identity monad (which simply applies the
given continuation as its sequencing logic and performs no wrapping):

```ocaml
module Id = struct
  type 'a t = 'a
  let bind x f = f x 
  let return x = x
end

module Client = ClientF(Id)
```

This results in synchronous semantics, whereby any code within `Xapi` that uses
it would block as it waits for a response via the RPC transport. This is not an
issue in practice, as each call is given its own thread during the dispatch logic.

Note that the RPC transport itself is defined in terms of the provided monad.
In the identity case, it's a simple alias, and so the type of `rpc` is rendered
`Rpc.call -> Rpc.response`. However, if you were to provide a monad defined,
for example, in terms of `Lwt.t` (i.e. `type 'a t = 'a Lwt.t`), the expected
type of the transport would reflect that: `Rpc.call -> Rpc.response Lwt.t`.

## Rbac_static

The data model assigns specific roles to messages and fields. In order
to permit RBAC (Role Based Access Control) checking for the related
actions, Xapi must be able to determine the required role(s) for a
given action. To this end, `Rbac_static` is generated to contain
entries that encode this information.

The format of the entries in `Rbac_static` is rather peculiar. For
example, for the action `Pool_patch.apply`, we find
`permission_pool_patch_apply` defined at the top-level:

```ocaml
let permission_pool_patch_apply = 
  { (* 311/2196 *)
  role_uuid = "d4385002-b920-5412-4c57-b010f451fa81";
  role_name_label = "pool_patch.apply";
  role_name_description = permission_description;
  role_subroles = []; (* permission cannot have any subroles *)
  role_is_internal = true;
  }
```

This record is of type `role_t` (as defined by `Db_actions`). This
record is later incorporated into role-specific lists of permissions
(for each statically known role).

The reason that `Rbac_static` defines permissions in a format defined
by `Db_actions` is because, to avoid flooding the database with
thousands of entries, `Rbac_static` acts as its own database. In
`Xapi_role`, functions are defined that mirror the functionality of
functions within `Db_actions` (e.g. `get_by_uuid`).

The `get_by_uuid` function (within `Xapi_role`) illustrates the bypassing of the database clearly:

```ocaml
let get_by_uuid ~__context ~uuid =
  match find_role_by_uuid uuid with
  | Some static_record ->
      ref_of_role ~role:static_record
  | None ->
      (* pass-through to Db *)
      Db.Role.get_by_uuid ~__context ~uuid
```

If a role can be found (by UUID) statically (within `Rbac_static`),
then that is used. Otherwise, the database is queried. Using the
database as a fallback is important because there is still a dynamic
component to the RBAC checking in Xapi: users can define their own
roles that incorporate other roles as sub-roles - it's just that
the statically-known roles won't be stored in the database. Precluding
static roles from the database helps to avoid making the database
larger and prevents users from deleting static roles from the
database.
