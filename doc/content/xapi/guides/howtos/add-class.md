+++
title = "Adding a Class to the API"
+++

This document describes how to add a new class to the data model that
defines the Xen Server API. It complements two other documents that
describe how to extend an existing class:

* [Adding a field](add-field)
* [Adding a function](add-function)

As a running example, we will use the addition of a class that is part
of the design for the PVS Direct feature. PVS Direct introduces
proxies that serve VMs with disk images. This class was added via commit
[CP-16939] to Xen API.

## Example: PVS_server

In the world of Xen Server, each important concept like a virtual
machine, interface, or users is represented by a class in the data model.
A class defines methods and instance variables. At runtime, all class
instances are held in an in-memory database. For example, part of [PVS
Direct] is a class `PVS_server`, representing a resource that provides
block-level data for virtual machines. The design document defines it to
have the following important properties:

### Fields

*   `(string set) addresses` (RO/constructor) IPv4 addresses of the
    server.

*   `(int) first_port` (RO/constructor) First UDP port accepted by the
    server.

*   `(int) last_port` (RO/constructor) Last UDP port accepted by the
    server.

*   `(PVS_farm ref) farm` (RO/constructor) Link to the farm that this
    server is included in. A PVS_server object must always have a valid
    farm reference; the PVS_server will be automatically GC’ed by xapi
    if the associated PVS_farm object is removed.

*   `(string) uuid (R0/runtime)` Unique identifier/object reference.
    Allocated by the server.

### Methods (or Functions)

*   `(PVS_server ref) introduce (string set addresses, int first_port,
    int last_port, PVS_farm ref farm)` Introduce a new PVS server into
    the farm.  Allowed at any time, even when proxies are in use. The
    proxies will be updated automatically.

*   `(void) forget (PVS_server ref self)` Remove a PVS server from the
    farm.  Allowed at any time, even when proxies are in use. The
    proxies will be updated automatically.


### Implementation Overview

The implementation of a class is distributed over several files:

* `ocaml/idl/datamodel.ml` -- central class definition
* `ocaml/idl/datamodel_types.ml` -- definition of releases
* `ocaml/xapi/cli_frontend.ml` -- declaration of CLI operations
* `ocaml/xapi/cli_operations.ml` -- implementation of CLI operations
* `ocaml/xapi/records.ml` -- getters and setters
* `ocaml/xapi/OMakefile` -- refers to `xapi_pvs_farm.ml`
* `ocaml/xapi/api_server.ml` -- refers to `xapi_pvs_farm.ml`
* `ocaml/xapi/message_forwarding.ml`
* `ocaml/xapi/xapi_pvs_farm.ml` -- implementation of methods, new file

### Data Model

The data model `ocaml/idl/datamodel.ml` defines the class. To keep the
name space tidy, most helper functions are grouped into an internal
module:

    (* datamodel.ml *)

    let schema_minor_vsn = 103 (* line 21 -- increment this *)
    let _pvs_farm = "PVS_farm" (* line 153 *)

    module PVS_farm = struct (* line 8658 *)
      let lifecycle = [Prototyped, rel_dundee_plus, ""]

      let introduce = call
        ~name:"introduce"
        ~doc:"Introduce new PVS farm"
        ~result:(Ref _pvs_farm, "the new PVS farm")
        ~params:
        [ String,"name","name of the PVS farm"
        ]
        ~lifecycle
        ~allowed_roles:_R_POOL_OP
        ()

      let forget = call
        ~name:"forget"
        ~doc:"Remove a farm's meta data"
        ~params:
        [ Ref _pvs_farm, "self", "this PVS farm"
        ]
        ~errs:[
          Api_errors.pvs_farm_contains_running_proxies;
          Api_errors.pvs_farm_contains_servers;
        ]
        ~lifecycle
        ~allowed_roles:_R_POOL_OP
        ()


      let set_name = call
        ~name:"set_name"
        ~doc:"Update the name of the PVS farm"
        ~params:
        [ Ref _pvs_farm, "self", "this PVS farm"
        ; String, "value", "name to be used"
        ]
        ~lifecycle
        ~allowed_roles:_R_POOL_OP
        ()

      let add_cache_storage = call
        ~name:"add_cache_storage"
        ~doc:"Add a cache SR for the proxies on the farm"
        ~params:
        [ Ref _pvs_farm, "self", "this PVS farm"
        ; Ref _sr, "value", "SR to be used"
        ]
        ~lifecycle
        ~allowed_roles:_R_POOL_OP
        ()

      let remove_cache_storage = call
        ~name:"remove_cache_storage"
        ~doc:"Remove a cache SR for the proxies on the farm"
        ~params:
        [ Ref _pvs_farm, "self", "this PVS farm"
        ; Ref _sr, "value", "SR to be removed"
        ]
        ~lifecycle
        ~allowed_roles:_R_POOL_OP
        ()

      let obj =
        let null_str = Some (VString "") in
        let null_set = Some (VSet []) in
        create_obj (* <---- creates class *)
        ~name: _pvs_farm
        ~descr:"machines serving blocks of data for provisioning VMs"
        ~doccomments:[]
        ~gen_constructor_destructor:false
        ~gen_events:true
        ~in_db:true
        ~lifecycle
        ~persist:PersistEverything
        ~in_oss_since:None
        ~messages_default_allowed_roles:_R_POOL_OP
        ~contents:
        [ uid     _pvs_farm ~lifecycle

        ; field   ~qualifier:StaticRO ~lifecycle
                  ~ty:String "name" ~default_value:null_str
                  "Name of the PVS farm. Must match name configured in PVS"

        ; field   ~qualifier:DynamicRO ~lifecycle
                  ~ty:(Set (Ref _sr)) "cache_storage" ~default_value:null_set
                  ~ignore_foreign_key:true
                  "The SR used by PVS proxy for the cache"

        ; field   ~qualifier:DynamicRO ~lifecycle
                  ~ty:(Set (Ref _pvs_server)) "servers"
                  "The set of PVS servers in the farm"


        ; field   ~qualifier:DynamicRO ~lifecycle
                  ~ty:(Set (Ref _pvs_proxy)) "proxies"
                  "The set of proxies associated with the farm"
        ]
        ~messages:
        [ introduce
        ; forget
        ; set_name
        ; add_cache_storage
        ; remove_cache_storage
        ]
        ()
    end
    let pvs_farm = PVS_farm.obj

The class is defined by a call to `create_obj` and it defines the
fields and messages (methods) belonging to the class. Each field has a
name, a type, and some meta information. Likewise, each message
(or method) is created by `call` that describes its parameters.

The `PVS_farm` has additional getter and setter methods for accessing
its fields. These are not declared here as part of the messages
but are automatically generated.

To make sure the new class is actually used, it is important to enter it
into two lists:

    (* datamodel.ml *)
    let all_system = (* line 8917 *)
      [
        ...
        vgpu_type;
        pvs_farm;
        ...
      ]

    let expose_get_all_messages_for = [ (* line 9097 *)
      ...
      _pvs_farm;
      _pvs_server;
      _pvs_proxy;

When a field refers to another object that itself refers back to it,
these two need to be entered into the `all_relations` list. For example,
`_pvs_server` refers to a `_pvs_farm` value via `"farm"`, which, in
turn, refers to the `_pvs_server` value via its `"servers"` field.

    let all_relations =
      [
        (* ... *)
        (_sr, "introduced_by"), (_dr_task, "introduced_SRs");
        (_pvs_server, "farm"), (_pvs_farm, "servers");
        (_pvs_proxy,  "farm"), (_pvs_farm, "proxies");
      ]


## CLI Conventions

The CLI provides access to objects from the command line. The following
conventions exist for naming fields:

* A field in the data model uses an underscore (`_`) but a hyphen (`-`)
  in the CLI: what is `cache_storage` in the data model becomes
  `cache-storage` in the CLI.

* When a field contains a reference or multiple, like `proxies`, it
  becomes `proxy-uuids` in the CLI because references are always
  referred to by their UUID.

## CLI Getters and Setters

All fields can be read from the CLI and some fields can also be set via
the CLI. These getters and setters are mostly generated automatically
and need to be connected to the CLI through a function in
`ocaml/xapi/records.ml`. Note that field names here use the
naming convention for the CLI:

    (* ocaml/xapi/records.ml *)
    let pvs_farm_record rpc session_id pvs_farm =
      let _ref = ref pvs_farm in
      let empty_record =
        ToGet (fun () -> Client.PVS_farm.get_record rpc session_id !_ref) in
      let record = ref empty_record in
      let x () = lzy_get record in
        { setref    = (fun r -> _ref := r ; record := empty_record)
        ; setrefrec = (fun (a,b) -> _ref := a; record := Got b)
        ; record    = x
        ; getref    = (fun () -> !_ref)
        ; fields=
          [ make_field ~name:"uuid"
            ~get:(fun () -> (x ()).API.pVS_farm_uuid) ()
          ; make_field ~name:"name"
            ~get:(fun () -> (x ()).API.pVS_farm_name)
            ~set:(fun name ->
              Client.PVS_farm.set_name rpc session_id !_ref name) ()
          ; make_field ~name:"cache-storage"
            ~get:(fun () -> (x ()).API.pVS_farm_cache_storage
              |> List.map get_uuid_from_ref |> String.concat "; ")
            ~add_to_set:(fun sr_uuid ->
              let sr = Client.SR.get_by_uuid rpc session_id sr_uuid in
              Client.PVS_farm.add_cache_storage rpc session_id !_ref sr)
            ~remove_from_set:(fun sr_uuid ->
              let sr = Client.SR.get_by_uuid rpc session_id sr_uuid in
              Client.PVS_farm.remove_cache_storage rpc session_id !_ref sr)
            ()
          ; make_field ~name:"server-uuids"
            ~get:(fun () -> (x ()).API.pVS_farm_servers
              |> List.map get_uuid_from_ref |> String.concat "; ")
            ~get_set:(fun () -> (x ()).API.pVS_farm_servers
              |> List.map get_uuid_from_ref)
            ()
          ; make_field ~name:"proxy-uuids"
            ~get:(fun () -> (x ()).API.pVS_farm_proxies
              |> List.map get_uuid_from_ref |> String.concat "; ")
            ~get_set:(fun () -> (x ()).API.pVS_farm_proxies
              |> List.map get_uuid_from_ref)
            ()
          ]
        }

## CLI Interface to Methods

Methods accessible from the CLI are declared in
`ocaml/xapi/cli_frontend.ml`. Each declaration refers to the real
implementation of the method, like `Cli_operations.PVS_far.introduce`:

    (* cli_frontend.ml *)
    let rec cmdtable_data : (string*cmd_spec) list =
      (* ... *)
      "pvs-farm-introduce",
      {
        reqd=["name"];
        optn=[];
        help="Introduce new PVS farm";
        implementation=No_fd Cli_operations.PVS_farm.introduce;
        flags=[];
      };
      "pvs-farm-forget",
      {
        reqd=["uuid"];
        optn=[];
        help="Forget a PVS farm";
        implementation=No_fd Cli_operations.PVS_farm.forget;
        flags=[];
      };

## CLI Implementation of Methods

Each CLI operation that is not a getter or setter has an implementation
in `cli_operations.ml` which is implemented in terms of the real
implementation:

    (* cli_operations.ml *)
    module PVS_farm = struct
      let introduce printer rpc session_id params =
        let name  = List.assoc "name" params in
        let ref   = Client.PVS_farm.introduce ~rpc ~session_id ~name in
        let uuid  = Client.PVS_farm.get_uuid rpc session_id ref in
        printer (Cli_printer.PList [uuid])

      let forget printer rpc session_id params =
        let uuid  = List.assoc "uuid" params in
        let ref   = Client.PVS_farm.get_by_uuid ~rpc ~session_id ~uuid in
        Client.PVS_farm.forget rpc session_id ref
    end

Fields that should show up in the CLI interface by default are declared
in the `gen_cmds` value:

    (* cli_operations.ml *)
    let gen_cmds rpc session_id =
      let mk = make_param_funs in
      List.concat
      [ (*...*)
      ; Client.Pool.(mk get_all get_all_records_where
        get_by_uuid pool_record "pool" []
        ["uuid";"name-label";"name-description";"master"
        ;"default-SR"] rpc session_id)
      ; Client.PVS_farm.(mk get_all get_all_records_where
        get_by_uuid pvs_farm_record "pvs-farm" []
        ["uuid";"name";"cache-storage";"server-uuids"] rpc session_id)


## Error messages

Error messages used by an implementation are introduced in two files:

    (* ocaml/xapi-consts/api_errors.ml *)
    let pvs_farm_contains_running_proxies = "PVS_FARM_CONTAINS_RUNNING_PROXIES"
    let pvs_farm_contains_servers = "PVS_FARM_CONTAINS_SERVERS"
    let pvs_farm_sr_already_added = "PVS_FARM_SR_ALREADY_ADDED"
    let pvs_farm_sr_is_in_use = "PVS_FARM_SR_IS_IN_USE"
    let sr_not_in_pvs_farm = "SR_NOT_IN_PVS_FARM"
    let pvs_farm_cant_set_name = "PVS_FARM_CANT_SET_NAME"

    (* ocaml/idl/datamodel.ml *)
      (* PVS errors *)
      error Api_errors.pvs_farm_contains_running_proxies ["proxies"]
        ~doc:"The PVS farm contains running proxies and cannot be forgotten." ();

      error Api_errors.pvs_farm_contains_servers ["servers"]
        ~doc:"The PVS farm contains servers and cannot be forgotten."
        ();

      error Api_errors.pvs_farm_sr_already_added ["farm"; "SR"]
        ~doc:"Trying to add a cache SR that is already associated with the farm"
        ();

      error Api_errors.sr_not_in_pvs_farm ["farm"; "SR"]
        ~doc:"The SR is not associated with the farm."
        ();

      error Api_errors.pvs_farm_sr_is_in_use ["farm"; "SR"]
        ~doc:"The SR is in use by the farm and cannot be removed."
        ();

      error Api_errors.pvs_farm_cant_set_name ["farm"]
        ~doc:"The name of the farm can't be set while proxies are active."
        ()

## Method Implementation

The implementation of methods lives in a module in `ocaml/xapi`:

    (* ocaml/xapi/api_server.ml *)
      module PVS_farm = Xapi_pvs_farm

The file below is typically a new file and needs to be added to
`ocaml/xapi/OMakefile`.

    (* ocaml/xapi/xapi_pvs_farm.ml *)
    module D = Debug.Make(struct let name = "xapi_pvs_farm" end)
    module E = Api_errors

    let api_error msg xs = raise (E.Server_error (msg, xs))

    let introduce ~__context ~name =
      let pvs_farm = Ref.make () in
      let uuid = Uuid.to_string (Uuid.make_uuid ()) in
      Db.PVS_farm.create ~__context
        ~ref:pvs_farm ~uuid ~name ~cache_storage:[];
      pvs_farm

    (* ... *)


Messages received on a slave host may or may not be executed there. In
the simple case, each methods executes locally:

    (* ocaml/xapi/message_forwarding.ml *)
    module PVS_farm = struct
      let introduce ~__context ~name =
        info "PVS_farm.introduce %s" name;
        Local.PVS_farm.introduce ~__context ~name

      let forget ~__context ~self =
        info "PVS_farm.forget";
        Local.PVS_farm.forget ~__context ~self

      let set_name ~__context ~self ~value =
        info "PVS_farm.set_name %s" value;
        Local.PVS_farm.set_name ~__context ~self ~value

      let add_cache_storage ~__context ~self ~value =
        info "PVS_farm.add_cache_storage";
        Local.PVS_farm.add_cache_storage ~__context ~self ~value

      let remove_cache_storage ~__context ~self ~value =
        info "PVS_farm.remove_cache_storage";
        Local.PVS_farm.remove_cache_storage ~__context ~self ~value
    end



[CP-16939]:           https://github.com/xenserver/xen-api/commit/78fe558dad19458a89519fe196069317d57eac58
[Adding a Field]:     add-field.html
[Adding a Function]:  add-function.html
