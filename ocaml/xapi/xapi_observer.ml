(*
 * Copyright (C) 2023 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = "xapi_observer" end)

open D
open Xapi_observer_components

let ( // ) = Filename.concat

module type ObserverInterface = sig
  val create :
       __context:Context.t
    -> uuid:string
    -> name_label:string
    -> attributes:(string * string) list
    -> endpoints:string list
    -> enabled:bool
    -> unit

  val destroy : __context:Context.t -> uuid:string -> unit

  val set_enabled : __context:Context.t -> uuid:string -> enabled:bool -> unit

  val set_attributes :
       __context:Context.t
    -> uuid:string
    -> attributes:(string * string) list
    -> unit

  val set_endpoints :
    __context:Context.t -> uuid:string -> endpoints:string list -> unit

  val init : __context:Context.t -> unit

  val set_trace_log_dir : __context:Context.t -> dir:string -> unit

  val set_export_interval : __context:Context.t -> interval:float -> unit

  val set_max_spans : __context:Context.t -> spans:int -> unit

  val set_max_traces : __context:Context.t -> traces:int -> unit

  val set_max_file_size : __context:Context.t -> file_size:int -> unit

  val set_host_id : __context:Context.t -> host_id:string -> unit

  val set_compress_tracing_files : __context:Context.t -> enabled:bool -> unit
end

module Observer : ObserverInterface = struct
  let create ~__context ~uuid ~name_label ~attributes ~endpoints ~enabled =
    debug "Observer.create %s" uuid ;
    Tracing.create ~uuid ~name_label ~attributes ~endpoints ~enabled

  let destroy ~__context ~uuid =
    debug "Observer.destroy %s" uuid ;
    Tracing.destroy ~uuid

  let set_enabled ~__context ~uuid ~enabled =
    debug "Observer.set_enabled %s" uuid ;
    Tracing.set ~uuid ~enabled ()

  let set_attributes ~__context ~uuid ~attributes =
    debug "Observer.set_attributes %s" uuid ;
    Tracing.set ~uuid ~attributes ()

  let set_endpoints ~__context ~uuid ~endpoints =
    debug "Observer.set_endpoints %s" uuid ;
    Tracing.set ~uuid ~endpoints ()

  let init ~__context =
    debug "Observer.init" ;
    ignore @@ Tracing_export.main ()

  let set_trace_log_dir ~__context ~dir =
    debug "Observer.set_trace_log_dir" ;
    Tracing_export.Destination.File.set_trace_log_dir dir

  let set_export_interval ~__context ~interval =
    debug "Observer.set_export_interval" ;
    Tracing_export.set_export_interval interval

  let set_max_spans ~__context ~spans =
    debug "Observer.set_max_spans" ;
    Tracing.Spans.set_max_spans spans

  let set_max_traces ~__context ~traces =
    debug "Observer.set_max_traces" ;
    Tracing.Spans.set_max_traces traces

  let set_max_file_size ~__context ~file_size =
    debug "Observer.set_max_file_size" ;
    Tracing_export.Destination.File.set_max_file_size file_size

  let set_host_id ~__context ~host_id =
    debug "Observer.set_host_id" ;
    Tracing_export.set_host_id host_id

  let set_compress_tracing_files ~__context ~enabled =
    debug "Observer.set_compress_tracing_files" ;
    Tracing_export.Destination.File.set_compress_tracing_files enabled
end

module Xapi_cluster = struct
  module type XAPI_CLUSTER = module type of Cluster_client.LocalClientExn

  let local_client ~__context =
    let module Client = Cluster_interface.LocalAPI (Idl.Exn.GenClient (struct
      let rpc x =
        (* It's ok to not check Daemon.enabled here because we will be using message
           switch to communicate with clusterd *)
        match Context.get_test_clusterd_rpc __context with
        | Some rpc ->
            rpc x
        | None ->
            if !Xcp_client.use_switch then
              Xcp_client.json_switch_rpc Cluster_interface.queue_name x
            else
              Cluster_client.json_http_rpc ~srcstr:"xapi"
                ~dststr:"xapi-clusterd"
                (fun () ->
                  failwith
                    "Can only communicate with xapi-clusterd through \
                     message-switch"
                )
                x
    end)) in
    (module Client : XAPI_CLUSTER)

  module Observer = struct
    let create ~__context ~uuid ~name_label ~attributes ~endpoints ~enabled =
      debug "Observer.create %s" uuid ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.create dbg uuid name_label attributes endpoints enabled

    let destroy ~__context ~uuid =
      debug "Observer.destroy %s" uuid ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.destroy dbg uuid

    let set_enabled ~__context ~uuid ~enabled =
      debug "Observer.set_enabled %s" uuid ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_enabled dbg uuid enabled

    let set_attributes ~__context ~uuid ~attributes =
      debug "Observer.set_attributes %s" uuid ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_attributes dbg uuid attributes

    let set_endpoints ~__context ~uuid ~endpoints =
      debug "Observer.set_endpoints %s" uuid ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_endpoints dbg uuid endpoints

    let init ~__context =
      debug "Observer.init" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.init dbg

    let set_trace_log_dir ~__context ~dir =
      debug "Observer.set_trace_log_dir" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_trace_log_dir dbg dir

    let set_export_interval ~__context ~interval =
      debug "Observer.set_export_interval" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_export_interval dbg interval

    let set_max_spans ~__context ~spans =
      debug "Observer.set_max_spans" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_max_spans dbg spans

    let set_max_traces ~__context ~traces =
      debug "Observer.set_max_traces" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_max_traces dbg traces

    let set_max_file_size ~__context ~file_size =
      debug "Observer.set_max_file_size" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_max_file_size dbg file_size

    let set_host_id ~__context ~host_id =
      debug "Observer.set_host_id" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_host_id dbg host_id

    let set_compress_tracing_files ~__context ~enabled =
      debug "Observer.set_compress_tracing_files" ;
      let module S = (val local_client ~__context : XAPI_CLUSTER) in
      let dbg = Context.string_of_task __context in
      S.Observer.set_compress_tracing_files dbg enabled
  end
end

let default_attributes ~__context ~host ~name_label ~component =
  let pool = Helpers.get_pool ~__context in
  let host_label = Db.Host.get_name_label ~__context ~self:host in
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let pool_uuid = Db.Pool.get_uuid ~__context ~self:pool in
  [
    ("xs.pool.uuid", pool_uuid)
  ; ("xs.host.name", host_label)
  ; ("xs.host.uuid", host_uuid)
  ; ("xs.observer.name", name_label)
  ; ("service.name", to_string component)
  ]

module ObserverConfig = struct
  type t = {
      otel_service_name: string
    ; otel_resource_attributes: (string * string) list
    ; xs_exporter_zipkin_endpoints: string list
    ; xs_exporter_bugtool_endpoint: string option
  }

  let zipkin_endpoints endpoints =
    (*For now, this accepts all endpoints except bugtool. This will change as more endpoints are added*)
    List.filter (fun endpoint -> endpoint <> Tracing.bugtool_name) endpoints

  let rec bugtool_endpoint endpoints =
    match endpoints with
    | x :: _ when x = Tracing.bugtool_name ->
        Some (Tracing_export.Destination.File.get_trace_log_dir ())
    | _ :: t ->
        bugtool_endpoint t
    | [] ->
        None

  let attributes_to_W3CBaggage attrs =
    List.map
      (fun (k, v) -> (k, Tracing.W3CBaggage.Value.(v |> make |> to_string)))
      attrs

  let config_of_observer ~__context ~component ~observer =
    (* In the future this should be updated so that the config is read
       from and updated instead of being regenerated. *)
    let endpoints = Db.Observer.get_endpoints ~__context ~self:observer in
    let host = Helpers.get_localhost ~__context in
    let name_label = Db.Observer.get_name_label ~__context ~self:observer in
    {
      otel_service_name= to_string component
    ; otel_resource_attributes=
        attributes_to_W3CBaggage
          (Db.Observer.get_attributes ~__context ~self:observer
          @ default_attributes ~__context ~host ~name_label ~component
          )
    ; xs_exporter_zipkin_endpoints= zipkin_endpoints endpoints
    ; xs_exporter_bugtool_endpoint= bugtool_endpoint endpoints
    }
end

module type OBSERVER_COMPONENT = sig
  val component : t
end

module Dom0ObserverConfig (ObserverComponent : OBSERVER_COMPONENT) :
  ObserverInterface = struct
  let dir_name = dir_name_of_component ObserverComponent.component

  let env_vars_of_config (config : ObserverConfig.t) =
    Env_record.(
      to_shell_string
        [
          ("OTEL_SERVICE_NAME", str config.otel_service_name)
        ; ( "OTEL_RESOURCE_ATTRIBUTES"
          , (list pair) config.otel_resource_attributes
          )
        ; ( "XS_EXPORTER_ZIPKIN_ENDPOINTS"
          , (list str) config.xs_exporter_zipkin_endpoints
          )
        ; ( "XS_EXPORTER_BUGTOOL_ENDPOINT"
          , option config.xs_exporter_bugtool_endpoint
          )
        ]
    )

  let observer_conf_path_of ~uuid = dir_name // (uuid ^ ".observer.conf")

  let remove_config ~uuid =
    Xapi_stdext_unix.Unixext.unlink_safe (observer_conf_path_of ~uuid)

  let update_config ~__context ~observer ~uuid =
    if Db.Observer.get_enabled ~__context ~self:observer then (
      let observer_config =
        ObserverConfig.config_of_observer ~__context
          ~component:ObserverComponent.component ~observer
      in
      Xapi_stdext_unix.Unixext.mkdir_rec dir_name 0o755 ;
      let file_name = observer_conf_path_of ~uuid in
      Xapi_stdext_unix.Unixext.write_string_to_file file_name
        (env_vars_of_config observer_config)
    ) else
      remove_config ~uuid

  let update_all_configs ~__context ~observer_all =
    List.iter
      (fun observer ->
        let uuid = Db.Observer.get_uuid ~__context ~self:observer in
        update_config ~__context ~observer ~uuid
      )
      observer_all

  let create ~__context ~uuid ~name_label:_ ~attributes:_ ~endpoints:_
      ~enabled:_ =
    let observer = Db.Observer.get_by_uuid ~__context ~uuid in
    update_config ~__context ~observer ~uuid

  let destroy ~__context ~uuid = remove_config ~uuid

  let set_enabled ~__context ~uuid ~enabled:_ =
    let observer = Db.Observer.get_by_uuid ~__context ~uuid in
    update_config ~__context ~observer ~uuid

  let set_attributes ~__context ~uuid ~attributes:_ =
    let observer = Db.Observer.get_by_uuid ~__context ~uuid in
    update_config ~__context ~observer ~uuid

  let set_endpoints ~__context ~uuid ~endpoints:_ =
    let observer = Db.Observer.get_by_uuid ~__context ~uuid in
    update_config ~__context ~observer ~uuid

  let init ~__context =
    let observer_all = Db.Observer.get_all ~__context in
    update_all_configs ~__context ~observer_all

  let set_trace_log_dir ~__context ~dir:_ =
    let observer_all = Db.Observer.get_all ~__context in
    update_all_configs ~__context ~observer_all

  let set_export_interval ~__context:_ ~interval:_ = ()

  let set_max_spans ~__context:_ ~spans:_ = ()

  let set_max_traces ~__context:_ ~traces:_ = ()

  let set_max_file_size ~__context:_ ~file_size:_ = ()

  let set_host_id ~__context:_ ~host_id:_ = ()

  let set_compress_tracing_files ~__context:_ ~enabled:_ = ()
end

module SMObserverConfig = Dom0ObserverConfig (struct let component = SMApi end)

let get_forwarder c =
  let module Forwarder =
    ( val match c with
          | Xapi ->
              (module Observer)
          | Xenopsd ->
              (module Xapi_xenops.Observer)
          | Xapi_clusterd ->
              (module Xapi_cluster.Observer)
          | SMApi ->
              (module SMObserverConfig)
        : ObserverInterface
      )
  in
  (module Forwarder : ObserverInterface)

module ComponentSet = Set.Make (Xapi_observer_components)

let observed_hosts_of ~__context hosts =
  match hosts with [] -> Db.Host.get_all ~__context | hosts -> hosts

let assert_valid_hosts ~__context hosts =
  List.iter
    (fun self ->
      if not (Db.is_valid_ref __context self) then
        raise
          Api_errors.(Server_error (invalid_value, ["host"; Ref.string_of self]))
    )
    hosts

let assert_valid_endpoints ~__context endpoints =
  let is_scheme_enabled scheme =
    let host = Helpers.get_localhost ~__context in
    let is_master = Helpers.is_pool_master ~__context ~host in
    match scheme with
    | Some "http" ->
        (not is_master) || !Xapi_globs.observer_endpoint_http_enabled
    | Some "https" ->
        (not is_master) || !Xapi_globs.observer_endpoint_https_enabled
    | _ ->
        false
  in
  let validate_endpoint = function
    | str when str = Tracing.bugtool_name ->
        true
    | url -> (
      try
        let uri = Uri.of_string url in
        let scheme = Uri.scheme uri in
        let host = Uri.host uri in
        is_scheme_enabled scheme
        &&
        match host with
        | Some host ->
            Result.is_ok (Ipaddr.of_string host)
            || Result.is_ok (Domain_name.of_string host)
        | _ ->
            false
      with _ -> false
    )
  in
  List.iter
    (fun endpoint ->
      if not (validate_endpoint endpoint) then
        raise Api_errors.(Server_error (invalid_value, ["endpoint"; endpoint]))
    )
    endpoints

let assert_valid_attributes attributes =
  List.iter
    (fun (k, v) ->
      if not (Tracing.validate_attribute (k, v)) then
        let kv = Printf.sprintf "%s:%s" k v in
        raise Api_errors.(Server_error (invalid_value, ["attributes"; kv]))
    )
    attributes

let register_component ~__context ~self ~host ~component =
  let name_label = Db.Observer.get_name_label ~__context ~self in
  let attributes =
    default_attributes ~__context ~host ~name_label ~component
    @ Db.Observer.get_attributes ~__context ~self
  in
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let endpoints = Db.Observer.get_endpoints ~__context ~self in
  let enabled = Db.Observer.get_enabled ~__context ~self in
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.create ~__context ~uuid ~name_label ~attributes ~endpoints ~enabled

let register_components ~__context ~self ~host =
  List.iter (fun component ->
      register_component ~__context ~self ~host ~component
  )

let register ~__context ~self =
  Db.Observer.get_components ~__context ~self
  |> List.map of_string
  |> observed_components_of
  |> register_components ~__context ~self

let create ~__context ~name_label ~name_description ~hosts ~attributes
    ~endpoints ~components ~enabled =
  assert_valid_components components ;
  assert_valid_endpoints ~__context endpoints ;
  assert_valid_hosts ~__context hosts ;
  assert_valid_attributes attributes ;
  let ref = Ref.make () in
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  Db.Observer.create ~__context ~ref ~uuid ~name_label ~name_description ~hosts
    ~enabled ~attributes ~endpoints ~components ;
  ref

let unregister_components ~__context ~self =
  let uuid = Db.Observer.get_uuid ~__context ~self in
  List.iter (fun c ->
      let module Forwarder = (val get_forwarder c : ObserverInterface) in
      Forwarder.destroy ~__context ~uuid
  )

let unregister ~__context ~self ~host:_ =
  Db.Observer.get_components ~__context ~self
  |> List.map of_string
  |> observed_components_of
  |> unregister_components ~__context ~self

let destroy ~__context ~self = Db.Observer.destroy ~__context ~self

let set_trace_log_dir ~__context dir component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_trace_log_dir ~__context ~dir

let set_export_interval ~__context interval component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_export_interval ~__context ~interval

let set_max_spans ~__context spans component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_max_spans ~__context ~spans

let set_max_traces ~__context traces component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_max_traces ~__context ~traces

let set_max_file_size ~__context file_size component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_max_file_size ~__context ~file_size

let set_host_id ~__context host_id component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_host_id ~__context ~host_id

let set_compress_tracing_files ~__context enabled component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.set_compress_tracing_files ~__context ~enabled

let init ~__context component =
  let module Forwarder = (val get_forwarder component : ObserverInterface) in
  Forwarder.init ~__context

let load ~__context component =
  let all = Db.Observer.get_all ~__context in
  List.iter
    (fun self ->
      let host = Helpers.get_localhost ~__context in
      let hosts = Db.Observer.get_hosts ~__context ~self in
      if hosts = [] || List.mem host hosts then
        register_component ~__context ~self ~host ~component
    )
    all

(** Called only when there is a component associated with an observer*)
let initialise_observer_component ~__context component =
  load ~__context component

(** Called only once for the same component *)
let initialise_observer_meta ~__context component =
  set_trace_log_dir ~__context !Xapi_globs.trace_log_dir component ;
  set_export_interval ~__context !Xapi_globs.export_interval component ;
  set_max_spans ~__context !Xapi_globs.max_spans component ;
  set_max_traces ~__context !Xapi_globs.max_traces component ;
  set_max_file_size ~__context !Xapi_globs.max_observer_file_size component ;
  set_host_id ~__context (Helpers.get_localhost_uuid ()) component ;
  set_compress_tracing_files ~__context
    !Xapi_globs.compress_tracing_files
    component ;
  init ~__context component

let initialise_observer ~__context component =
  initialise_observer_meta ~__context component ;
  initialise_observer_component ~__context component

let initialise ~__context =
  List.iter (initialise_observer_meta ~__context) (startup_components ()) ;
  Db.Observer.get_all ~__context
  |> List.iter (fun self ->
         Db.Observer.get_components ~__context ~self
         |> List.map of_string
         |> observed_components_of
         |> List.iter (initialise_observer_component ~__context)
     ) ;
  Tracing_export.set_service_name "xapi"

let set_hosts ~__context ~self ~value =
  assert_valid_hosts ~__context value ;
  Db.Observer.set_hosts ~__context ~self ~value

let do_set_op ~__context ~self ~observation_fn ~db_fn =
  let host = Helpers.get_localhost ~__context in
  (* Execute the db_fn before the observation_fn for the forwarders that rely on the DB's data *)
  if Helpers.is_pool_master ~__context ~host then db_fn () ;
  match Db.Observer.get_hosts ~__context ~self with
  | [] ->
      observation_fn ()
  | hosts when List.mem host hosts ->
      observation_fn ()
  | _ ->
      ()

let set_enabled ~__context ~self ~value =
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let observation_fn () =
    List.iter
      (fun c ->
        let module Forwarder = (val get_forwarder c : ObserverInterface) in
        Forwarder.set_enabled ~__context ~uuid ~enabled:value
      )
      (Db.Observer.get_components ~__context ~self
      |> List.map of_string
      |> observed_components_of
      )
  in
  let db_fn () = Db.Observer.set_enabled ~__context ~self ~value in
  do_set_op ~__context ~self ~observation_fn ~db_fn

let set_attributes ~__context ~self ~value =
  assert_valid_attributes value ;
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let host = Helpers.get_localhost ~__context in
  let name_label = Db.Observer.get_name_label ~__context ~self in
  let observation_fn () =
    List.iter
      (fun c ->
        let module Forwarder = (val get_forwarder c : ObserverInterface) in
        Forwarder.set_attributes ~__context ~uuid
          ~attributes:
            (default_attributes ~__context ~host ~name_label ~component:c
            @ value
            )
      )
      (Db.Observer.get_components ~__context ~self
      |> List.map of_string
      |> observed_components_of
      )
  in
  let db_fn () = Db.Observer.set_attributes ~__context ~self ~value in
  do_set_op ~__context ~self ~observation_fn ~db_fn

let set_endpoints ~__context ~self ~value =
  assert_valid_endpoints ~__context value ;
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let observation_fn () =
    List.iter
      (fun c ->
        let module Forwarder = (val get_forwarder c : ObserverInterface) in
        Forwarder.set_endpoints ~__context ~uuid ~endpoints:value
      )
      (Db.Observer.get_components ~__context ~self
      |> List.map of_string
      |> observed_components_of
      )
  in

  let db_fn () = Db.Observer.set_endpoints ~__context ~self ~value in
  do_set_op ~__context ~self ~observation_fn ~db_fn

let set_components ~__context ~self ~value =
  assert_valid_components value ;
  let db_fn () = Db.Observer.set_components ~__context ~self ~value in
  let host = Helpers.get_localhost ~__context in
  let current =
    Db.Observer.get_components ~__context ~self |> List.map of_string
  in
  let future = List.map of_string value in
  let new_components = ComponentSet.of_list (observed_components_of future) in
  let old_components = ComponentSet.of_list (observed_components_of current) in
  let to_add = ComponentSet.diff new_components old_components in
  let to_remove = ComponentSet.diff old_components new_components in
  let observation_fn () =
    ComponentSet.iter
      (fun unreg -> unregister_components ~__context ~self [unreg])
      to_remove ;
    ComponentSet.iter
      (fun reg -> register_components ~__context ~self ~host [reg])
      to_add
  in
  do_set_op ~__context ~self ~observation_fn ~db_fn
