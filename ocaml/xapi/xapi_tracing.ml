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

let set_trace_log_dir ~__context _dir = ()

let set_export_timeout ~__context _timeout = ()

let load ~__context ~host =
  let host_label = Db.Host.get_name_label ~__context ~self:host in
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let all = Db.Tracing.get_all ~__context in
  List.iter
    (fun self ->
      let _name_label = Db.Tracing.get_name_label ~__context ~self in
      let _status = Db.Tracing.get_status ~__context ~self in
      let _tags =
        ("host", host_uuid)
        :: ("host name", host_label)
        :: Db.Tracing.get_tags ~__context ~self
      in
      let _endpoints = Db.Tracing.get_endpoints ~__context ~self in
      let _filters = Db.Tracing.get_filters ~__context ~self in
      let _processors = Db.Tracing.get_processors ~__context ~self in
      ()
    )
    all

let create_record ~__context ?(hosts = []) ?(tags = [])
    ?(endpoints = ["bugtool"]) ?(components = []) ?(filters = [])
    ?(processors = []) ?(status = false) ~name_label () : unit =
  let tracing = Ref.make () in
  Db.Tracing.create ~__context
    ~uuid:(Uuidx.to_string (Uuidx.make ()))
    ~ref:tracing ~hosts ~name_label ~tags ~endpoints ~components ~filters
    ~processors ~status

let set_default ~__context ~pool ~host =
  (* Called on each host *)
  let pool_uuid = Db.Pool.get_uuid ~__context ~self:pool in
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let host_label = Db.Host.get_name_label ~__context ~self:host in
  let default =
    List.find_opt
      (fun self -> Db.Tracing.get_name_label ~__context ~self = "default")
      (Db.Tracing.get_all ~__context)
  in
  let is_master = Helpers.is_pool_master ~__context ~host in
  match default with
  | Some _ ->
      ()
  | None ->
      let _tags =
        [("pool", pool_uuid); ("host", host_uuid); ("host name", host_label)]
      in
      let endpoints = !Xapi_globs.tracing_default_endpoints in
      let processors = !Xapi_globs.tracing_default_processors in
      let filters = !Xapi_globs.tracing_default_filters in
      if is_master then
        create_record ~__context ~tags:[("pool", pool_uuid)] ~endpoints
          ~processors ~filters ~status:false ~name_label:"default" ()

let initialise ~__context =
  let pool = Helpers.get_pool ~__context in
  let host = Helpers.get_localhost ~__context in
  load ~__context ~host ;
  set_default ~__context ~pool ~host ;
  set_trace_log_dir ~__context !Xapi_globs.trace_log_dir ;
  set_export_timeout ~__context !Xapi_globs.export_timeout

let create ~__context ~name_label:_ ~hosts:_ ~status:_ ~tags:_ ~endpoints:_
    ~components:_ ~filters:_ ~processors:_ =
  ()

let destroy ~__context ~self:_ = ()

let set_hosts ~__context ~self:_ ~hosts:_ = ()

let set_status ~__context ~self:_ ~status:_ = ()

let set_tags ~__context ~self:_ ~tags:_ = ()

let set_endpoints ~__context ~self:_ ~endpoints:_ = ()

let set_components ~__context ~self:_ ~components:_ = ()

(* Will implement later, this function will set / unset providers on veraious components *)
let set_filters ~__context ~self:_ ~filters:_ = ()

let set_processors ~__context ~self:_ ~processors:_ = ()
