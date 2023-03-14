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
let create ~__context ?(hosts = []) ?(tags = []) ?(endpoints = ["bugtool"])
    ?(components = []) ?(filters = []) ?(processors = []) ?(status = false)
    ~name_label () : unit =
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
  | Some self ->
      let tags =
        ("host", host_uuid)
        :: ("host name", host_label)
        :: Db.Tracing.get_tags ~__context ~self
      in
      let endpoints = Db.Tracing.get_endpoints ~__context ~self in
      let processors = Db.Tracing.get_processors ~__context ~self in
      let filters = Db.Tracing.get_filters ~__context ~self in
      let enabled = Db.Tracing.get_status ~__context ~self in
      Tracing.TracerProviders.set_default ~tags ~endpoints ~processors ~filters
        ~enabled
  | None ->
      let tags =
        [("pool", pool_uuid); ("host", host_uuid); ("host name", host_label)]
      in
      let endpoints = !Xapi_globs.tracing_default_endpoints in
      let processors = !Xapi_globs.tracing_default_processors in
      let filters = !Xapi_globs.tracing_default_filters in
      if is_master then
        create ~__context ~tags:[("pool", pool_uuid)] ~endpoints ~processors
          ~filters ~status:false ~name_label:"default" () ;
      Tracing.TracerProviders.set_default ~tags ~endpoints ~processors ~filters
        ~enabled:false

let set_status ~__context ~self ~status =
  let name_label = Db.Tracing.get_name_label ~__context ~self in
  Tracing.TracerProviders.set ~name_label ~status ()

let set_tags ~__context ~self ~tags =
  let name_label = Db.Tracing.get_name_label ~__context ~self in
  let tags =
    let host = Helpers.get_localhost ~__context in
    let host_uuid = Db.Host.get_uuid ~__context ~self:host in
    let host_label = Db.Host.get_name_label ~__context ~self:host in
    ("host", host_uuid) :: ("host name", host_label) :: tags
  in
  Tracing.TracerProviders.set ~name_label ~tags ()

let set_endpoints ~__context ~self ~endpoints =
  let name_label = Db.Tracing.get_name_label ~__context ~self in
  Tracing.TracerProviders.set ~name_label ~endpoints ()

let set_components ~__context ~self:_ ~components:_ = ()

(* Will implement later, this function will set / unset providers on veraious components *)
let set_filters ~__context ~self ~filters =
  let name_label = Db.Tracing.get_name_label ~__context ~self in
  Tracing.TracerProviders.set ~name_label ~filters ()

let set_processors ~__context ~self ~processors =
  let name_label = Db.Tracing.get_name_label ~__context ~self in
  Tracing.TracerProviders.set ~name_label ~processors ()
