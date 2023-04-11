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
let supported_components = ["xapi"]

let observed_hosts_of ~__context hosts =
  match hosts with [] -> Db.Host.get_all ~__context | hosts -> hosts

let assert_valid_hosts ~__context hosts =
  List.iter
    (fun self ->
      if not (Db.is_valid_ref __context self) then
        raise
          Api_errors.(
            Server_error (invalid_value, ["host"; Ref.string_of self])
          )
    )
    hosts

let assert_valid_components components =
  List.iter
    (fun component ->
      if not (List.mem component supported_components) then
        raise
          Api_errors.(Server_error (invalid_value, ["component"; component]))
    )
    components

let assert_valid_endpoints endpoints =
  let validate_endpoint = function
    | "bugtool" ->
        true
    | url -> (
      try
        let _ = Uri.of_string url in
        true
      with _ -> false
    )
  in
  List.iter
    (fun endpoint ->
      if not (validate_endpoint endpoint) then
        raise Api_errors.(Server_error (invalid_value, ["endpoint"; endpoint]))
    )
    endpoints

let register ~__context ~self ~host =
  let pool = Helpers.get_pool ~__context in
  let host_label = Db.Host.get_name_label ~__context ~self:host in
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let pool_uuid = Db.Pool.get_uuid ~__context ~self:pool in
  let name_label = Db.Observer.get_name_label ~__context ~self in
  let attributes =
    ("xs.pool.uuid", pool_uuid)
    :: ("xs.host.name", host_label)
    :: ("xs.host.uuid", host_uuid)
    :: ("xs.observer.name", name_label)
    :: List.map
         (fun (k, v) -> ("user." ^ k, v))
         (Db.Observer.get_attributes ~__context ~self)
  in
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let endpoints = Db.Observer.get_endpoints ~__context ~self in
  let enabled = Db.Observer.get_enabled ~__context ~self in
  ()

let create ~__context ~name_label ~name_description ~hosts ~attributes
    ~endpoints ~components ~enabled =
  assert_valid_components components ;
  assert_valid_endpoints endpoints ;
  assert_valid_hosts ~__context hosts ;
  let ref = Ref.make () in
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  Db.Observer.create ~__context ~ref ~uuid ~name_label ~name_description ~hosts
    ~enabled ~attributes ~endpoints ~components ;
  ref

let unregister ~__context ~self ~host:_ =
  let uuid = Db.Observer.get_uuid ~__context ~self in
  ()

let destroy ~__context ~self = Db.Observer.destroy ~__context ~self

let set_trace_log_dir dir = ()

let set_export_interval interval = ()

let set_max_spans spans = ()

let set_max_traces traces = ()

let set_host_id host_id = ()

let init () = ()

let load ~__context =
  let all = Db.Observer.get_all ~__context in
  List.iter
    (fun self ->
      let host = Helpers.get_localhost ~__context in
      let hosts = Db.Observer.get_hosts ~__context ~self in
      if hosts = [] || List.mem host hosts then
        register ~__context ~self ~host
    )
    all

let initialise ~__context =
  init () ;
  load ~__context ;
  set_trace_log_dir !Xapi_globs.trace_log_dir ;
  set_export_interval !Xapi_globs.export_interval ;
  set_max_spans !Xapi_globs.max_spans ;
  set_max_traces !Xapi_globs.max_traces ;
  set_host_id (Helpers.get_localhost_uuid ())

let set_hosts ~__context ~self ~value =
  assert_valid_hosts ~__context value ;
  Db.Observer.set_hosts ~__context ~self ~value

let do_set_op ~__context ~self ~observation_fn ~db_fn =
  let host = Helpers.get_localhost ~__context in
  ( match Db.Observer.get_hosts ~__context ~self with
  | [] ->
      observation_fn ()
  | hosts when List.mem host hosts ->
      observation_fn ()
  | _ ->
      ()
  ) ;
  if Helpers.is_pool_master ~__context ~host then db_fn ()

let set_enabled ~__context ~self ~value =
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let observation_fn () = () in
  let db_fn () = Db.Observer.set_enabled ~__context ~self ~value in
  do_set_op ~__context ~self ~observation_fn ~db_fn

let set_attributes ~__context ~self ~value =
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let observation_fn () = () in
  let db_fn () = Db.Observer.set_attributes ~__context ~self ~value in
  do_set_op ~__context ~self ~observation_fn ~db_fn

let set_endpoints ~__context ~self ~value =
  assert_valid_endpoints value ;
  let uuid = Db.Observer.get_uuid ~__context ~self in
  let observation_fn () = () in
  let db_fn () = Db.Observer.set_endpoints ~__context ~self ~value in
  do_set_op ~__context ~self ~observation_fn ~db_fn

let set_components ~__context ~self ~value =
  assert_valid_components value ;
  Db.Observer.set_components ~__context ~self ~value
(* Will implement later, this function will set / unset providers on veraious components *)
