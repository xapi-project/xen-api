(*
 * Copyright (C) 2006-2016 Citrix Systems Inc.
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

open Network

module D = Debug.Make (struct let name = "xapi_sdn_controller" end)

open D

let db_introduce ~__context ~protocol ~address ~port =
  if Db.SDN_controller.get_all ~__context <> [] then
    raise
      (Api_errors.Server_error
         ( Api_errors.operation_not_allowed
         , ["SDN controller has been configured. Please forget it first."]
         )
      ) ;
  if protocol = `pssl then (
    if address <> "" then
      raise
        (Api_errors.Server_error (Api_errors.invalid_value, ["address"; address])
        ) ;
    if port <> 0L then
      raise
        (Api_errors.Server_error
           (Api_errors.invalid_value, ["port"; Int64.to_string port])
        )
  ) ;
  if protocol = `ssl then
    if address <> "" then
      Helpers.assert_is_valid_ip `ipv4 "address" address
    else
      raise
        (Api_errors.Server_error (Api_errors.invalid_value, ["address"; address])
        ) ;
  if port <> 0L then
    Helpers.assert_is_valid_tcp_udp_port (Int64.to_int port) "port" ;
  let tcpport = if protocol = `ssl && port = 0L then 6632L else port in
  let r = Ref.make () and uuid = Uuid.make_uuid () in
  Db.SDN_controller.create ~__context ~ref:r ~uuid:(Uuid.to_string uuid)
    ~protocol ~address ~port:tcpport ~local_ports:[] ;
  r

let introduce ~__context ~protocol ~address ~port =
  let dbg = Context.string_of_task __context in
  match Net.Bridge.get_kind dbg () with
  | Network_interface.Openvswitch ->
      let r = db_introduce ~__context ~protocol ~address ~port in
      List.iter
        (fun host -> Helpers.update_vswitch_controller ~__context ~host)
        (Db.Host.get_all ~__context) ;
      r
  | _ ->
      raise
        (Api_errors.Server_error
           ( Api_errors.operation_not_allowed
           , ["host not configured for vswitch operation"]
           )
        )

let forget ~__context ~self =
  let dbg = Context.string_of_task __context in
  Db.SDN_controller.destroy ~__context ~self ;
  if Net.Bridge.get_kind dbg () = Network_interface.Openvswitch then
    List.iter
      (fun host -> Helpers.update_vswitch_controller ~__context ~host)
      (Db.Host.get_all ~__context)

let add_local_port ~__context ~self ~protocol ~port =
  Helpers.assert_is_valid_tcp_udp_port (Int64.to_int port) "port" ;
  let port_str = Int64.to_string port in
  let protocol_str = Record_util.sdn_port_protocol_to_string protocol in
  let value = protocol_str ^ ":" ^ port_str in
  Db.SDN_controller.add_local_ports ~__context ~self ~value ;
  List.iter
    (fun host -> Helpers.update_vswitch_controller ~__context ~host)
    (Db.Host.get_all ~__context)

let remove_local_port ~__context ~self ~protocol ~port =
  Helpers.assert_is_valid_tcp_udp_port (Int64.to_int port) "port" ;
  let port_str = Int64.to_string port in
  let protocol_str = Record_util.sdn_port_protocol_to_string protocol in
  let value = protocol_str ^ ":" ^ port_str in
  Db.SDN_controller.remove_local_ports ~__context ~self ~value ;
  List.iter
    (fun host -> Helpers.update_vswitch_controller ~__context ~host)
    (Db.Host.get_all ~__context)
