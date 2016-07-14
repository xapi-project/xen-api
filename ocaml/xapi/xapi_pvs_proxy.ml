(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

(* This module implements methods for the PVS_proxy class *)

open Stdext

module D = Debug.Make(struct let name = "xapi_pvs_proxy" end)
open D

let not_implemented x =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ x ]))

let start ~__context vif proxy =
  if not (Db.PVS_proxy.get_currently_attached ~__context ~self:proxy) then begin
    try
      let host = Helpers.get_localhost ~__context in
      let farm = Db.PVS_proxy.get_farm ~__context ~self:proxy in
      let sr, vdi = Xapi_pvs_cache.find_or_create_cache_vdi ~__context ~host ~farm in
      Xapi_pvs_farm.update_farm_on_localhost ~__context ~self:farm ~vdi ~starting_proxies:[vif, proxy] ();
      Db.PVS_proxy.set_currently_attached ~__context ~self:proxy ~value:true;
      Db.PVS_proxy.set_cache_SR ~__context ~self:proxy ~value:sr
    with e ->
      let reason =
        match e with
        | Xapi_pvs_cache.No_cache_sr_available -> "no PVS cache SR available"
        | Network_interface.PVS_proxy_connection_error -> "unable to connect to PVS proxy daemon"
        | _ -> Printf.sprintf "unknown error (%s)" (Printexc.to_string e)
      in
      warn "Unable to enable PVS proxy for VIF %s: %s. Continuing with proxy unattached." (Ref.string_of vif) reason
  end

let stop ~__context vif proxy =
  if Db.PVS_proxy.get_currently_attached ~__context ~self:proxy then begin
    try
      let farm = Db.PVS_proxy.get_farm ~__context ~self:proxy in
      let sr = Db.PVS_proxy.get_cache_SR ~__context ~self:proxy in
      let vdi = Xapi_pvs_cache.find_cache_vdi ~__context ~sr in
      Xapi_pvs_farm.update_farm_on_localhost ~__context ~self:farm ~vdi ~stopping_proxies:[vif, proxy] ();
      Db.PVS_proxy.set_currently_attached ~__context ~self:proxy ~value:false;
      Db.PVS_proxy.set_cache_SR ~__context ~self:proxy ~value:Ref.null
    with e ->
      let reason =
        match e with
        | Xapi_pvs_cache.No_cache_vdi_present -> "no PVS cache VDI found"
        | Network_interface.PVS_proxy_connection_error -> "unable to connect to PVS proxy daemon"
        | _ -> Printf.sprintf "unknown error (%s)" (Printexc.to_string e)
      in
      error "Unable to disable PVS proxy for VIF %s: %s." (Ref.string_of vif) reason
  end

let find_proxy_for_vif ~__context ~vif =
  let open Db_filter_types in
  let proxies = Db.PVS_proxy.get_refs_where ~__context
      ~expr:(Eq (Field "VIF", Literal (Ref.string_of vif))) in
  match proxies with
  | [] -> None
  | proxy :: _ -> Some proxy

let maybe_start_proxy_for_vif ~__context ~vif =
  Opt.iter
    (start ~__context vif)
    (find_proxy_for_vif ~__context ~vif)

let maybe_stop_proxy_for_vif ~__context ~vif =
  Opt.iter
    (stop ~__context vif)
    (find_proxy_for_vif ~__context ~vif)

let make_xenstore_keys_for_vif ~__context ~vif =
  match find_proxy_for_vif ~__context ~vif with
  | None -> []
  | Some proxy ->
    let network = Db.VIF.get_network ~__context ~self:vif in
    let bridge = Db.Network.get_bridge ~__context ~self:network in
    let farm = Db.PVS_proxy.get_farm ~__context ~self:proxy in
    let servers = Db.PVS_farm.get_servers ~__context ~self:farm in
    let server_keys =
      List.mapi (fun i server ->
          let open Printf in
          let rc = Db.PVS_server.get_record ~__context ~self:server in
          [
            sprintf "pvs-server-%d-addresses" i, String.concat "," rc.API.pVS_server_addresses;
            sprintf "pvs-server-%d-ports" i, sprintf "%Ld-%Ld" rc.API.pVS_server_first_port rc.API.pVS_server_last_port;
          ]
        ) servers
      |> List.flatten
    in
    ("pvs-interface", Xapi_pvs_farm.proxy_port_name bridge) ::
    ("pvs-server-num", string_of_int (List.length servers)) ::
    server_keys

let create ~__context ~farm ~vIF ~prepopulate =
  Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
  Helpers.assert_is_valid_ref ~__context ~name:"farm" ~ref:farm;
  Helpers.assert_is_valid_ref ~__context ~name:"VIF" ~ref:vIF;
  let device = Db.VIF.get_device ~__context ~self:vIF in
  if device <> "0"
  then raise Api_errors.(Server_error (invalid_device, [device]));
  let pvs_proxy = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.PVS_proxy.create ~__context
    ~ref:pvs_proxy ~uuid ~farm ~vIF ~prepopulate 
    ~currently_attached:false ~cache_SR:Ref.null;
  if Db.VIF.get_currently_attached ~__context ~self:vIF then
    start ~__context vIF pvs_proxy;
  pvs_proxy

let destroy ~__context ~self =
  let vIF = Db.PVS_proxy.get_VIF ~__context ~self in
  if Db.VIF.get_currently_attached ~__context ~self:vIF then
    stop ~__context vIF self;
  Db.PVS_proxy.destroy ~__context ~self

let set_prepopulate ~__context ~self ~value =
  not_implemented "PVS_proxy.set_prepopulate"
