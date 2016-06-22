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

let not_implemented x =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ x ]))

let proxy_port_name bridge =
  "pvs-" ^ bridge

let start ~__context vif proxy =
  if not (Db.PVS_proxy.get_currently_attached ~__context ~self:proxy) then begin
    let dbg = Context.string_of_task __context in
    let network = Db.VIF.get_network ~__context ~self:vif in
    let bridge = Db.Network.get_bridge ~__context ~self:network in
    let name = proxy_port_name bridge in
    Network.Net.Bridge.add_port dbg ~bridge ~name ~kind:Network_interface.PVS_proxy ~interfaces:[] ();
    Db.PVS_proxy.set_currently_attached ~__context ~self:proxy ~value:true
  end

let stop ~__context vif proxy =
  if Db.PVS_proxy.get_currently_attached ~__context ~self:proxy then begin
    let dbg = Context.string_of_task __context in
    let network = Db.VIF.get_network ~__context ~self:vif in
    let bridge = Db.Network.get_bridge ~__context ~self:network in
    let name = proxy_port_name bridge in
    Network.Net.Bridge.remove_port dbg ~bridge ~name;
    Db.PVS_proxy.set_currently_attached ~__context ~self:proxy ~value:false
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

let create ~__context ~farm ~vIF ~prepopulate =
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
