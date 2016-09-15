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

let create ~__context ~site ~vIF =
  Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
  Helpers.assert_using_vswitch ~__context;
  let expr = Db_filter_types.(Eq (Field "VIF", Literal (Ref.string_of vIF))) in
  let proxies = Db.PVS_proxy.get_refs_where ~__context ~expr in
  if List.length proxies > 0
  then raise Api_errors.(Server_error (pvs_proxy_already_present, List.map Ref.string_of proxies));

  Helpers.assert_is_valid_ref ~__context ~name:"site" ~ref:site;
  Helpers.assert_is_valid_ref ~__context ~name:"VIF" ~ref:vIF;
  let device = Db.VIF.get_device ~__context ~self:vIF in
  if device <> "0"
  then raise Api_errors.(Server_error (invalid_device, [device]));

  let pvs_proxy = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.PVS_proxy.create ~__context
    ~ref:pvs_proxy ~uuid ~site ~vIF ~currently_attached:false ~status:`stopped;
  if Db.VIF.get_currently_attached ~__context ~self:vIF then begin
    if Pvs_proxy_control.start_proxy ~__context vIF pvs_proxy then
      try
        (* This will cause Pvs_proxy.currently_attached to become true. *)
        Xapi_xenops.vif_set_pvs_proxy ~__context ~self:vIF true
      with e -> (
          let body = Printf.sprintf "Failed to setup PVS-proxy %s for VIF %s due to an internal error"
              uuid (Db.VIF.get_uuid ~__context ~self:vIF) in
          let (name, priority) = Api_messages.pvs_proxy_setup_failed in
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              ignore(Client.Client.Message.create ~rpc ~session_id ~name ~priority ~cls:`PVS_proxy ~obj_uuid:uuid ~body));
          error "Unable to setup PVS proxy for vif %s: %s." (Ref.string_of vIF) (ExnHelper.string_of_exn e)
        )
  end;
  pvs_proxy

let destroy ~__context ~self =
  if Db.PVS_proxy.get_currently_attached ~__context ~self then begin
    let vif = Db.PVS_proxy.get_VIF ~__context ~self in
    Xapi_xenops.vif_set_pvs_proxy ~__context ~self:vif false;
    Pvs_proxy_control.stop_proxy ~__context vif self
  end;
  Db.PVS_proxy.destroy ~__context ~self
