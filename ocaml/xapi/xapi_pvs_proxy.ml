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

let create ~__context ~site ~vIF ~prepopulate =
  Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
  Helpers.assert_is_valid_ref ~__context ~name:"site" ~ref:site;
  Helpers.assert_is_valid_ref ~__context ~name:"VIF" ~ref:vIF;
  let device = Db.VIF.get_device ~__context ~self:vIF in
  if device <> "0"
  then raise Api_errors.(Server_error (invalid_device, [device]));
  let pvs_proxy = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.PVS_proxy.create ~__context
    ~ref:pvs_proxy ~uuid ~site ~vIF ~prepopulate ~currently_attached:false ~cache_SR:Ref.null;
  if Db.VIF.get_currently_attached ~__context ~self:vIF then begin
    Pvs_proxy_control.start_proxy ~__context vIF pvs_proxy;
    if Db.PVS_proxy.get_currently_attached ~__context ~self:pvs_proxy then
      Xapi_xenops.vif_set_pvs_proxy ~__context ~self:vIF true
  end;
  pvs_proxy

let destroy ~__context ~self =
  if Db.PVS_proxy.get_currently_attached ~__context ~self then begin
    let vif = Db.PVS_proxy.get_VIF ~__context ~self in
    Xapi_xenops.vif_set_pvs_proxy ~__context ~self:vif false;
    Pvs_proxy_control.stop_proxy ~__context vif self
  end;
  Db.PVS_proxy.destroy ~__context ~self

let set_prepopulate ~__context ~self ~value =
  not_implemented "PVS_proxy.set_prepopulate"
