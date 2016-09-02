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

module E = Api_errors

let api_error msg xs = raise (E.Server_error (msg, xs))
let str ref = Ref.string_of ref

let create ~__context ~site ~sR ~size =
  let caches = Db.PVS_site.get_cache_storage ~__context ~self:site in
  let srs = List.map (fun pcs -> Db.PVS_cache_storage.get_SR ~__context ~self:pcs) caches in

  if List.mem sR srs then
    api_error E.pvs_site_sr_already_added [str site; str sR]
  else begin
    let cache_storage = Ref.make () in
    let uuid = Uuidm.to_string (Uuidm.create `V4) in
    Db.PVS_cache_storage.create ~__context ~ref:cache_storage ~uuid ~site ~sR ~host_vdis:[] ~size;
    cache_storage
  end


(** [sr_is_in_use] is true, if the [sr] is currently in use. *)
let sr_is_in_use ~__context site sr =
  Pvs_proxy_control.get_running_proxies ~__context ~site
  |> List.map (fun px -> Db.PVS_proxy.get_cache_SR ~__context ~self:px)
  |> List.mem sr


let destroy ~__context ~self =
  let site = Db.PVS_cache_storage.get_site ~__context ~self in
  let sr = Db.PVS_cache_storage.get_SR ~__context ~self in
  if sr_is_in_use ~__context site sr then
    api_error E.pvs_site_sr_is_in_use [str site; str sr]
  else begin
    Xapi_pvs_cache.on_sr_remove ~__context ~sr ~site;
    Db.PVS_cache_storage.destroy ~__context ~self
  end
