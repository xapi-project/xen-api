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

(* This module implements methods for the PVS_site class *)

module D = Debug.Make(struct let name = "xapi_pvs_site" end)
open D
module E = Api_errors

let api_error msg xs = raise (E.Server_error (msg, xs))

let introduce ~__context ~name =
  Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
  let pvs_site = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.PVS_site.create ~__context
    ~ref:pvs_site ~uuid ~name ~cache_storage:[];
  pvs_site

let forget ~__context ~self =
  let open Db_filter_types in
  (* Check there are no running proxies. *)
  let running_proxies = Pvs_proxy_control.get_running_proxies ~__context ~site:self in
  if running_proxies <> [] then
    raise Api_errors.(Server_error
                        (pvs_site_contains_running_proxies,
                         List.map Ref.string_of running_proxies));
  (* Check there are no servers. *)
  let servers = Db.PVS_site.get_servers ~__context ~self in
  if servers <> [] then raise Api_errors.(Server_error
                                            (pvs_site_contains_servers, List.map Ref.string_of servers));
  List.iter
    (fun sr -> Xapi_pvs_cache.on_sr_remove ~__context ~sr)
    (Db.PVS_site.get_cache_storage ~__context ~self);
  Db.PVS_site.destroy ~__context ~self

(** set the name of [self] *)
let set_name ~__context ~self ~value =
  let px = Pvs_proxy_control.get_running_proxies ~__context ~site:self in
  if px <> [] then
    api_error E.pvs_site_contains_running_proxies (List.map Ref.string_of px)
  else
    Db.PVS_site.set_name ~__context ~self ~value

(** [sr_is_in_use] is true, if the [sr] is currently in use. *)
let sr_is_in_use ~__context ~self sr =
  Pvs_proxy_control.get_running_proxies ~__context ~site:self
  |> List.map (fun px -> Db.PVS_proxy.get_cache_SR ~__context ~self:px)
  |> List.mem sr

(** [add_cache_storage] adds an SR for caching *)
let add_cache_storage ~__context ~self ~value =
  let str ref = Ref.string_of ref in
  let cache = Db.PVS_site.get_cache_storage ~__context ~self in
  if List.mem value cache then
    api_error E.pvs_site_sr_already_added [str self; str value]
  else
    Db.PVS_site.add_cache_storage ~__context ~self ~value

(** [remove_cache_storage] remove an SR unless it is used *)
let remove_cache_storage ~__context ~self ~value =
  let cache = Db.PVS_site.get_cache_storage ~__context ~self in
  let str ref = Ref.string_of ref in
  if not @@ List.mem value cache then
    api_error E.sr_not_in_pvs_site [str self; str value]
  else if sr_is_in_use ~__context ~self value then
    api_error E.pvs_site_sr_is_in_use [str self; str value]
  else begin
    Xapi_pvs_cache.on_sr_remove ~__context ~sr:value;
    Db.PVS_site.remove_cache_storage ~__context ~self ~value
  end

