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

let introduce ~__context ~name_label ~name_description ~pVS_uuid =
  Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
  Helpers.assert_using_vswitch ~__context;
  let pvs_site = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.PVS_site.create ~__context
    ~ref:pvs_site ~uuid ~name_label ~name_description ~pVS_uuid ~cache_storage:[];
  pvs_site

let cleanup_storage __context self =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Db.PVS_site.get_cache_storage ~__context ~self
      |> List.iter (fun self -> Client.Client.PVS_cache_storage.destroy ~rpc ~session_id ~self)
    )

let forget_internal ~__context ~self ~cleanup_storage =
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
  cleanup_storage __context self;
  Db.PVS_site.destroy ~__context ~self

let forget = forget_internal ~cleanup_storage

(** set the PVS UUID of [self] *)
let set_PVS_uuid ~__context ~self ~value =
  let px = Pvs_proxy_control.get_running_proxies ~__context ~site:self in
  if px <> [] then
    api_error E.pvs_site_contains_running_proxies (List.map Ref.string_of px)
  else
    Db.PVS_site.set_PVS_uuid ~__context ~self ~value

