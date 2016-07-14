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

(* This module implements methods for the PVS_farm class *)

module D = Debug.Make(struct let name = "xapi_pvs_farm" end)
open D
module E = Api_errors

open Stdext
open Listext

let proxy_port_name bridge =
  "pvs-" ^ bridge

let api_error msg xs = raise (E.Server_error (msg, xs))

let introduce ~__context ~name =
  Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
  let pvs_farm = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.PVS_farm.create ~__context
    ~ref:pvs_farm ~uuid ~name ~cache_storage:[];
  pvs_farm

(** [proxies] returns all currently attached proxies *)
let proxies ~__context ~self =
  let open Db_filter_types in
  Db.PVS_proxy.get_refs_where ~__context
    ~expr:
      (And
         ((Eq (Field "farm", Literal (Ref.string_of self)))
         ,(Eq (Field "currently_attached", Literal "true"))
         ))

let metadata_of_farm ~__context ~self ~vdi ~proxies =
  let open Network_interface in
  let farm_rc = Db.PVS_farm.get_record ~__context ~self in
  let servers =
    List.map (fun self ->
        let rc = Db.PVS_server.get_record ~__context ~self in
        PVS_proxy.Server.{
          uuid = rc.API.pVS_server_uuid;
          addresses = List.map Unix.inet_addr_of_string rc.API.pVS_server_addresses;
          first_port = Int64.to_int rc.API.pVS_server_first_port;
          last_port = Int64.to_int rc.API.pVS_server_last_port;
        }
      ) farm_rc.API.pVS_farm_servers
  in
  let clients =
    List.map (fun (vif, proxy) ->
        let rc = Db.VIF.get_record ~__context ~self:vif in
        let prepopulate = Db.PVS_proxy.get_prepopulate ~__context ~self:proxy in
        PVS_proxy.Client.{
          uuid = rc.API.vIF_uuid;
          mac = rc.API.vIF_MAC;
          interface = proxy_port_name (Db.Network.get_bridge ~__context ~self:rc.API.vIF_network);
          prepopulate;
        }
      ) proxies
  in
  let vdi = Db.VDI.get_uuid ~__context ~self:vdi in
  PVS_proxy.{
    farm_uuid = farm_rc.API.pVS_farm_uuid;
    farm_name = farm_rc.API.pVS_farm_name;
    servers;
    clients;
    vdi;
  }

let update_farm_on_localhost ~__context ~self ~vdi ?(starting_proxies=[]) ?(stopping_proxies=[]) () =
  debug "Updating PVS farm %s. Starting proxies: [%s]. Stopping proxies: [%s]"
    (Ref.string_of self)
    (String.concat ", " (List.map (fun (_, p) -> Ref.string_of p) starting_proxies))
    (String.concat ", " (List.map (fun (_, p) -> Ref.string_of p) stopping_proxies));

  let open Network_interface.PVS_proxy in
  let dbg = Context.string_of_task __context in

  (* Ensure that OVS ports for the proxy daemon exist for starting proxies *)
  List.iter
    (fun (vif, _) ->
       let network = Db.VIF.get_network ~__context ~self:vif in
       let bridge = Db.Network.get_bridge ~__context ~self:network in
       let port_name = proxy_port_name bridge in
       Network.Net.Bridge.add_port dbg ~bridge ~name:port_name ~kind:Network_interface.PVS_proxy ~interfaces:[] ()
    )
    starting_proxies;

  let running_proxies = proxies ~__context ~self in
  let localhost = Helpers.get_localhost ~__context in
  let local_running_proxies = List.filter_map (fun proxy ->
      let vif = Db.PVS_proxy.get_VIF ~__context ~self:proxy in
      let vm = Db.VIF.get_VM ~__context ~self:vif in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        Some (vif, proxy)
      else
        None
    ) running_proxies in
  let proxies = starting_proxies @ (List.set_difference local_running_proxies stopping_proxies) in
  let proxy_config = metadata_of_farm ~__context ~self ~vdi ~proxies in
  if proxy_config.clients <> [] then
    Network.Net.PVS_proxy.configure_farm dbg proxy_config
  else
    let uuid = Db.PVS_farm.get_uuid ~__context ~self in
    Network.Net.PVS_proxy.remove_farm dbg uuid;

    (* Ensure that OVS ports for the proxy daemon are removed if they are no longer used *)
    List.iter
      (fun (vif, _) ->
         let network = Db.VIF.get_network ~__context ~self:vif in
         let bridge = Db.Network.get_bridge ~__context ~self:network in
         let port_name = proxy_port_name bridge in
         let active_ports = List.map (fun client -> client.Client.interface) proxy_config.clients in
         if not (List.mem port_name active_ports) then
           Network.Net.Bridge.remove_port dbg ~bridge ~name:port_name
      )
      stopping_proxies

let forget ~__context ~self =
  let open Db_filter_types in
  (* Check there are no running proxies. *)
  let running_proxies = proxies ~__context ~self in
  if running_proxies <> [] then
    raise Api_errors.(Server_error
                        (pvs_farm_contains_running_proxies,
                         List.map Ref.string_of running_proxies));
  (* Check there are no servers. *)
  let servers = Db.PVS_farm.get_servers ~__context ~self in
  if servers <> [] then raise Api_errors.(Server_error
                                            (pvs_farm_contains_servers, List.map Ref.string_of servers));
  List.iter
    (fun sr -> Xapi_pvs_cache.on_sr_remove ~__context ~sr)
    (Db.PVS_farm.get_cache_storage ~__context ~self);
  Db.PVS_farm.destroy ~__context ~self

(** set the name of [self] *)
let set_name ~__context ~self ~value =
  let px = proxies ~__context ~self in
  if px <> [] then
    api_error E.pvs_farm_contains_running_proxies (List.map Ref.string_of px)
  else
    Db.PVS_farm.set_name ~__context ~self ~value

(** [sr_is_in_use] is true, if the [sr] is currently in use. *)
let sr_is_in_use ~__context ~self sr =
  proxies ~__context ~self
  |> List.map (fun px -> Db.PVS_proxy.get_cache_SR ~__context ~self:px)
  |> List.mem sr

(** [add_cache_storage] adds an SR for caching *)
let add_cache_storage ~__context ~self ~value =
  let str ref = Ref.string_of ref in
  let cache = Db.PVS_farm.get_cache_storage ~__context ~self in
  if List.mem value cache then
    api_error E.pvs_farm_sr_already_added [str self; str value]
  else
    Db.PVS_farm.add_cache_storage ~__context ~self ~value

(** [remove_cache_storage] remove an SR unless it is used *)
let remove_cache_storage ~__context ~self ~value =
  let cache = Db.PVS_farm.get_cache_storage ~__context ~self in
  let str ref = Ref.string_of ref in
  if not @@ List.mem value cache then
    api_error E.sr_not_in_pvs_farm [str self; str value]
  else if sr_is_in_use ~__context ~self value then
    api_error E.pvs_farm_sr_is_in_use [str self; str value]
  else begin
    Xapi_pvs_cache.on_sr_remove ~__context ~sr:value;
    Db.PVS_farm.remove_cache_storage ~__context ~self ~value
  end

