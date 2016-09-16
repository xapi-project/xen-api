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

open Stdext
open Listext

module D = Debug.Make(struct let name = "xapi_pvs_proxy_control" end)
open D

let proxy_port_name bridge =
  "pvs-" ^ bridge

(** [proxies] returns all currently attached proxies *)
let get_running_proxies ~__context ~site =
  let open Db_filter_types in
  Db.PVS_proxy.get_refs_where ~__context
    ~expr:
      (And
         ((Eq (Field "site", Literal (Ref.string_of site)))
         ,(Eq (Field "currently_attached", Literal "true"))
         ))

let metadata_of_site ~__context ~site ~vdi ~proxies =
  let open Network_interface in
  let site_rc = Db.PVS_site.get_record ~__context ~self:site in
  let servers =
    List.map (fun self ->
        let rc = Db.PVS_server.get_record ~__context ~self in
        PVS_proxy.Server.{
          uuid = rc.API.pVS_server_uuid;
          addresses = List.map Unix.inet_addr_of_string rc.API.pVS_server_addresses;
          first_port = Int64.to_int rc.API.pVS_server_first_port;
          last_port = Int64.to_int rc.API.pVS_server_last_port;
        }
      ) site_rc.API.pVS_site_servers
  in
  let clients =
    List.map (fun (vif, proxy) ->
        let rc = Db.VIF.get_record ~__context ~self:vif in
        PVS_proxy.Client.{
          uuid = rc.API.vIF_uuid;
          mac = rc.API.vIF_MAC;
          interface = proxy_port_name (Db.Network.get_bridge ~__context ~self:rc.API.vIF_network);
          prepopulate = false;
        }
      ) proxies
  in
  let vdi = Db.VDI.get_uuid ~__context ~self:vdi in
  PVS_proxy.{
    site_uuid = site_rc.API.pVS_site_uuid;
    site_name = site_rc.API.pVS_site_name_label;
    servers;
    clients;
    vdi;
  }

(** Request xcp-networkd to update a site's PVS-proxy daemon configuration,
 *  for all locally running proxies, taking into account starting and stopping proxies *)
let update_site_on_localhost ~__context ~site ~vdi ?(starting_proxies=[]) ?(stopping_proxies=[]) () =
  debug "Updating PVS site %s. Starting proxies: [%s]. Stopping proxies: [%s]."
    (Ref.string_of site)
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

  let running_proxies = get_running_proxies ~__context ~site in
  let localhost = Helpers.get_localhost ~__context in
  let local_running_proxies = List.filter_map (fun proxy ->
      let vif = Db.PVS_proxy.get_VIF ~__context ~self:proxy in
      let vm = Db.VIF.get_VM ~__context ~self:vif in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        Some (vif, proxy)
      else
        None
    ) running_proxies in
  let proxies = starting_proxies @ (List.set_difference local_running_proxies stopping_proxies) |> List.setify in
  let proxy_config = metadata_of_site ~__context ~site ~vdi ~proxies in
  Network.Net.PVS_proxy.configure_site dbg proxy_config;

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

(** Request xcp-networkd to tell the local PVS-proxy daemon that it must stop
 *  proxying for the given site, and release the associated cache VDI. *)
let remove_site_on_localhost ~__context ~site =
  let open Network_interface.PVS_proxy in
  let dbg = Context.string_of_task __context in
  let uuid = Db.PVS_site.get_PVS_uuid ~__context ~self:site in
  Network.Net.PVS_proxy.remove_site dbg uuid

exception No_cache_sr_available

let find_cache_vdi ~__context ~host ~site =
  let open Db_filter_types in
  (* There should be at most one matching PVS_cache_storage object *)
  let pcs' = Db.PVS_cache_storage.get_refs_where ~__context ~expr:(And
                                                                     (Eq (Field "host", Literal (Ref.string_of host)),
                                                                      Eq (Field "site", Literal (Ref.string_of site))))
  in
  match pcs' with
  | [] ->
    raise No_cache_sr_available
  | pcs :: _ ->
    Db.PVS_cache_storage.get_VDI ~__context ~self:pcs

let start_proxy ~__context vif proxy =
  try
    Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
    Helpers.assert_using_vswitch ~__context;
    let host = Helpers.get_localhost ~__context in
    let site = Db.PVS_proxy.get_site ~__context ~self:proxy in
    let vdi = find_cache_vdi ~__context ~host ~site in
    update_site_on_localhost ~__context ~site ~vdi ~starting_proxies:[vif, proxy] ();
    Db.PVS_proxy.set_status ~__context ~self:proxy ~value:`initialised;
    true
  with e ->
    let reason =
      match e with
      | No_cache_sr_available ->
        let proxy_uuid = Db.PVS_proxy.get_uuid ~__context ~self:proxy in
        let body = Printf.sprintf "Unable to setup PVS-proxy %s for VIF %s: no cache storage found on PVS-site %s for host %s."
            proxy_uuid (Db.VIF.get_uuid ~__context ~self:vif)
            (Db.PVS_site.get_name_label ~__context ~self:(Db.PVS_proxy.get_site ~__context ~self:proxy))
            (Db.Host.get_name_label ~__context ~self:(Helpers.get_localhost ~__context)) in
        let (name, priority) = Api_messages.pvs_proxy_no_cache_sr_available in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            ignore(Client.Client.Message.create ~rpc ~session_id ~name  ~priority ~cls:`PVS_proxy ~obj_uuid:proxy_uuid  ~body));
        "no PVS cache SR available"
      | Network_interface.PVS_proxy_connection_error ->
        let proxy_uuid = Db.PVS_proxy.get_uuid ~__context ~self:proxy in
        let body = Printf.sprintf "Failed to setup PVS-proxy %s for VIF %s on host %s due to an internal error"
            proxy_uuid (Db.VIF.get_uuid ~__context ~self:vif)
            (Db.Host.get_name_label ~__context ~self:(Helpers.get_localhost ~__context)) in
        let (name, priority) = Api_messages.pvs_proxy_setup_failed in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            ignore(Client.Client.Message.create ~rpc ~session_id ~name ~priority ~cls:`PVS_proxy ~obj_uuid:proxy_uuid ~body));
        "unable to connect to PVS proxy daemon"
      | Api_errors.Server_error (code, args) when
          code = Api_errors.license_restriction
          && args = [Features.(name_of_feature PVS_proxy)] ->
        "PVS proxy not licensed"
      | Api_errors.Server_error (code, args) when
          code = Api_errors.openvswitch_not_active ->
        "Host is not using openvswitch"
      | _ -> Printf.sprintf "unknown error (%s)" (Printexc.to_string e)
    in
    warn "Unable to enable PVS proxy for VIF %s: %s. Continuing with proxy unattached." (Ref.string_of vif) reason;
    false

let stop_proxy ~__context vif proxy =
  try
    let site = Db.PVS_proxy.get_site ~__context ~self:proxy in
    let host = Helpers.get_localhost ~__context in
    let vdi = find_cache_vdi ~__context ~host ~site in
    update_site_on_localhost ~__context ~site ~vdi ~stopping_proxies:[vif, proxy] ();
    Db.PVS_proxy.set_status ~__context ~self:proxy ~value:`stopped
  with e ->
    let reason =
      match e with
      | No_cache_sr_available -> "no PVS cache VDI found"
      | Network_interface.PVS_proxy_connection_error -> "unable to connect to PVS proxy daemon"
      | _ -> Printf.sprintf "unknown error (%s)" (Printexc.to_string e)
    in
    error "Unable to disable PVS proxy for VIF %s: %s." (Ref.string_of vif) reason

let find_proxy_for_vif ~__context ~vif =
  let open Db_filter_types in
  let proxies = Db.PVS_proxy.get_refs_where ~__context
      ~expr:(Eq (Field "VIF", Literal (Ref.string_of vif))) in
  match proxies with
  | [] -> None
  | proxy :: _ -> Some proxy

(* Called on VM start *)
let maybe_start_proxy_for_vif ~__context ~vif =
  Opt.iter
    (fun p -> start_proxy ~__context vif p |> ignore)
    (find_proxy_for_vif ~__context ~vif)

let maybe_stop_proxy_for_vif ~__context ~vif =
  Opt.iter
    (stop_proxy ~__context vif)
    (find_proxy_for_vif ~__context ~vif)
