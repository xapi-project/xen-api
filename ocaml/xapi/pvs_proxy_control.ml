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
open Threadext
open Xstringext

module D = Debug.Make(struct let name = "xapi_pvs_proxy_control" end)
open D

let proxy_port_name vif =
  (* Interface names in Linux are at most 15 characters. We derive a 
     name from the MAC address to ensure uniqueness, and make it fit. *)
  let mac = String.replace ":" "" vif.API.vIF_MAC in
  Printf.sprintf "pvs%s" mac

(** [proxies] returns all currently attached proxies *)
let get_running_proxies ~__context ~site =
  let open Db_filter_types in
  Db.PVS_proxy.get_refs_where ~__context
    ~expr:
      (And
         ((Eq (Field "site", Literal (Ref.string_of site)))
         ,(Eq (Field "currently_attached", Literal "true"))
         ))

(* A module to update and query the state of the proxies on the local host *)
module State = struct
  type t = Starting | Started | Stopping | Failed

  open Xenstore

  let of_string = function
    | "starting" -> Starting
    | "started" -> Started
    | "stopping" -> Stopping
    | "failed" -> Failed
    | _ -> failwith "unknown proxy state"

  let string_of = function
    | Starting -> "starting"
    | Started -> "started"
    | Stopping -> "stopping"
    | Failed -> "failed"

  let (//) = Filename.concat
  let root = "/xapi/pvs-proxy"
  let _state = "state"
  let _proxy_uuid = "proxy-uuid"

  (*
    For each proxy, we have the following xenstore entries:

    /xapi/pvs-proxy/<site-uuid>/<vif-uuid>/state = <state>
    /xapi/pvs-proxy/<site-uuid>/<vif-uuid>/proxy-uuid = <proxy-uuid>
  *)

  let mark_proxy ~__context site vif proxy state =
    let site_uuid = Db.PVS_site.get_uuid ~__context ~self:site in
    let vif_uuid = Db.VIF.get_uuid ~__context ~self:vif in
    let proxy_uuid = Db.PVS_proxy.get_uuid ~__context ~self:proxy in
    with_xs (fun xs ->
        let dir = root // site_uuid // vif_uuid in
        xs.Xs.write (dir // _state) (string_of state);
        xs.Xs.write (dir // _proxy_uuid) proxy_uuid
      )

  let remove_proxy ~__context site vif =
    let site_uuid = Db.PVS_site.get_uuid ~__context ~self:site in
    let vif_uuid = Db.VIF.get_uuid ~__context ~self:vif in
    with_xs (fun xs ->
        let dir = root // site_uuid // vif_uuid in
        xs.Xs.rm dir
      )

  let remove_site ~__context site =
    let site_uuid = Db.PVS_site.get_uuid ~__context ~self:site in
    with_xs (fun xs ->
        xs.Xs.rm (root // site_uuid)
      )

  let get_running_proxies ~__context site =
    let site_uuid = Db.PVS_site.get_uuid ~__context ~self:site in
    with_xs (fun xs ->
        xs.Xs.directory (root // site_uuid) |>
        List.filter_map (fun vif_uuid ->
            try
              let dir = root // site_uuid // vif_uuid in
              let state = of_string (xs.Xs.read (dir // _state)) in
              if state = Starting || state = Started then
                let proxy_uuid = xs.Xs.read (dir // _proxy_uuid) in
                let vif = Db.VIF.get_by_uuid ~__context ~uuid:vif_uuid in
                let proxy = Db.PVS_proxy.get_by_uuid ~__context ~uuid:proxy_uuid in
                Some (vif, proxy)
              else
                None
            with _ -> None
          )
      )
end

let metadata_of_site ~__context ~site ~vdi ~proxies =
  let open Network.Net in
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
        let uuid = Db.VM.get_uuid ~__context ~self:rc.API.vIF_VM in
        PVS_proxy.Client.{
          uuid;
          mac = rc.API.vIF_MAC;
          interface = proxy_port_name rc;
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

let configure_proxy_m = Mutex.create ()

(** Request xcp-networkd to update a site's PVS-proxy daemon configuration,
 *  for all locally running proxies, taking into account starting and stopping proxies *)
let update_site_on_localhost ~__context ~site ~vdi =
  debug "Updating PVS site %s." (Ref.string_of site);
  let open Network.Net.PVS_proxy in
  let dbg = Context.string_of_task __context in
  Mutex.execute configure_proxy_m (fun () ->
      let proxies = State.get_running_proxies ~__context site in
      let proxy_config = metadata_of_site ~__context ~site ~vdi ~proxies in
      Network.Net.PVS_proxy.configure_site dbg proxy_config
    )

(** Request xcp-networkd to tell the local PVS-proxy daemon that it must stop
 *  proxying for the given site, and release the associated cache VDI. *)
let remove_site_on_localhost ~__context ~site =
  debug "Removing PVS site %s." (Ref.string_of site);
  let open Network.Net.PVS_proxy in
  let dbg = Context.string_of_task __context in
  let uuid = Db.PVS_site.get_uuid ~__context ~self:site in
  State.remove_site ~__context site;
  Mutex.execute configure_proxy_m (fun () ->
      Network.Net.PVS_proxy.remove_site dbg uuid
    )

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
    Pvs_cache_vdi.get_or_recreate_vdi ~__context ~self:pcs

let start_proxy ~__context vif proxy =
  let dbg = Context.string_of_task __context in
  let host = Helpers.get_localhost ~__context in
  let site = Db.PVS_proxy.get_site ~__context ~self:proxy in
  try
    Pool_features.assert_enabled ~__context ~f:Features.PVS_proxy;
    Helpers.assert_using_vswitch ~__context;
    Helpers.assert_pvs_servers_available ~__context ~pvs_site:site;

    (* Create an interface and OVS port for the proxy *)
    let vifr = Db.VIF.get_record ~__context ~self:vif in
    let bridge = Db.Network.get_bridge ~__context ~self:vifr.API.vIF_network in
    let port_name = proxy_port_name vifr in
    Network.Net.Bridge.add_port dbg None bridge port_name [] None (Some Network_interface.PVS_proxy);

    (* Update the PVS proxy daemon and related metadata *)
    let vdi = find_cache_vdi ~__context ~host ~site in
    State.mark_proxy ~__context site vif proxy State.Starting;
    update_site_on_localhost ~__context ~site ~vdi;
    State.mark_proxy ~__context site vif proxy State.Started;
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
      | Network_interface.(Network_error PVS_proxy_connection_error) ->
        let proxy_uuid = Db.PVS_proxy.get_uuid ~__context ~self:proxy in
        let body = Printf.sprintf "Failed to setup PVS-proxy %s for VIF %s on host %s due to an internal error"
            proxy_uuid (Db.VIF.get_uuid ~__context ~self:vif)
            (Db.Host.get_name_label ~__context ~self:(Helpers.get_localhost ~__context)) in
        let (name, priority) = Api_messages.pvs_proxy_setup_failed in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            ignore(Client.Client.Message.create ~rpc ~session_id ~name ~priority ~cls:`PVS_proxy ~obj_uuid:proxy_uuid ~body));
        "unable to connect to PVS proxy daemon"
      | Helpers.No_pvs_server_available ->
        let proxy_uuid = Db.PVS_proxy.get_uuid ~__context ~self:proxy in
        let vif_uuid = Db.VIF.get_uuid ~__context ~self:vif in
        let site_name_label = Db.PVS_site.get_name_label ~__context ~self:(Db.PVS_proxy.get_site ~__context ~self:proxy) in
        let host_name_label = Db.Host.get_name_label ~__context ~self:(Helpers.get_localhost ~__context) in
        let body = Printf.sprintf "Unable to setup PVS-proxy %s for VIF %s: no PVS-server found on PVS-site %s for host %s."
            proxy_uuid vif_uuid site_name_label host_name_label in
        let (name, priority) = Api_messages.pvs_proxy_no_server_available in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            ignore(Client.Client.Message.create ~rpc ~session_id ~name ~priority ~cls:`PVS_proxy ~obj_uuid:proxy_uuid ~body));
        "no PVS server available"
      | Api_errors.Server_error (code, args) when
          code = Api_errors.license_restriction
          && args = [Features.(name_of_feature PVS_proxy)] ->
        "PVS proxy not licensed"
      | Api_errors.Server_error (code, args) when
          code = Api_errors.openvswitch_not_active ->
        "Host is not using openvswitch"
      | Api_errors.Server_error ("SR_BACKEND_FAILURE_44", _)
      | Api_errors.Server_error ("SR_BACKEND_FAILURE_79", _) ->
        let proxy_uuid = Db.PVS_proxy.get_uuid ~__context ~self:proxy in
        let body = Printf.sprintf
            "Unable to setup PVS-proxy %s for VIF %s and PVS-site %s: \
             PVS cache size for host %s exceeds SR available space."
            proxy_uuid (Db.VIF.get_uuid ~__context ~self:vif)
            (Db.PVS_site.get_name_label ~__context ~self:(Db.PVS_proxy.get_site ~__context ~self:proxy))
            (Db.Host.get_name_label ~__context ~self:(Helpers.get_localhost ~__context)) in
        let (name, priority) = Api_messages.pvs_proxy_sr_out_of_space in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            ignore(Client.Client.Message.create
                     ~rpc ~session_id ~name  ~priority ~cls:`PVS_proxy ~obj_uuid:proxy_uuid  ~body));
        "PVS cache size exceeds SR available space"
      | _ -> Printf.sprintf "unknown error (%s)" (Printexc.to_string e)
    in
    State.mark_proxy ~__context site vif proxy State.Failed;
    warn "Unable to enable PVS proxy for VIF %s: %s. Continuing with proxy unattached." (Ref.string_of vif) reason;
    false

let stop_proxy ~__context vif proxy =
  try
    let dbg = Context.string_of_task __context in
    let site = Db.PVS_proxy.get_site ~__context ~self:proxy in
    let host = Helpers.get_localhost ~__context in

    (* Update the PVS proxy daemon and related metadata *)
    let vdi = find_cache_vdi ~__context ~host ~site in
    State.mark_proxy ~__context site vif proxy State.Stopping;
    update_site_on_localhost ~__context ~site ~vdi;
    State.remove_proxy ~__context site vif;
    Db.PVS_proxy.set_status ~__context ~self:proxy ~value:`stopped;

    (* Remove the interface and OVS port for the proxy *)
    let vifr = Db.VIF.get_record ~__context ~self:vif in
    let bridge = Db.Network.get_bridge ~__context ~self:vifr.API.vIF_network in
    let port_name = proxy_port_name vifr in
    Network.Net.Bridge.remove_port dbg bridge port_name
  with e ->
    let reason =
      match e with
      | No_cache_sr_available -> "no PVS cache VDI found"
      | Network_interface.(Network_error PVS_proxy_connection_error) -> "unable to connect to PVS proxy daemon"
      | _ -> Printf.sprintf "unknown error (%s)" (Printexc.to_string e)
    in
    error "Unable to disable PVS proxy for VIF %s: %s." (Ref.string_of vif) reason

let clear_proxy_state ~__context vif proxy =
  Db.PVS_proxy.set_currently_attached ~__context ~self:proxy ~value:false;
  Db.PVS_proxy.set_status ~__context ~self:proxy ~value:`stopped

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
