(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

module D=Debug.Make(struct let name="network_event_loop" end)
open D

let _watch_networks_for_nbd_changes __context ~update_firewall ~wait_after_event_seconds ~wait_after_failure_seconds =
  (* We keep track of the network objects in the database using this event loop. *)
  let classes = ["network"] in
  (* We keep track of the interfaces that we last passed to the firewall script
     to allow NBD traffic on them. At startup, we don't know on which
     interfaces NBD is allowed, and we always update the firewall. *)
  let allowed_interfaces = None in

  let api_timeout = 60. in
  let timeout = 30. +. api_timeout +. !Db_globs.master_connection_reset_timeout in
  let wait_for_network_change ~token =
    let open Event_types in
    let from = ref {events = []; valid_ref_counts = []; token = ""} in
    if !Xapi_globs.slave_dbs then
    from :=
        Event_types.parse_event_from (Xapi_slave_db.call_with_updated_context __context
                                        (Xapi_event.with_safe_missing_handling (fun () -> (Xapi_event.from ~classes ~token ~timeout))))
    else
      from := Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.Event.from ~rpc ~session_id ~classes ~token ~timeout |> Event_types.event_from_of_rpc);
    !from.Event_types.token
  in

  let localhost = Helpers.get_localhost ~__context in

  let rec loop ~token ~allowed_interfaces =
    let (token, allowed_interfaces) =
      try
        let token = wait_for_network_change ~token in
        let pifs = Db.Host.get_PIFs ~__context ~self:localhost in
        let allowed_connected_networks =
          (* We use Valid_ref_list to continue processing the list in case some network refs are null or invalid *)
          Valid_ref_list.filter_map
            (fun pif ->
               let network = Db.PIF.get_network ~__context ~self:pif in
               let purpose = Db.Network.get_purpose ~__context ~self:network in
               if List.mem `nbd purpose || List.mem `insecure_nbd purpose then
                 Some network
               else
                 None
            )
            pifs
        in
        let interfaces = List.map (fun network -> Db.Network.get_bridge ~__context ~self:network) allowed_connected_networks in
        let interfaces = Xapi_stdext_std.Listext.List.setify interfaces in
        let needs_firewall_update = match allowed_interfaces with
          | Some allowed_interfaces ->
            not (Xapi_stdext_std.Listext.List.set_equiv interfaces allowed_interfaces)
          | None ->
            (* We've just started the event loop, and we do not know the state of the firewall. *)
            true
        in
        let interface_list = String.concat ", " interfaces in
        if needs_firewall_update then begin
          debug "Updating the firewall to use the following interfaces for NBD: [%s]" interface_list;
          update_firewall interfaces
        end else begin
          debug "Not updating the firewall, because the set of interfaces to use for NBD did not change: [%s]" interface_list
        end;
        (* Wait for a bit after we processed the event for rate-limiting the database queries *)
        Thread.delay wait_after_event_seconds;
        (token, Some interfaces)
      with
      | Api_errors.Server_error (code, _) as e when code = Api_errors.events_lost ->
        warn "Lost events: %s" (ExnHelper.string_of_exn e);
        ("", allowed_interfaces)
      | e ->
        (* In case of failures, for example due to the script, which updates
           the firewall rules, we start from scratch to ensure that we will
           process the missed events. *)
        error "Caught %s listening to events on network objects" (ExnHelper.string_of_exn e);
        Thread.delay wait_after_failure_seconds;
        ("", allowed_interfaces)
    in

    loop ~token ~allowed_interfaces
  in

  debug "Listening to events on network objects";
  loop ~token:"" ~allowed_interfaces

let update_firewall interfaces_allowed_for_nbd =
  let args = "set" :: interfaces_allowed_for_nbd in
  Forkhelpers.execute_command_get_output !Xapi_globs.nbd_firewall_config_script args |> ignore

let watch_networks_for_nbd_changes () =
  Server_helpers.exec_with_new_task "watching networks for NBD-related changes" (_watch_networks_for_nbd_changes ~update_firewall ~wait_after_event_seconds:5.0 ~wait_after_failure_seconds:5.0)
