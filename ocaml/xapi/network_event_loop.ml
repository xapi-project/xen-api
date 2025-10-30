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

module D = Debug.Make (struct let name = "network_event_loop" end)

open D

let _watch_networks_for_nbd_changes __context ~update_firewall
    ~wait_after_event_seconds ~wait_after_failure_seconds =
  (* We keep track of the network objects in the database using this event loop. *)
  let classes = ["network"] in
  (* We keep track of the interfaces that we last passed to the firewall script
     to allow NBD traffic on them. At startup, we don't know on which
     interfaces NBD is allowed, and we always update the firewall. *)
  let allowed_interfaces = None in
  let api_timeout = 60. in
  let timeout =
    30.
    +. api_timeout
    +. !Xapi_database.Db_globs.master_connection_reset_timeout
  in
  let wait_for_network_change ~token =
    let from =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.Event.from ~rpc ~session_id ~classes ~token ~timeout
          |> Event_types.event_from_of_rpc
      )
    in
    from.Event_types.token
  in
  let localhost = Helpers.get_localhost ~__context in
  let rec loop ~token ~allowed_interfaces =
    let token, allowed_interfaces =
      try
        let token = wait_for_network_change ~token in
        let interfaces =
          Xapi_host.get_nbd_interfaces ~__context ~self:localhost
        in
        let needs_firewall_update =
          match allowed_interfaces with
          | Some allowed_interfaces ->
              not
                (Xapi_stdext_std.Listext.List.set_equiv interfaces
                   allowed_interfaces
                )
          | None ->
              (* We've just started the event loop, and we do not know the state of the firewall. *)
              true
        in
        let interface_list = String.concat ", " interfaces in
        if needs_firewall_update then (
          debug
            "Updating the firewall to use the following interfaces for NBD: \
             [%s]"
            interface_list ;
          update_firewall interfaces
        ) else
          debug
            "Not updating the firewall, because the set of interfaces to use \
             for NBD did not change: [%s]"
            interface_list ;
        (* Wait for a bit after we processed the event for rate-limiting the database queries *)
        Thread.delay wait_after_event_seconds ;
        (token, Some interfaces)
      with
      | Api_errors.Server_error (code, _) as e
        when code = Api_errors.events_lost ->
          warn "Lost events: %s" (ExnHelper.string_of_exn e) ;
          ("", allowed_interfaces)
      | e ->
          (* In case of failures, for example due to the script, which updates
             the firewall rules, we start from scratch to ensure that we will
             process the missed events. *)
          error "Caught %s listening to events on network objects"
            (ExnHelper.string_of_exn e) ;
          Thread.delay wait_after_failure_seconds ;
          ("", allowed_interfaces)
    in
    loop ~token ~allowed_interfaces
  in
  debug "Listening to events on network objects" ;
  loop ~token:"" ~allowed_interfaces

let update_firewall interfaces_allowed_for_nbd =
  let module Fw =
    ( val Firewall.firewall_provider !Xapi_globs.firewall_backend
        : Firewall.FIREWALL
      )
  in
  let status =
    match interfaces_allowed_for_nbd with
    | [] ->
        Firewall.Disabled
    | _ ->
        Firewall.Enabled
  in
  Fw.update_firewall_status ~interfaces:interfaces_allowed_for_nbd Firewall.Nbd
    status

let watch_networks_for_nbd_changes () =
  Server_helpers.exec_with_new_task "watching networks for NBD-related changes"
    (_watch_networks_for_nbd_changes ~update_firewall
       ~wait_after_event_seconds:5.0 ~wait_after_failure_seconds:5.0
    )
