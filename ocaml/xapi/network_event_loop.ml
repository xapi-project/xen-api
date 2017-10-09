module D=Debug.Make(struct let name="network_event_loop" end)

module StringMap = Map.Make(struct type t = string let compare = compare end)

let _watch_networks __context ~update_firewall ~wait_after_failure_seconds =

  (* We keep track of the network objects in the database using this event loop. *)
  let classes = ["network"] in
  (* We keep track of the interfaces that we last passed to the firewall script
     to allow NBD traffic on them. At startup, we don't know on which
     interfaces NBD is allowed, and we always update the firewall. *)
  let allowed_interfaces = None in

  let api_timeout = 60. in
  let timeout = 30. +. api_timeout +. !Db_globs.master_connection_reset_timeout in

  let wait_for_network_change ~token =
    let from =
      Helpers.call_api_functions ~__context
        (fun rpc session_id ->
           Client.Client.Event.from ~rpc ~session_id ~classes ~token ~timeout |> Event_types.event_from_of_rpc)
        ~test_fn:(fun () -> Xapi_event.from ~__context ~classes ~token ~timeout |> Event_types.parse_event_from)
    in
    let events = from.Event_types.events in
    List.iter (fun event -> D.debug "got event %s" (Event_types.string_of_event event)) events;
    from.Event_types.token
  in

  let rec loop ~token ~allowed_interfaces =
    let (token, allowed_interfaces) =
      try
        let token = wait_for_network_change ~token in
        let localhost = Helpers.get_localhost ~__context in
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
        let allowed_local_bridges = List.map (fun network -> Db.Network.get_bridge ~__context ~self:network) allowed_connected_networks in
        let allowed_local_bridges = List.filter ((<>) "") allowed_local_bridges in
        let interfaces = allowed_local_bridges in
        let needs_firewall_update = match allowed_interfaces with
          | Some allowed_interfaces ->
            not (Xapi_stdext_std.Listext.List.set_equiv interfaces allowed_interfaces)
          | None ->
            (* We've just started the event loop, and we do not know the state of the firewall. *)
            true
        in
        let interface_list = String.concat ", " interfaces in
        if needs_firewall_update then begin
          D.debug "Updating the firewall to use the following interfaces for NBD: [%s]" interface_list;
          update_firewall interfaces
        end else begin
          D.debug "Not updating the firewall, because the set of interfaces to use for NBD did not change: [%s]" interface_list
        end;
        (token, Some interfaces)
      with
      | Api_errors.Server_error (code, _) as e when code = Api_errors.events_lost ->
        D.warn "Lost events: %s" (ExnHelper.string_of_exn e);
        ("", allowed_interfaces)
      | e ->
        (* In case of failures, for example due to the script, which updates
           the firewall rules, we start from scratch to ensure that we will
           process the missed events. *)
        D.error "Caught %s listening to events on network objects" (ExnHelper.string_of_exn e);
        Thread.delay wait_after_failure_seconds;
        ("", allowed_interfaces)
    in

    loop ~token ~allowed_interfaces
  in

  D.debug "Listening to events on network objects";
  loop ~token:"" ~allowed_interfaces

let update_firewall interfaces_allowed_for_nbd =
  let args = "set" :: interfaces_allowed_for_nbd in
  Forkhelpers.execute_command_get_output !Xapi_globs.nbd_firewall_config_script args |> ignore

let watch_networks () =
  Server_helpers.exec_with_new_task "watching changes in network objects" (_watch_networks ~update_firewall ~wait_after_failure_seconds:5.0)
