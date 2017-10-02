module D=Debug.Make(struct let name="network_event_loop" end)

module StringMap = Map.Make(struct type t = string let compare = compare end)

type 'a update = Deleted | Snapshot of 'a

let _watch_networks __context ~update_firewall ~wait_after_failure_seconds =

  (* We keep track of the network objects in the database using this event loop. *)
  let classes = ["network"] in
  (* We store the networks in a reference to record map. *)
  let networks = StringMap.empty in
  (* We keep track of the interfaces that we last passed to the firewall script
     to allow NBD traffic on them. At startup, NBD is blocked on all
     interfaces. *)
  let allowed_interfaces = [] in

  let api_timeout = 60. in
  let timeout = 30. +. api_timeout +. !Db_globs.master_connection_reset_timeout in

  let rec update_networks ~token ~networks =

    let most_recent_state events =
      let events_from_newest_to_oldest =
        (* We need to sort the timestamp strings in decreasing order *)
        List.sort (fun e1 e2 -> Event_types.(- String.compare e1.ts e2.ts)) events
      in
      (* We should immediately get a [Some x] after inspecting the first
         element, because the modification and addition events contain a
         snapshot of the object. *)
      List.fold_left
        (fun state_opt event ->
           match state_opt with
           | Some x as s -> s (* We already found the most recent state *)
           | None ->
             if event.Event_types.op = `del then Some Deleted
             else
               (* If we have the object snapshot, return that, otherwise keep looking. *)
               event.Event_types.snapshot
               |> Xapi_stdext_monadic.Opt.map
                 (fun rpc -> Snapshot (API.network_t_of_rpc rpc))
        )
        None
        events_from_newest_to_oldest
    in

    let from =
      Helpers.call_api_functions ~__context
        (fun rpc session_id ->
           Client.Client.Event.from ~rpc ~session_id ~classes ~token ~timeout |> Event_types.event_from_of_rpc)
        ~test_fn:(fun () -> Xapi_event.from ~__context ~classes ~token ~timeout |> Event_types.parse_event_from)
    in
    let events = from.Event_types.events in
    List.iter (fun event -> D.debug "got event %s" (Event_types.string_of_event event)) events;

    let events_by_network_ref =
      List.fold_left
        (fun m event ->
           let ref = event.Event_types.reference  in
           let events = try StringMap.find ref m with _ -> [] in
           let events = event :: events in
           StringMap.add ref events m)
        StringMap.empty
        events
    in

    (* We remove the keys corrersponding to networks for which we do not have an update. *)
    let most_recent_state_by_network_ref = StringMap.fold
        (fun ref events m ->
           match most_recent_state events with
           | Some s -> StringMap.add ref s m
           | None -> m
        )
        events_by_network_ref
        StringMap.empty
    in

    let networks = StringMap.merge
        (fun ref network most_recent_state ->
           match most_recent_state with
           (* We did not get an update about this network, keep our local copy. *)
           | None -> network
           (* The network has been deleted, remove our local copy. *)
           | Some Deleted -> None
           (* The network has been changed, update our local copy. *)
           | Some (Snapshot s) -> Some s
        )
        networks
        most_recent_state_by_network_ref
    in
    (from.Event_types.token, networks)
  in

  let rec loop ~token ~networks ~allowed_interfaces =
    let (token, networks, allowed_interfaces) =
      try
        let token, networks = update_networks ~token ~networks in
        let pifs =
          StringMap.fold
            (fun _ref network pifs -> network.API.network_PIFs @ pifs)
            networks
            []
        in
        let localhost = Helpers.get_localhost ~__context in
        let pifs = List.map (fun pif -> Db.PIF.get_record ~__context ~self:pif) pifs in
        let pifs = List.filter
            (fun pif -> pif.API.pIF_host = localhost && pif.API.pIF_physical = true)
            pifs
        in
        let devices = List.map (fun pif -> pif.API.pIF_device) pifs in
        let interface_list = String.concat ", " devices in
        if not (Xapi_stdext_std.Listext.List.set_equiv devices allowed_interfaces) then begin
          D.debug "Updating the firewall to use the following interfaces for NBD: [%s]" interface_list;
          update_firewall devices
        end else begin
          D.debug "Not updating the firewall, because the set of interfaces to use for NBD did not change: [%s]" interface_list
        end;
        (token, networks, devices)
      with
      | Api_errors.Server_error (code, _) as e when code = Api_errors.events_lost ->
        D.warn "Lost events: %s" (ExnHelper.string_of_exn e);
        ("", networks, allowed_interfaces)
      | e ->
        (* In case of failures, for example due to the script, which updates
           the firewall rules, we start from scratch to ensure that we will
           process the missed events. *)
        D.error "Caught %s listening to events on network objects" (ExnHelper.string_of_exn e);
        Thread.delay wait_after_failure_seconds;
        ("", networks, allowed_interfaces)
    in

    loop ~token ~networks ~allowed_interfaces
  in

  D.debug "Listening to events on network objects";
  loop ~token:"" ~networks ~allowed_interfaces

let update_firewall interfaces_allowed_for_nbd =
  let args = "set" :: interfaces_allowed_for_nbd in
  Forkhelpers.execute_command_get_output !Xapi_globs.nbd_firewall_config_script args |> ignore

let watch_networks () =
  Server_helpers.exec_with_new_task "watching changes in network objects" (_watch_networks ~update_firewall ~wait_after_failure_seconds:5.0)
