
module Mutex = Xapi_stdext_threads.Threadext.Mutex

(* CA-11402 *)
let event_next_unblocking_test rpc session_id () =
  (* Need to create a temporary session ID *)
  let session_id = Qt.init_session rpc !Quicktest_args.username !Quicktest_args.password in
  let () = Client.Client.Event.register rpc session_id [] in (* no events *)
  let m = Mutex.create () in
  let unblocked = ref false in
  let (_: Thread.t) = Thread.create
      (fun () ->
         begin
           try ignore(Client.Client.Event.next rpc session_id)
           with e ->
             print_endline (Printf.sprintf "background thread caught: %s (an exception is expected)" (Printexc.to_string e))
         end;
         Mutex.execute m (fun () -> unblocked := true)
      ) () in
  (* Background thread is started but it cannot simultaneously block and signal us to
     logout so a little pause in here is probably the best we can do *)
  Thread.delay 2.;
  (* Logout which should cause the background thread to unblock *)
  Client.Client.Session.logout rpc session_id;
  (* Again we can't tell the difference between a slow and a totally blocked thread
     so a little pause in here is also required *)
  Thread.delay 2.;
  Alcotest.(check bool) "returns true" true (Mutex.execute m (fun () -> !unblocked))

let event_next_test rpc session_id () =
  let () = Client.Client.Event.register rpc session_id [ "pool" ] in
  let m = Mutex.create () in
  let finished = ref false in
  let pool = Client.Client.Pool.get_all rpc session_id |> List.hd in
  let key = "event_next_test" in
  begin try Client.Client.Pool.remove_from_other_config rpc session_id pool key with _ -> () end;
  let (_: Thread.t) = Thread.create
      (fun () ->
         while not (Mutex.execute m (fun () -> !finished)) do
           ignore (Client.Client.Event.next rpc session_id);
           let oc = Client.Client.Pool.get_other_config rpc session_id pool in
           if List.mem_assoc key oc && (List.assoc key oc) = "1"
           then Mutex.execute m (fun () ->
               print_endline "got expected event";
               finished := true;
             )
         done
      ) () in
  Thread.delay 1.;
  Client.Client.Pool.add_to_other_config rpc session_id pool key "1";
  Thread.delay 1.;
  Alcotest.(check bool) "failed to see pool.other_config change" true (Mutex.execute m (fun () -> !finished))

let wait_for_pool_key rpc session_id key =
  let token = ref "" in
  let finished = ref false in
  let pool = Client.Client.Pool.get_all rpc session_id |> List.hd in
  while not !finished do
    let events = Client.Client.Event.from rpc session_id [ "pool" ] (!token) 10. |> Event_types.event_from_of_rpc in
    token := events.token;
    let oc = Client.Client.Pool.get_other_config rpc session_id pool in
    if List.mem_assoc key oc && (List.assoc key oc) = "1" then finished := true;
  done

let event_from_test rpc session_id () =
  let m = Mutex.create () in
  let finished = ref false in
  let pool = Client.Client.Pool.get_all rpc session_id |> List.hd in
  let key = "event_next_test" in
  begin try Client.Client.Pool.remove_from_other_config rpc session_id pool key with _ -> () end;
  let (_: Thread.t) = Thread.create
      (fun () ->
         wait_for_pool_key rpc session_id key;
         Mutex.execute m (fun () -> finished := true)
      ) () in
  Thread.delay 1.;
  Client.Client.Pool.add_to_other_config rpc session_id pool key "1";
  Thread.delay 1.;
  Alcotest.(check bool) "failed to see pool.other_config change" true (Mutex.execute m (fun () -> !finished))

let event_from_parallel_test rpc session_id () =
  let pool = Client.Client.Pool.get_all rpc session_id |> List.hd in
  let key = "event_next_test" in
  begin try Client.Client.Pool.remove_from_other_config rpc session_id pool key with _ -> () end;
  let ok = ref true in
  let (i_should_succeed: Thread.t) = Thread.create
      (fun () ->
         try
           let _ = Client.Client.Event.from rpc session_id [] "" 10. in
           () (* good *)
         with e ->
           print_endline (Printexc.to_string e);
           ok := false;
      ) () in
  let (interfering_thread: Thread.t) = Thread.create
      (fun () ->
         wait_for_pool_key rpc session_id key
      ) () in
  Thread.delay 1.; (* wait for both threads to block in Event.from *)
  Client.Client.Pool.add_to_other_config rpc session_id pool key "1";
  Thread.join interfering_thread;
  Thread.join i_should_succeed;
  Alcotest.(check bool) "Event.from got cancelled by mistake" true !ok

let object_level_event_test rpc session_id () =
  let m = Mutex.create () in
  let finished = ref false in
  let reported_failure = ref false in
  (* Let's play with templates *)
  let vms = Client.Client.VM.get_all rpc session_id in
  if List.length vms < 2 then failwith "Test needs 2 VMs";
  let vm_a = List.hd vms in
  let vm_b = List.hd (List.tl vms) in
  print_endline (Printf.sprintf "watching %s" (Ref.string_of vm_a));
  print_endline (Printf.sprintf "ignoring %s" (Ref.string_of vm_b));
  let key = "object_level_event_next" in
  begin try Client.Client.VM.remove_from_other_config rpc session_id vm_a key with _ -> () end;
  begin try Client.Client.VM.remove_from_other_config rpc session_id vm_b key with _ -> () end;

  let (_: Thread.t) = Thread.create
      (fun () ->
         let token = ref "" in
         while not (Mutex.execute m (fun () -> !finished)) do
           let events = Client.Client.Event.from rpc session_id [ Printf.sprintf "vm/%s" (Ref.string_of vm_a) ] (!token) 10. |> Event_types.event_from_of_rpc in
           List.iter
             (fun event ->
                if event.Event_types.reference <> Ref.string_of vm_a then begin
                  print_endline (Printf.sprintf "event on %s which we aren't watching" event.reference);
                  Mutex.execute m
                    (fun () ->
                       reported_failure := true;
                       finished := true;
                       Alcotest.fail (Printf.sprintf "got unexpected event (new token = %s)" !token)
                    )
                end
             ) events.events;
           token := events.token;
           let oc = Client.Client.VM.get_other_config rpc session_id vm_a in
           if List.mem_assoc key oc && (List.assoc key oc) = "1"
           then Mutex.execute m (fun () ->
               print_endline (Printf.sprintf "got expected event (new token = %s)" !token);
               finished := true;
             );
         done
      ) () in
  Thread.delay 1.;
  Client.Client.VM.add_to_other_config rpc session_id vm_b key "1";
  Thread.delay 1.;
  Client.Client.VM.remove_from_other_config rpc session_id vm_b key;
  Client.Client.VM.add_to_other_config rpc session_id vm_a key "1";
  Thread.delay 1.;
  Mutex.execute m
    (fun () ->
       if not (!reported_failure) then
         Alcotest.(check bool) "failed to see object-level event change" true !finished
       else
         Alcotest.fail "test failed"
    )

let event_message_test rpc session_id () =
  print_endline "Message creation event test";
  let events = Client.Client.Event.from rpc session_id [ "message" ] "" 1.0 |> Event_types.event_from_of_rpc in
  let token = events.token in
  let pool = List.hd (Client.Client.Pool.get_all rpc session_id) in
  let obj_uuid = Client.Client.Pool.get_uuid rpc session_id pool in
  print_endline "Creating message";
  let cls = `Pool in
  let message = Client.Client.Message.create ~rpc:rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  print_endline (Printf.sprintf "Created message: %s" (Ref.string_of message));
  let events = Client.Client.Event.from rpc session_id [ "message" ] token 1.0 |> Event_types.event_from_of_rpc
  in
  print_endline (Printf.sprintf "Got some events: %d %s" (List.length events.events) (String.concat "," (List.map (fun ev -> ev.Event_types.reference) events.events)));
  Alcotest.(check bool)
    "Failed to receive an event with the message"
    true
    (List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message) && ev.op = `add) events.events);

  print_endline "Message deletion event test";
  print_endline "Destroying message";
  Client.Client.Message.destroy rpc session_id message;
  let events = Client.Client.Event.from rpc session_id [ "message" ] token 1.0 |> Event_types.event_from_of_rpc in
  print_endline "Got some events";
  Alcotest.(check bool)
    "Failed to receive a delete event"
    true
    (List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message) && ev.op = `del) events.events);

  print_endline "Message deletion from cache test";
  let events = Client.Client.Event.from rpc session_id [ "message" ] "" 1.0 |> Event_types.event_from_of_rpc in
  print_endline "Got lots of events";
  Alcotest.(check bool)
    "Got told about a deleted message"
    false
    (List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message) && ev.op <> `del) events.events);

  print_endline "Multi message test";
  let message1 = Client.Client.Message.create ~rpc:rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  let message2 = Client.Client.Message.create ~rpc:rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  let events = Client.Client.Event.from rpc session_id [ "message" ] token 1.0 |> Event_types.event_from_of_rpc in
  let token = events.token in
  let message3 = Client.Client.Message.create ~rpc:rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  let events2 = Client.Client.Event.from rpc session_id [ "message" ] token 1.0 |> Event_types.event_from_of_rpc in
  print_endline (Printf.sprintf "message1=%s" (Ref.string_of message1));
  print_endline (Printf.sprintf "message2=%s" (Ref.string_of message2));
  print_endline (Printf.sprintf "message3=%s" (Ref.string_of message3));
  List.iter (fun ev -> print_endline (Printf.sprintf "events1: ev.ref=%s" ev.Event_types.reference)) events.events;
  List.iter (fun ev -> print_endline (Printf.sprintf "events2: ev.ref=%s" ev.Event_types.reference)) events2.events;
  let ok1 =
    List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message1) && ev.op = `add) events.events &&
    List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message2) && ev.op = `add) events.events in
  let ok2 =
    List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message3) && ev.op = `add) events2.events in
  let ok3 =
    not (List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message1) && ev.op = `add) events2.events) &&
    not (List.exists (fun ev -> ev.Event_types.reference = (Ref.string_of message2) && ev.op = `add) events2.events)
  in
  Alcotest.(check bool)
    (Printf.sprintf "Multi message test: ok1=%b ok2=%b ok3=%b" ok1 ok2 ok3)
    true
    (ok1 && ok2 && ok3);

  print_endline (Printf.sprintf "Finding messages for object: %s" (Client.Client.Pool.get_uuid rpc session_id pool));
  let messages = Client.Client.Message.get ~rpc:rpc ~session_id  ~cls ~obj_uuid ~since:(Xapi_stdext_date.Date.never) in
  let has_msg m = List.exists (fun (r,_) -> r=m) messages in
  Alcotest.(check bool)
    "Object messages test: failed to get messages for object"
    true
    (has_msg message1 && has_msg message2 && has_msg message3)

let event_inject_test rpc session_id () =
  let events = Client.Client.Event.from rpc session_id [ "pool" ] "" 1.0 |> Event_types.event_from_of_rpc in
  let token = events.token in
  let pool = List.hd (Client.Client.Pool.get_all rpc session_id) in
  let starttime = Unix.gettimeofday () in
  let (x: Thread.t) = Thread.create
      (fun () ->
         let _ = Client.Client.Event.from rpc session_id [ "pool" ] token 5.0 in
         ()
      ) () in
  ignore(Client.Client.Event.inject ~rpc:rpc ~session_id ~_class:"pool" ~_ref:(Ref.string_of pool));
  Thread.join x;
  let endtime = Unix.gettimeofday () in
  Alcotest.(check bool)
    "Failed to see injected event"
    false
    (endtime -. starttime > 4.5)

module StringSet=Set.Make(String)

let event_from_number_test rpc session_id () =
  let events = Client.Client.Event.from rpc session_id [ "vm" ] "" 10. |> Event_types.event_from_of_rpc in
  let (_,f) = List.fold_left (fun (set,failed) ev ->
      let reference = ev.Event_types.reference in
      if StringSet.mem reference set
      then (set,true)
      else (StringSet.add reference set, failed)) (StringSet.empty, false) events.events in
  Alcotest.(check bool) "Object seen twice in events" false f

let tests () =
  let open Qt_filter in
  [ ["event_next_unblocking_test", `Slow, event_next_unblocking_test] |> conn
  ; ["event_next_test", `Slow, event_next_test] |> conn
  ; ["event_from_test", `Slow, event_from_test] |> conn
  ; ["event_from_parallel_test", `Slow, event_from_parallel_test] |> conn
  ; ["object_level_event_test", `Slow, object_level_event_test] |> conn
  ; ["event_message_test", `Slow, event_message_test] |> conn
  ; ["event_inject_test", `Slow, event_inject_test] |> conn
  ; ["event_from_number_test", `Slow, event_from_number_test] |> conn
  ]
  |> List.concat
