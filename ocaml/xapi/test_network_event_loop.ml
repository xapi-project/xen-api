let test_network_event_loop ~no_networks_at_start () =
  let __context, _ = Test_event_common.event_setup_common () in
  (* We need to set test_mode to true to ensure that the event loop will use
     the local Xapi_events.from function instead of the Client module, which
     would cause the test to fail. *)
  Helpers.test_mode := true;

  let localhost = Helpers.get_localhost ~__context in
  let other_host = Test_common.make_host ~__context () in

  (* We have to wait for a bit for the event loop to notice the changes, without a delay the test will fail. *)
  let delay = 0.1 in
  let network_event_loop_wait_after_failure_seconds = 0.4 in
  let received_params = ref None in

  (* We simulate failure of the firewall update script this way *)
  let fail_firewall_update = ref false in

  let start_event_loop = Thread.create
      (fun () ->
         Network_event_loop._watch_networks
           __context
           ~update_firewall:(fun pifs ->
               if !fail_firewall_update then failwith "Failed to update firewall";
               received_params := Some pifs
             )
           ~wait_after_failure_seconds:network_event_loop_wait_after_failure_seconds
      )
  in

  let assert_received_params expected =
    Thread.delay delay;
    match !received_params with
    | None -> OUnit.assert_failure "The update_firewall function was not called"
    | Some p ->
      Ounit_comparators.StringSet.(assert_equal (of_list expected) (of_list p))
  in

  let assert_not_called () =
    Thread.delay delay;
    OUnit.assert_equal ~msg:"update_firewall shouldn't have been called" None !received_params
  in

  if no_networks_at_start then begin
    received_params := None;
    let _ : Thread.t = start_event_loop () in
    assert_not_called ()
  end;

  (* Add a new network "network1" with a PIF connected to this host *)
  received_params := None;
  let network1 = Test_common.make_network ~__context ~purpose:[`nbd] () in
  assert_not_called ();
  let pif1 = Test_common.make_pif ~__context ~network:network1 ~host:localhost ~device:"pif1" () in
  if not no_networks_at_start then begin
    let _ : Thread.t = start_event_loop () in ()
  end;
  assert_received_params ["pif1"];

  (* Add another PIF to network1 connected to this host *)
  received_params := None;
  fail_firewall_update := true;
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network1 ~host:localhost ~device:"pif2" () in
  (* After [delay] seconds, the event loop should have noticed the change and
     failed after trying to update the firewall *)
  Thread.delay delay;
  fail_firewall_update := false;
  (* Test that we do wait for the given delay in case of failures *)
  assert_not_called ();
  (* Now wait for the event loop to reregister and continue *)
  Thread.delay network_event_loop_wait_after_failure_seconds;
  (* Test that transient failures in the script won't stop the event loop, and
     that we will eventually process the event we missed *)
  assert_received_params ["pif1"; "pif2"];

  received_params := None;
  Db.Network.set_name_description ~__context ~self:network1 ~value:"irrelevant network object modification";
  assert_not_called ();

  (* Add a new network "network2" with a PIF connected to another host *)
  received_params := None;
  let network2 = Test_common.make_network ~__context ~purpose:[`nbd] () in
  assert_not_called ();
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network2 ~host:other_host ~device:"pifX" () in
  assert_not_called ();

  (* Add another PIF to network2 connected to another host *)
  received_params := None;
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network2 ~host:other_host ~device:"pifY" () in
  assert_not_called ();

  (* Add a PIF to network2 connected to this host *)
  received_params := None;
  let pif3 = Test_common.make_pif ~__context ~network:network2 ~host:localhost ~device:"pif3" () in
  assert_received_params ["pif1"; "pif2"; "pif3"];

  (* Remove pif1 on network1 *)
  received_params := None;
  Db.PIF.destroy ~__context ~self:pif1;
  assert_received_params ["pif2"; "pif3"];

  (* Remove network1 *)
  received_params := None;
  Db.Network.destroy ~__context ~self:network1;
  assert_received_params ["pif3"];

  (* Remove pif3 on network2 *)
  received_params := None;
  fail_firewall_update := true;
  Db.PIF.destroy ~__context ~self:pif3;
  (* After [delay] seconds, the event loop should have noticed the change and
     failed after trying to update the firewall *)
  Thread.delay delay;
  fail_firewall_update := false;
  (* Test that we do wait for the given delay in case of failures *)
  assert_not_called ();
  (* Now wait for the event loop to reregister and continue *)
  Thread.delay network_event_loop_wait_after_failure_seconds;
  (* Test that transient failures in the script won't stop the event loop, and
     that we will eventually process the event we missed, and update the
     firewall with the correct list of interfaces, even if it is the empty list *)
  assert_received_params []

let test =
  let ((>:::), (>::)) = OUnit.((>:::), (>::)) in
  "test_network_event_loop" >:::
  [ "test_network_event_loop_with_no_networks_at_start" >:: (test_network_event_loop ~no_networks_at_start:true)
  ; "test_network_event_loop_with_some_networks_at_start" >:: (test_network_event_loop ~no_networks_at_start:false)
  ]
