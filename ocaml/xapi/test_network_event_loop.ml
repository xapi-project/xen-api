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

let test_network_event_loop ~no_nbd_networks_at_start () =
  let __context, _ = Test_event_common.event_setup_common () in

  let localhost = Helpers.get_localhost ~__context in
  let other_host = Test_common.make_host ~__context () in

  (* We have to wait for a bit for the event loop to notice the changes, without a delay the test will fail. *)
  let delay = 0.2 in
  let network_event_loop_wait_after_failure_seconds = 0.4 in
  let received_params = ref None in

  (* We simulate failure of the firewall update script this way *)
  let fail_firewall_update = ref false in

  let start_event_loop = Thread.create
      (fun () ->
         Network_event_loop._watch_networks_for_nbd_changes
           __context
           ~update_firewall:(fun pifs ->
               if !fail_firewall_update then failwith "Failed to update firewall";
               received_params := Some pifs
             )
           ~wait_after_event_seconds:0.0
           ~wait_after_failure_seconds:network_event_loop_wait_after_failure_seconds
      )
  in

  let assert_received_params msg expected =
    Thread.delay delay;
    match !received_params with
    | None -> OUnit.assert_failure ("The update_firewall function was not called: " ^ msg)
    | Some p ->
      Ounit_comparators.StringSet.(assert_equal (of_list expected) (of_list p))
  in

  let assert_not_called msg () =
    Thread.delay delay;
    OUnit.assert_equal ~msg:("update_firewall shouldn't have been called: " ^ msg) None !received_params
  in

  let network1 = Test_common.make_network ~__context ~bridge:"bridge1" () in

  if no_nbd_networks_at_start then begin
    received_params := None;
    let _ : Thread.t = start_event_loop () in
    assert_received_params "The event loop should always update the firewall state at startup, even if no networks are allowed for NBD" []
  end;

  (* Add a new network "network1" with a PIF connected to this host *)
  received_params := None;
  Db.Network.set_purpose ~__context ~self:network1 ~value:[`nbd];
  assert_not_called "network1 has no PIF connected to this host" ();
  let pif1 = Test_common.make_pif ~__context ~network:network1 ~host:localhost ~device:"network1_pif1" () in
  if not no_nbd_networks_at_start then begin
    let _ : Thread.t = start_event_loop () in ()
  end;
  assert_received_params "network1 is now connected to this host with pif1" ["bridge1"];

  (* Add another PIF to network1 connected to this host *)
  received_params := None;
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network1 ~host:localhost ~device:"network1_pif2" () in
  assert_not_called "We've already enabled NBD on this network's bridge" ();

  received_params := None;
  Db.Network.set_name_description ~__context ~self:network1 ~value:"irrelevant network object modification";
  assert_not_called "only the network1's name label was modified" ();

  (* Add a new network "network2" with a PIF connected to this host *)
  received_params := None;
  fail_firewall_update := true;
  let network2 = Test_common.make_network ~__context ~purpose:[`nbd] ~bridge:"bridge2" () in
  assert_not_called "network2 has no PIF connected to this host" ();
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network2 ~host:localhost ~device:"network2_pif1" () in
  (* After [delay] seconds, the event loop should have noticed the change and
     failed after trying to update the firewall *)
  Thread.delay delay;
  fail_firewall_update := false;
  (* Test that we do wait for the given delay in case of failures *)
  assert_not_called "The event loop should wait for the given delay in case of failures" ();
  (* Now wait for the event loop to reregister and continue *)
  Thread.delay network_event_loop_wait_after_failure_seconds;
  (* Test that transient failures in the script won't stop the event loop, and
     that we will eventually process the event we missed *)
  assert_received_params "we should have noticed the addition of network2 and its PIF after the transient failure" ["bridge1"; "bridge2"];

  (* Add a new network "network3" with a PIF connected to another host *)
  received_params := None;
  let network3 = Test_common.make_network ~__context ~purpose:[`nbd] ~bridge:"bridge3" () in
  assert_not_called "network3 has no PIFs at all" ();
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network3 ~host:other_host ~device:"network3_pif1" () in
  assert_not_called "pif1 of network3 is connected to another host" ();

  (* Add another PIF to network3 connected to another host *)
  received_params := None;
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network3 ~host:other_host ~device:"network3_pif2" () in
  assert_not_called "pif2 of network3 is connected to another host" ();

  (* Add a PIF to network3 connected to this host *)
  received_params := None;
  let pif3 = Test_common.make_pif ~__context ~network:network3 ~host:localhost ~device:"network3_pif3" () in
  assert_received_params "network3 is now connected to this host by pif3" ["bridge1"; "bridge2"; "bridge3"];

  (* Remove pif1 on network1 *)
  received_params := None;
  Db.PIF.destroy ~__context ~self:pif1;
  assert_not_called "network1 still has a PIF connected to this host, so we shouldn't disallow its bridge" ();

  (* Remove network1 *)
  received_params := None;
  Db.Network.destroy ~__context ~self:network1;
  assert_received_params "network1 has been removed" ["bridge2"; "bridge3"];

  (* Remove pif3 of network3 *)
  received_params := None;
  Db.PIF.destroy ~__context ~self:pif3;
  (* This was network3's only PIF connected to this host, so we should have disabled NBD on network3's bridge *)
  assert_received_params "network3's only PIF connected to this host has been removed" ["bridge2"];

  (* Remove network2 *)
  received_params := None;
  fail_firewall_update := true;
  Db.Network.destroy ~__context ~self:network2;
  (* After [delay] seconds, the event loop should have noticed the change and
     failed after trying to update the firewall *)
  Thread.delay delay;
  fail_firewall_update := false;
  assert_not_called "The event loop should wait for the given delay in case of failures" ();
  (* Now wait for the event loop to reregister and continue *)
  Thread.delay network_event_loop_wait_after_failure_seconds;
  (* Test that transient failures in the script won't stop the event loop, and
     that we will eventually process the event we missed, and update the
     firewall with the correct list of interfaces, even if it is the empty list *)
  assert_received_params "no network is connected to this host now, we should have noticed this after the transient failure" [];

  (* Add a new network "network4" with a PIF connected to this host on which NBD is not allowed *)
  received_params := None;
  let network4 = Test_common.make_network ~__context ~purpose:[] ~bridge:"bridge4" () in
  assert_not_called "network4 has no PIF connected to this host" ();
  let _ : _ API.Ref.t = Test_common.make_pif ~__context ~network:network4 ~host:localhost ~device:"network4_pif1" () in
  assert_not_called "NBD is not allowed on network4" ();

  (* Now enable secure NBD on network4 *)
  received_params := None;
  Db.Network.set_purpose ~__context ~self:network4 ~value:[`nbd];
  assert_received_params "NBD has been enabled on network4" ["bridge4"];

  (* Disable NBD on network4 *)
  received_params := None;
  Db.Network.set_purpose ~__context ~self:network4 ~value:[];
  assert_received_params "NBD has been disabled on network4" [];

  (* Now enable insecure NBD on network4 *)
  received_params := None;
  Db.Network.set_purpose ~__context ~self:network4 ~value:[`insecure_nbd];
  assert_received_params "NBD has been enabled on network4" ["bridge4"]

let test =
  let ((>:::), (>::)) = OUnit.((>:::), (>::)) in
  "test_network_event_loop" >:::
  [ "test_network_event_loop_with_no_networks_at_start" >:: (test_network_event_loop ~no_nbd_networks_at_start:true)
  ; "test_network_event_loop_with_some_networks_at_start" >:: (test_network_event_loop ~no_nbd_networks_at_start:false)
  ]
