(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Xstringext
open Threadext
open Pervasiveext
open Fun
open Client
open Event_types
open Quicktest_common

let username = ref ""
let password = ref ""

let export_filename = "/tmp/quicktest-export"

(* CA-11402 *)
let event_next_unblocking_test () =
  let test = make_test "Event.next unblocking test" 0 in
  start test;
  (* Need to create a temporary session ID *)
  let session_id = init_session !username !password in
  let () = Client.Event.register !rpc session_id [] in (* no events *)
  let m = Mutex.create () in
  let unblocked = ref false in
  let (_: Thread.t) = Thread.create
      (fun () ->
         begin
           try ignore(Client.Event.next !rpc session_id)
           with e ->
             debug test (Printf.sprintf "background thread caught: %s (an exception is expected)" (Printexc.to_string e))
         end;
         Mutex.execute m (fun () -> unblocked := true)
      ) () in
  (* Background thread is started but it cannot simultaneously block and signal us to
     logout so a little pause in here is probably the best we can do *)
  Thread.delay 2.;
  (* Logout which should cause the background thread to unblock *)
  Client.Session.logout !rpc session_id;
  (* Again we can't tell the difference between a slow and a totally blocked thread
     so a little pause in here is also required *)
  Thread.delay 2.;
  if not (Mutex.execute m (fun () -> !unblocked))
  then failed test ""
  else success test

let event_next_test session_id =
  let test = make_test "Event.next test" 0 in
  start test;
  let () = Client.Event.register !rpc session_id [ "pool" ] in
  let m = Mutex.create () in
  let finished = ref false in
  let pool = Client.Pool.get_all !rpc session_id |> List.hd in
  let key = "event_next_test" in
  begin try Client.Pool.remove_from_other_config !rpc session_id pool key with _ -> () end;
  let (_: Thread.t) = Thread.create
      (fun () ->
         while not (Mutex.execute m (fun () -> !finished)) do
           ignore (Client.Event.next !rpc session_id);
           let oc = Client.Pool.get_other_config !rpc session_id pool in
           if List.mem_assoc key oc && (List.assoc key oc) = "1"
           then Mutex.execute m (fun () ->
               debug test "got expected event";
               finished := true;
             )
         done
      ) () in
  Thread.delay 1.;
  Client.Pool.add_to_other_config !rpc session_id pool key "1";
  Thread.delay 1.;
  if not(Mutex.execute m (fun () -> !finished))
  then failed test "failed to see pool.other_config change"
  else success test

let wait_for_pool_key test session_id key =
  let token = ref "" in
  let finished = ref false in
  let pool = Client.Pool.get_all !rpc session_id |> List.hd in
  while not !finished do
    let events = Client.Event.from !rpc session_id [ "pool" ] (!token) 10. |> event_from_of_rpc in
    token := events.token;
    let oc = Client.Pool.get_other_config !rpc session_id pool in
    if List.mem_assoc key oc && (List.assoc key oc) = "1" then finished := true;
  done

let event_from_test session_id =
  let test = make_test "Event.from test" 0 in
  start test;
  let m = Mutex.create () in
  let finished = ref false in
  let pool = Client.Pool.get_all !rpc session_id |> List.hd in
  let key = "event_next_test" in
  begin try Client.Pool.remove_from_other_config !rpc session_id pool key with _ -> () end;
  let (_: Thread.t) = Thread.create
      (fun () ->
         wait_for_pool_key test session_id key;
         Mutex.execute m (fun () -> finished := true)
      ) () in
  Thread.delay 1.;
  Client.Pool.add_to_other_config !rpc session_id pool key "1";
  Thread.delay 1.;
  if not(Mutex.execute m (fun () -> !finished))
  then failed test "failed to see pool.other_config change"
  else success test

let event_from_parallel_test session_id =
  let test = make_test "Event.from parallel test" 0 in
  start test;
  let pool = Client.Pool.get_all !rpc session_id |> List.hd in
  let key = "event_next_test" in
  begin try Client.Pool.remove_from_other_config !rpc session_id pool key with _ -> () end;
  let ok = ref true in
  let (i_should_succeed: Thread.t) = Thread.create
      (fun () ->
         try
           let _ = Client.Event.from !rpc session_id [] "" 10. in
           () (* good *)
         with e ->
           debug test (Printexc.to_string e);
           ok := false;
      ) () in
  let (interfering_thread: Thread.t) = Thread.create
      (fun () ->
         wait_for_pool_key test session_id key
      ) () in
  Thread.delay 1.; (* wait for both threads to block in Event.from *)
  Client.Pool.add_to_other_config !rpc session_id pool key "1";
  Thread.join interfering_thread;
  Thread.join i_should_succeed;
  if not !ok
  then failed test "Event.from got cancelled by mistake"
  else success test

let object_level_event_test session_id =
  let test = make_test "Event.from object-level test" 0 in
  start test;
  let m = Mutex.create () in
  let finished = ref false in
  let reported_failure = ref false in
  (* Let's play with templates *)
  let vms = Client.VM.get_all !rpc session_id in
  if List.length vms < 2 then failwith "Test needs 2 VMs";
  let vm_a = List.hd vms in
  let vm_b = List.hd (List.tl vms) in
  debug test (Printf.sprintf "watching %s" (Ref.string_of vm_a));
  debug test (Printf.sprintf "ignoring %s" (Ref.string_of vm_b));
  let key = "object_level_event_next" in
  begin try Client.VM.remove_from_other_config !rpc session_id vm_a key with _ -> () end;
  begin try Client.VM.remove_from_other_config !rpc session_id vm_b key with _ -> () end;

  let (_: Thread.t) = Thread.create
      (fun () ->
         let token = ref "" in
         while not (Mutex.execute m (fun () -> !finished)) do
           let events = Client.Event.from !rpc session_id [ Printf.sprintf "vm/%s" (Ref.string_of vm_a) ] (!token) 10. |> event_from_of_rpc in
           List.iter
             (fun event ->
                if event.reference <> Ref.string_of vm_a then begin
                  debug test (Printf.sprintf "event on %s which we aren't watching" event.reference);
                  Mutex.execute m
                    (fun () ->
                       reported_failure := true;
                       failed test (Printf.sprintf "got unexpected event (new token = %s)" !token);
                       finished := true;
                    )
                end
             ) events.events;
           token := events.token;
           let oc = Client.VM.get_other_config !rpc session_id vm_a in
           if List.mem_assoc key oc && (List.assoc key oc) = "1"
           then Mutex.execute m (fun () ->
               debug test (Printf.sprintf "got expected event (new token = %s)" !token);
               finished := true;
             );
         done
      ) () in
  Thread.delay 1.;
  Client.VM.add_to_other_config !rpc session_id vm_b key "1";
  Thread.delay 1.;
  Client.VM.remove_from_other_config !rpc session_id vm_b key;
  Client.VM.add_to_other_config !rpc session_id vm_a key "1";
  Thread.delay 1.;
  Mutex.execute m
    (fun () ->
       if not (!reported_failure) then begin
         if !finished
         then success test
         else failed test "failed to see object-level event change"
       end
    )

let event_message_test session_id =
  let test = make_test "Message creation event test" 1 in
  start test;
  let events = Client.Event.from !rpc session_id [ "message" ] "" 1.0 |> event_from_of_rpc in
  let token = events.token in
  let pool = List.hd (Client.Pool.get_all !rpc session_id) in
  let obj_uuid = Client.Pool.get_uuid !rpc session_id pool in
  debug test "Creating message";
  let cls = `Pool in
  let message = Client.Message.create ~rpc:!rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  debug test (Printf.sprintf "Created message: %s" (Ref.string_of message));
  let events = Client.Event.from !rpc session_id [ "message" ] token 1.0 |> event_from_of_rpc
  in
  debug test (Printf.sprintf "Got some events: %d %s" (List.length events.events) (String.concat "," (List.map (fun ev -> ev.reference) events.events)));
  let token = events.token in
  if List.exists (fun ev -> ev.reference = (Ref.string_of message) && ev.op = `add) events.events
  then success test
  else failed test "Failed to receive an event with the message";

  let test = make_test "Message deletion event test" 1 in
  start test;
  debug test "Destroying message";
  Client.Message.destroy !rpc session_id message;
  let events = Client.Event.from !rpc session_id [ "message" ] token 1.0 |> event_from_of_rpc in
  debug test "Got some events";
  if List.exists (fun ev -> ev.reference = (Ref.string_of message) && ev.op = `del) events.events
  then success test
  else failed test "Failed to receive a delete event";

  let test = make_test "Message deletion from cache test" 1 in
  start test;
  let events = Client.Event.from !rpc session_id [ "message" ] "" 1.0 |> event_from_of_rpc in
  debug test "Got lots of events";
  if List.exists (fun ev -> ev.reference = (Ref.string_of message) && ev.op <> `del) events.events
  then failed test "Got told about a deleted message"
  else success test;

  let test = make_test "Multi message test" 1 in
  start test;
  let message1 = Client.Message.create ~rpc:!rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  let message2 = Client.Message.create ~rpc:!rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  let events = Client.Event.from !rpc session_id [ "message" ] token 1.0 |> event_from_of_rpc in
  let token = events.token in
  let message3 = Client.Message.create ~rpc:!rpc ~session_id ~name:"test" ~priority:1L ~cls
      ~obj_uuid ~body:"Hello" in
  let events2 = Client.Event.from !rpc session_id [ "message" ] token 1.0 |> event_from_of_rpc in
  debug test (Printf.sprintf "message1=%s" (Ref.string_of message1));
  debug test (Printf.sprintf "message2=%s" (Ref.string_of message2));
  debug test (Printf.sprintf "message3=%s" (Ref.string_of message3));
  List.iter (fun ev -> debug test (Printf.sprintf "events1: ev.ref=%s" ev.reference)) events.events;
  List.iter (fun ev -> debug test (Printf.sprintf "events2: ev.ref=%s" ev.reference)) events2.events;
  let ok1 =
    List.exists (fun ev -> ev.reference = (Ref.string_of message1) && ev.op = `add) events.events &&
    List.exists (fun ev -> ev.reference = (Ref.string_of message2) && ev.op = `add) events.events in
  let ok2 =
    List.exists (fun ev -> ev.reference = (Ref.string_of message3) && ev.op = `add) events2.events in
  let ok3 =
    not (List.exists (fun ev -> ev.reference = (Ref.string_of message1) && ev.op = `add) events2.events) &&
    not (List.exists (fun ev -> ev.reference = (Ref.string_of message2) && ev.op = `add) events2.events)
  in
  if ok1 && ok2 && ok3 then success test else failed test (Printf.sprintf "ok1=%b ok2=%b ok3=%b" ok1 ok2 ok3);

  let test = make_test "Object messages test" 1 in
  start test;
  debug test (Printf.sprintf "Finding messages for object: %s" (Client.Pool.get_uuid !rpc session_id pool));
  let messages = Client.Message.get ~rpc:!rpc ~session_id  ~cls ~obj_uuid ~since:(Date.never) in
  let has_msg m = List.exists (fun (r,_) -> r=m) messages in
  let ok = has_msg message1 && has_msg message2 && has_msg message3 in
  if ok then success test else failed test "Failed to get messages for object"

let event_inject_test session_id =
  let test = make_test "Event.inject test" 0 in
  start test;
  let events = Client.Event.from !rpc session_id [ "pool" ] "" 1.0 |> event_from_of_rpc in
  let token = events.token in
  let pool = List.hd (Client.Pool.get_all !rpc session_id) in
  let starttime = Unix.gettimeofday () in
  let (x: Thread.t) = Thread.create
      (fun () ->
         let _ = Client.Event.from !rpc session_id [ "pool" ] token 5.0 in
         ()
      ) () in
  ignore(Client.Event.inject ~rpc:!rpc ~session_id ~_class:"pool" ~_ref:(Ref.string_of pool));
  Thread.join x;
  let endtime = Unix.gettimeofday () in
  if endtime -. starttime > 4.5
  then failed test "Failed to see injected event"
  else success test

module StringSet=Set.Make(String)

let event_from_number_test session_id =
  let test = make_test "Event.from test" 0 in
  start test;
  let events = Client.Event.from !rpc session_id [ "vm" ] "" 10. |> event_from_of_rpc in
  let (_,f) = List.fold_left (fun (set,failed) ev ->
      let reference = ev.reference in
      if StringSet.mem reference set
      then (set,true)
      else (StringSet.add reference set, failed)) (StringSet.empty, false) events.events in
  if f
  then failed test "Object seen twice in events"
  else success test

let all_srs_with_vdi_create session_id =
  Quicktest_storage.list_srs session_id
  (* Filter out those which support the vdi_create capability *)
  |> List.filter (fun sr -> List.mem Quicktest_storage.vdi_create (Quicktest_storage.sm_caps_of_sr session_id sr))
  (* Filter out those without the allowed operation *)
  |> List.filter (fun sr -> List.mem `vdi_create (Client.SR.get_allowed_operations !rpc session_id sr))
  (* Filter out those with content-type = iso (this confuses the import test logic) *)
  |> List.filter (fun sr -> Client.SR.get_content_type !rpc session_id sr <> "iso")

(** Create a small VM with a selection of CDs, empty drives, "iso" Disks etc *)
let setup_export_test_vm session_id =
  let test = make_test "Setting up test VM" 1 in
  start test;
  let t = find_template session_id other in
  let uuid = Client.VM.get_uuid !rpc session_id t in
  debug test (Printf.sprintf "Template has uuid: %s%!" uuid);
  let vm = vm_install test session_id uuid "quicktest-export" in
  debug test (Printf.sprintf "Installed new VM");
  let cd =
    let tools_iso_filter = "field \"is_tools_iso\"=\"true\"" in
    match Client.VDI.get_all_records_where !rpc session_id tools_iso_filter with
    | (vdi, _)::_ -> vdi
    | [] ->
      failed test "Failed to find tools ISO VDI";
      failwith "setup_export_test_vm";
  in
  debug test "Looking for the SR which supports the smallest disk size";
  let all_srs = all_srs_with_vdi_create session_id in
  let smallest : int64 option list = List.map (fun sr -> Quicktest_storage.find_smallest_disk_size session_id sr) all_srs in
  let sr_names = List.map (Quicktest_storage.name_of_sr session_id) all_srs in
  List.iter (function
      | sr, Some size -> debug test (Printf.sprintf "SR %s has minimum disk size: %Ld" sr size)
      | sr, None -> debug test (Printf.sprintf "SR %s has no minimum disk size!" sr)
    ) (List.combine sr_names smallest);
  let minimum = List.fold_left min (1L ** gib) (List.map (fun x -> Opt.default (1L ** gib) x) smallest) in
  let sr =
    match List.filter (fun (_, size) -> size = Some minimum) (List.combine all_srs smallest) with
    | (sr, _)::_ -> sr
    | [] ->
      failed test "Failed to find an SR which can create a VDI";
      failwith "setup_export_test_vm";
  in
  debug test (Printf.sprintf "Using a disk size of: %Ld on SR: %s" minimum (Quicktest_storage.name_of_sr session_id sr));
  let vdi = Client.VDI.create !rpc session_id "small"
      "description" sr 4194304L `user false false [] [] [] [] in
  ignore(Client.VBD.create ~rpc:!rpc ~session_id ~vM:vm ~vDI:cd ~userdevice:"0" ~bootable:false
           ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
  ignore(Client.VBD.create ~rpc:!rpc ~session_id ~vM:vm ~vDI:cd ~userdevice:"1" ~bootable:false
           ~mode:`RO ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
  ignore(Client.VBD.create ~rpc:!rpc ~session_id ~vM:vm ~vDI:cd ~userdevice:"2" ~bootable:false
           ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:true ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
  ignore(Client.VBD.create ~rpc:!rpc ~session_id ~vM:vm ~vDI:vdi ~userdevice:"3" ~bootable:false
           ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[Xapi_globs.owner_key,""]
           ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
  success test;
  vm

let all_non_iso_srs_with_vdi_create session_id =
  List.filter
    (fun sr -> "iso" <> Client.SR.get_content_type !rpc session_id sr)
    (all_srs_with_vdi_create session_id)

let import_export_test session_id =
  let test = make_test "VM import/export test" 0 in
  start test;
  let vm = setup_export_test_vm session_id in
  let by_device = List.map (fun vbd -> Client.VBD.get_userdevice !rpc session_id vbd, vbd) (Client.VM.get_VBDs !rpc session_id vm) in

  Unixext.unlink_safe export_filename;
  vm_export test session_id vm export_filename;
  let all_srs = all_non_iso_srs_with_vdi_create session_id in
  List.iter
    (fun sr ->
       debug test (Printf.sprintf "Attempting import to SR: %s" (Quicktest_storage.name_of_sr session_id sr));
       let vm' = List.hd (vm_import ~sr test session_id export_filename) in
       let vbds = Client.VM.get_VBDs !rpc session_id vm' in

       if List.length vbds <> (List.length by_device) then failed test "Wrong number of VBDs after import";
       List.iter (fun vbd ->
           let all = Client.VBD.get_record !rpc session_id vbd in
           let orig_vbd = List.assoc all.API.vBD_userdevice by_device in
           let orig_vbd = Client.VBD.get_record !rpc session_id orig_vbd in

           (* type, empty should match *)
           if all.API.vBD_type <> orig_vbd.API.vBD_type
           then failed test (Printf.sprintf "Device %s varies in type" all.API.vBD_userdevice);
           if all.API.vBD_empty <> orig_vbd.API.vBD_empty
           then failed test (Printf.sprintf "Device %s varies in emptiness" all.API.vBD_userdevice);
           match all.API.vBD_userdevice with
           | "0" | "1" | "2" ->
             (* VDI should be the same *)
             if all.API.vBD_VDI <> orig_vbd.API.vBD_VDI
             then failed test (Printf.sprintf "Device %s varies in VDIness (original = %s; new = %s)" all.API.vBD_userdevice (Client.VDI.get_uuid !rpc session_id orig_vbd.API.vBD_VDI) (Client.VDI.get_uuid !rpc session_id all.API.vBD_VDI));
           | "3" ->
             (* VDI should be different *)
             if all.API.vBD_VDI = orig_vbd.API.vBD_VDI
             then failed test (Printf.sprintf "Device %s should not vary in VDIness" all.API.vBD_userdevice)
           | _ -> failed test (Printf.sprintf "Unhandled device number: %s" all.API.vBD_userdevice)) vbds;
       vm_uninstall test session_id vm'
    ) all_srs;
  vm_uninstall test session_id vm;
  Unix.unlink export_filename;
  success test

(* Expect that two VMs have identical looking VIFs, mapped to the same Networks *)
let compare_vifs session_id test one two =
  let one_vifs = Client.VM.get_VIFs !rpc session_id one in
  let two_vifs = Client.VM.get_VIFs !rpc session_id two in
  if List.length one_vifs <> (List.length two_vifs) then begin
    failed test (Printf.sprintf "Original VM had %d VIFs; clone has %d VIFs"
                   (List.length one_vifs) (List.length two_vifs));
    failwith "powercycle_test"
  end;
  let one_vifs = List.filter (fun vif -> Client.VIF.get_currently_attached !rpc session_id vif) one_vifs in
  let two_vifs = List.filter (fun vif -> Client.VIF.get_currently_attached !rpc session_id vif) two_vifs in
  if List.length one_vifs <> (List.length two_vifs) then begin
    failed test (Printf.sprintf "Original VM had %d currently_attached VIFs; clone has %d currently_attached VIFs"
                   (List.length one_vifs) (List.length two_vifs));
    failwith "powercycle_test"
  end;
  (* look up two's VIFs by their device name *)
  let by_device = List.map (fun vif -> Client.VIF.get_device !rpc session_id vif, vif) two_vifs in
  List.iter (fun vif ->
      let dev = Client.VIF.get_device !rpc session_id vif in
      if not(List.mem_assoc dev by_device) then begin
        failed test (Printf.sprintf "Original VM has attached VIF device %s; clone has no" dev);
        failwith "powercycle_test"
      end;
      let vif' = List.assoc dev by_device in
      let one_net = Client.VIF.get_network !rpc session_id vif
      and two_net = Client.VIF.get_network !rpc session_id vif' in
      if one_net <> two_net then begin
        failed test (Printf.sprintf "Original VM has attached VIF device %s plugged into Network %s; clone has Network %s" dev (Client.Network.get_uuid !rpc session_id one_net) (Client.Network.get_uuid !rpc session_id two_net));
        failwith "powercycle_test"
      end) one_vifs

(* Expect that two VMs have identical looking VBDs, mapped to the same VDIs *)
let compare_vbds session_id test one two =
  let one_vbds = Client.VM.get_VBDs !rpc session_id one in
  let two_vbds = Client.VM.get_VBDs !rpc session_id two in
  if List.length one_vbds <> (List.length two_vbds) then begin
    failed test (Printf.sprintf "Original VM had %d VBDs; clone has %d VBDs"
                   (List.length one_vbds) (List.length two_vbds));
    failwith "powercycle_test"
  end;
  let one_vbds = List.filter (fun vbd -> Client.VBD.get_currently_attached !rpc session_id vbd) one_vbds in
  let two_vbds = List.filter (fun vbd -> Client.VBD.get_currently_attached !rpc session_id vbd) two_vbds in
  if List.length one_vbds <> (List.length two_vbds) then begin
    failed test (Printf.sprintf "Original VM had %d currently_attached VBDs; clone has %d currently_attached VBDs"
                   (List.length one_vbds) (List.length two_vbds));
    failwith "powercycle_test"
  end;
  (* look up two's VBDs by their device name *)
  let by_device = List.map (fun vbd -> Client.VBD.get_userdevice !rpc session_id vbd, vbd) two_vbds in
  List.iter (fun vbd ->
      let dev = Client.VBD.get_userdevice !rpc session_id vbd in
      if not(List.mem_assoc dev by_device) then begin
        failed test (Printf.sprintf "Original VM has attached VBD device %s; clone has no" dev);
        failwith "powercycle_test"
      end;
      let vbd' = List.assoc dev by_device in
      let one_vdi = Client.VBD.get_VDI !rpc session_id vbd
      and two_vdi = Client.VBD.get_VDI !rpc session_id vbd' in
      if one_vdi <> two_vdi then begin
        failed test (Printf.sprintf "Original VM has attached VBD device %s plugged into VDI %s; clone has VDI %s" dev (Client.VDI.get_uuid !rpc session_id one_vdi) (Client.VDI.get_uuid !rpc session_id two_vdi));
        failwith "powercycle_test"
      end) one_vbds

let compare_vms session_id test one two =
  let one_r = Client.VM.get_record !rpc session_id one
  and two_r = Client.VM.get_record !rpc session_id two in

  (* check the power-state field *)
  if one_r.API.vM_power_state <> two_r.API.vM_power_state then begin
    failed test (Printf.sprintf "Original VM powerstate = %s; copy has %s"
                   (Record_util.power_to_string one_r.API.vM_power_state)
                   (Record_util.power_to_string two_r.API.vM_power_state));
    failwith "powercycle_test";
  end;

  (* Check one 'normal' field and one 'last_boot_record' field *)
  if one_r.API.vM_HVM_shadow_multiplier <> two_r.API.vM_HVM_shadow_multiplier then begin
    failed test (Printf.sprintf "Original VM has shadow_multiplier = %f; copy has %f"
                   one_r.API.vM_HVM_shadow_multiplier two_r.API.vM_HVM_shadow_multiplier);
    failwith "powercycle_test"
  end;

  if one_r.API.vM_power_state <> `Halted then begin
    let one_b = Client.VM.get_boot_record !rpc session_id one
    and two_b = Client.VM.get_boot_record !rpc session_id two in
    if one_b.API.vM_HVM_shadow_multiplier <> two_b.API.vM_HVM_shadow_multiplier then begin
      failed test (Printf.sprintf "Original VM has live shadow_multiplier = %f; copy has %f"
                     one_b.API.vM_HVM_shadow_multiplier two_b.API.vM_HVM_shadow_multiplier);
      failwith "powercycle_test"
    end
  end;

  (* check snapshot fields *)
  if one_r.API.vM_is_a_snapshot <> two_r.API.vM_is_a_snapshot ||
     one_r.API.vM_is_a_template <> two_r.API.vM_is_a_template ||
     one_r.API.vM_snapshot_time <> two_r.API.vM_snapshot_time then begin
    failed test (Printf.sprintf
                   "Original VM has snapshot metadata: is-a-snapshot:%b, is-a-template:%b, snapshot-time:%s; copy has is-a-snapshot:%b, is-a-template:%b, snapshot-time:%s"
                   one_r.API.vM_is_a_snapshot one_r.API.vM_is_a_template (Date.to_string one_r.API.vM_snapshot_time)
                   two_r.API.vM_is_a_snapshot two_r.API.vM_is_a_template (Date.to_string two_r.API.vM_snapshot_time));
    failwith "powercycle_test";
  end

let compare_snapshots session_id test one two =
  let get_snapshots x = Client.VM.get_snapshots !rpc session_id x in
  let sort l =
    let lt = List.map (fun s -> s, Client.VM.get_snapshot_time !rpc session_id s) l in
    let lt_sorted = List.sort (fun (s1, t1) (s2, t2) -> compare t1 t2) lt in
    let l_sorted, _ = List.split lt_sorted in
    l_sorted in
  let one_s = sort (get_snapshots one) in
  let two_s = sort (get_snapshots two) in
  let compare_all x y =
    compare_vifs session_id test x y;
    compare_vbds session_id test x y;
    compare_vms session_id test x y in
  List.iter2 compare_all one_s two_s

let read_sys path = Xstringext.String.strip Xstringext.String.isspace (Unixext.string_of_file path)

let verify_network_connectivity session_id test vm =
  let vifs = Client.VM.get_VIFs !rpc session_id vm in
  List.iter
    (fun vif ->
       let network = Client.VIF.get_network !rpc session_id vif in
       let bridge = Client.Network.get_bridge !rpc session_id network in
       let device = Printf.sprintf "vif%Ld.%s" (Client.VM.get_domid !rpc session_id vm) (Client.VIF.get_device !rpc session_id vif) in
       let devices = Netdev.network.Netdev.intf_list bridge in
       let other_config = Client.VIF.get_other_config !rpc session_id vif in
       if not(List.mem device devices)
       then failed test (Printf.sprintf "Failed to find device %s on bridge %s (found [ %s ])" device bridge (String.concat ", " devices))
       else debug test (Printf.sprintf "Device %s is on bridge %s" device bridge);

       (* Check the udev script set promiscuous mode correctly, IFF brport/promisc exists in sysfs. *)
       let sysfs_promisc = Printf.sprintf "/sys/class/net/%s/brport/promisc" device in
       if Sys.file_exists sysfs_promisc
       then begin
         let promisc = List.mem_assoc "promiscuous" other_config && (let x = List.assoc "promiscuous" other_config in x = "true" || x = "on") in
         let promisc' = read_sys sysfs_promisc = "1" in
         if promisc <> promisc'
         then failed test (Printf.sprintf "VIF.other_config says promiscuous mode is %b while dom0 /sys says %b" promisc promisc')
         else debug test (Printf.sprintf "VIF.other_config and dom0 /sys agree that promiscuous mode is %b" promisc);
       end else
         debug test (Printf.sprintf "%s not found. assuming unsupported" sysfs_promisc);

       (* Check the MTU *)
       let mtu = Client.Network.get_MTU !rpc session_id network in
       let mtu' = if List.mem_assoc "mtu" other_config
         then Int64.of_string(List.assoc "mtu" other_config) else mtu in
       let mtu'' = Int64.of_string (read_sys (Printf.sprintf "/sys/class/net/%s/mtu" device)) in
       if mtu' <> mtu''
       then failed test (Printf.sprintf "VIF.MTU is %Ld but /sys says %Ld" mtu' mtu'')
       else debug test (Printf.sprintf "VIF.MTU is %Ld and /sys says %Ld" mtu' mtu'');
    ) vifs

let rec wait_for_task_complete session_id task =
  Thread.delay 1.;
  match Client.Task.get_status !rpc session_id task with
  | `pending | `cancelling -> wait_for_task_complete session_id task
  | _ -> ()

(* CP-831 *)
let test_vhd_locking_hook session_id vm =
  let test = make_test "test vhd locking hook" 2 in
  start test;
  Client.VM.start !rpc session_id vm false false;
  (* Add a new VDI whose VBD is unplugged (so 2 plugged, 1 unplugged *)

  let all_srs = all_srs_with_vdi_create session_id in
  let sr = List.hd all_srs in

  let new_vdi = Client.VDI.create !rpc session_id "lvhd_testvdi"
      "description" sr 4194304L `user false false [] [] [] [] in
  let new_vbd = Client.VBD.create ~rpc:!rpc ~session_id ~vM:vm ~vDI:new_vdi ~userdevice:"9" ~bootable:false
      ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[Xapi_globs.owner_key,""]
      ~qos_algorithm_type:"" ~qos_algorithm_params:[] in

  (* In a background thread plug/unplug the new VBD to cause some transient locking failures *)
  let start = Unix.gettimeofday () in
  debug test "Starting up conflicting thread in the background";
  let total_bg_ops = ref 0 in
  let t = Thread.create
      (fun () ->
         while Unix.gettimeofday () -. start < 30. do
           (* We throw away exceptions because unplugs can fail (if the guest isn't ready) and this causes the
              				   next plug to fail. We use asynchronous operations because we are sharing a single HTTP connection to the
              				   master and we genuinely want the operations to (attempt to) execute in parallel *)
           let task = Client.Async.VBD.plug !rpc session_id new_vbd in
           incr total_bg_ops;
           wait_for_task_complete session_id task;
           let task = Client.Async.VBD.unplug !rpc session_id new_vbd in
           incr total_bg_ops;
           wait_for_task_complete session_id task
         done) () in
  (* Give the background thread a chance to start *)
  Thread.delay 1.5;
  (* Verify that the function 'test' can be called in the script *)

  Thread.join t;
  debug test (Printf.sprintf "Meanwhile background thread executed %d conflicting operations" !total_bg_ops);
  success test

let powercycle_test session_id vm =
  let test = make_test "Powercycling VM" 1 in
  start test;
  (* avoid the race whereby reboot requests are ignored if too early *)
  let delay () =
    debug test "Pausing for 10s";
    Thread.delay 10. in
  debug test (Printf.sprintf "Trying to enable VM.clone for suspended VMs pool-wide");
  let pool = get_pool session_id in
  let enabled_csvm =
    try Client.Pool.add_to_other_config !rpc session_id pool "allow_clone_suspended_vm" "true"; true
    with _ -> false in
  finally
    (fun () ->
       (* We play with three VMs:
          			   1. a clean install of a VM                         (vm)
          			   2. a suspended clone of (1)                        (vm')
          			   3. a metadata import of the metadata export of (2) (vm'')
          			*)
       debug test "Starting VM";
       Client.VM.start !rpc session_id vm false false;
       (* Check that all VBDs are plugged in correctly *)
       List.iter
         (fun vbd ->
            let currently_attached = Client.VBD.get_currently_attached !rpc session_id vbd in
            if not currently_attached then failwith "after VM.start not currently_attached";
         ) (Client.VM.get_VBDs !rpc session_id vm);
       delay ();
       debug test "Rebooting VM";
       Client.VM.clean_reboot !rpc session_id vm;
       delay ();
       debug test "Shutting down VM";
       Client.VM.clean_shutdown !rpc session_id vm;
       debug test "Starting VM again";
       Client.VM.start !rpc session_id vm false false;
       verify_network_connectivity session_id test vm;
       delay ();
       debug test "Setting shadow-multiplier live to 10.";
       Client.VM.set_shadow_multiplier_live !rpc session_id vm 10.;
       delay ();
       debug test "Suspending VM";
       Client.VM.suspend !rpc session_id vm;
       debug test "Cloning suspended VM";
       let vm' = Client.VM.clone !rpc session_id vm "clone-suspended-test" in
       debug test "Snapshoting the VM twice";
       ignore(Client.VM.snapshot !rpc session_id vm' "snap1");
       ignore(Client.VM.snapshot !rpc session_id vm' "snap2");

       debug test "Comparing original, clone VIF configuration";
       compare_vifs session_id test vm vm';
       debug test "Comparing original, clone VM configuration";
       compare_vms session_id test vm vm';

       debug test "Importing metadata export of cloned suspended VM";
       Unixext.unlink_safe export_filename;
       vm_export ~metadata_only:true test session_id vm' export_filename;
       let vms = vm_import ~metadata_only:true test session_id export_filename in
       let vm'' = List.find (fun vm -> Client.VM.get_name_label !rpc session_id vm = "clone-suspended-test") vms in
       debug test "Comparing clone, import VIF configuration";
       compare_vifs session_id test vm' vm'';
       debug test "Comparing clone, import VBD configuration";
       compare_vbds session_id test vm' vm'';
       debug test "Comparing clone, import VM configuration";
       compare_vms session_id test vm' vm'';
       debug test "Comparing clone, import snapshot configuration";
       compare_snapshots session_id test vm' vm'';
       debug test "Comparing original, import VIF configuration";
       compare_vifs session_id test vm vm'';
       debug test "Comparing original, import VM configuration";
       compare_vms session_id test vm vm'';

       debug test "Resuming original VM";
       Client.VM.resume !rpc session_id vm false false;
       verify_network_connectivity session_id test vm;
       let host = Client.VM.get_resident_on !rpc session_id vm in
       debug test "Performing localhost migrate of original VM";
       Client.VM.pool_migrate !rpc session_id vm host [];
       verify_network_connectivity session_id test vm;
       debug test "Shutting down original VM";
       Client.VM.clean_shutdown !rpc session_id vm;
       debug test "Resuming imported VM";
       Client.VM.resume !rpc session_id vm'' false false;
       verify_network_connectivity session_id test vm'';
       debug test "Shutting down imported VMs";
       List.iter (fun vm -> if Client.VM.get_power_state !rpc session_id vm <> `Halted then Client.VM.hard_shutdown !rpc session_id vm) vms;

       (* Keep the imported VM and chuck away the clone *)
       (* NB cannot do this earlier because the suspend VDI would be destroyed
          			   and prevent the other VM being resumed *)
       Client.VM.hard_shutdown !rpc session_id vm';
       vm_uninstall test session_id vm';

       debug test "Uninstalling imported VMs";
       List.iter (vm_uninstall test session_id) vms;
       success test;
    ) (fun () ->
        if enabled_csvm then begin
          debug test (Printf.sprintf "Disabling VM.clone for suspended VMs pool-wide");
          Client.Pool.remove_from_other_config !rpc session_id pool "allow_clone_suspended_vm"
        end)

(* Make a VDI, find a host to put it on, create a VBD to dom0 on that host,
 * Attach, Unattach, destroy VBD, destroy VDI *)

let vdi_test session_id =
  let test = make_test "VDI.create/copy/destroy test" 0 in
  start test;

  let all_srs = all_srs_with_vdi_create session_id in
  debug test (Printf.sprintf "All SRs = [ %s ]" (String.concat ", " (List.map (fun x -> Client.SR.get_uuid !rpc session_id x) all_srs)));
  let sr = List.hd all_srs in
  let t = Unix.gettimeofday () in
  let newvdi = Client.VDI.create !rpc session_id "testvdi"
      "description" sr 4194304L `user false false [] [] [] [] in
  let createtime = Unix.gettimeofday () -. t in
  debug test (Printf.sprintf "Time to create: %f%!" createtime);
  let pbd = List.hd (Client.SR.get_PBDs !rpc session_id sr) in
  let host = Client.PBD.get_host !rpc session_id pbd in
  let dom0 = dom0_of_host session_id host in
  let device = List.hd (Client.VM.get_allowed_VBD_devices !rpc session_id dom0) in
  debug test (Printf.sprintf "Creating a VBD connecting the VDI to localhost%!");
  let vbd = Client.VBD.create ~rpc:!rpc ~session_id ~vM:dom0 ~vDI:newvdi ~userdevice:device ~bootable:false
      ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
  let t = Unix.gettimeofday () in
  debug test (Printf.sprintf "Attempting to copy the VDI%!");
  let newvdi2 = Client.VDI.copy !rpc session_id newvdi sr Ref.null Ref.null in
  let copytime = Unix.gettimeofday () -. t in
  debug test (Printf.sprintf "Time to copy: %f%!" copytime);
  Client.VBD.destroy !rpc session_id vbd;
  debug test (Printf.sprintf "Destroying original VDI%!");
  Client.VDI.destroy !rpc session_id newvdi;
  debug test (Printf.sprintf "Destroying copied VDI%!");
  Client.VDI.destroy !rpc session_id newvdi2;
  success test

(* Test a couple of async calls - VDIs are good for this, again! *)
let async_test session_id =
  let test = make_test "Async.VDI.copy" 0 in
  start test;
  let all_srs = all_srs_with_vdi_create session_id in
  let sr = List.hd all_srs in
  let newvdi = Client.VDI.create !rpc session_id "testvdi"
      "description" sr 4194304L `user false false [] [] [] [] in
  let pbd = List.hd (Client.SR.get_PBDs !rpc session_id sr) in
  let host = Client.PBD.get_host !rpc session_id pbd in
  let dom0 = dom0_of_host session_id host in
  let device = List.hd (Client.VM.get_allowed_VBD_devices !rpc session_id dom0) in
  let vbd = Client.VBD.create ~rpc:!rpc ~session_id ~vM:dom0 ~vDI:newvdi ~userdevice:device ~bootable:false
      ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
  let vdis = Client.VDI.get_all !rpc session_id in
  let task = Client.Async.VDI.copy !rpc session_id newvdi sr Ref.null Ref.null in
  wait_for_task_complete session_id task;
  debug test (Printf.sprintf "Task completed!%!");
  let status = Client.Task.get_status !rpc session_id task in
  debug test (Printf.sprintf "Status: %s  result: %s%!"
                (match status with
                 | `pending -> "pending"
                 | `success -> "success"
                 | `failure -> "failure"
                 | `cancelling -> "cancelling"
                 | `cancelled -> "cancelled")
                (Client.Task.get_result !rpc session_id task));
  if status=`failure then
    begin
      failed test (Printf.sprintf "Failure of VDI copy! error_info: %s%!" (String.concat "," (Client.Task.get_error_info !rpc session_id task)));
      failwith "Async VDI copy failed"
    end;
  let newvdis = Client.VDI.get_all !rpc session_id in
  let newvdis = List.filter (fun vdi -> try Client.VDI.get_SR !rpc session_id vdi = sr with _ -> false) newvdis in
  let newvdis2 = List.filter (fun vdi -> not (List.mem vdi vdis)) newvdis in
  match newvdis2 with
  | [newvdi2] ->
    debug test (Printf.sprintf "New vdi: %s%!" (Ref.string_of newvdi2));
    Client.VBD.destroy !rpc session_id vbd;
    Client.VDI.destroy !rpc session_id newvdi;
    Client.VDI.destroy !rpc session_id newvdi2;
    success test
  | _ -> failwith "Expecting 1 new disk!"

let make_vif ~session_id ~vM ~network ~device =
  Client.VIF.create ~rpc:!rpc ~session_id ~vM ~network ~mTU:0L ~mAC:"" ~device ~other_config:["promiscuous", "on"; "mtu", "1400"] ~qos_algorithm_type:"" ~qos_algorithm_params:[]

let with_vm s f =
  try
    let (_: API.ref_VM) = find_template s vm_template in
    let test = make_test "Setting up test VM" 0 in
    start test;
    let vm = install_vm test s in
    f s vm;
    vm_uninstall test s vm;
    success test
  with Unable_to_find_suitable_vm_template ->
    (* SKIP *)
    ()

let vm_powercycle_test s vm =
  let test = make_test "VM powercycle test" 1 in
  start test;
  (* Try to add some VIFs *)
  let (guest_installer_network: API.ref_network) = find_guest_installer_network s in
  debug test (Printf.sprintf "Adding VIF to guest installer network (%s)" (Client.Network.get_uuid !rpc s guest_installer_network));
  let (_: API.ref_VIF) = make_vif ~session_id:s ~vM:vm ~network:guest_installer_network ~device:"0" ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[] in
  begin match Client.PIF.get_all !rpc s with
    | pif :: _ ->
      let net = Client.PIF.get_network !rpc s pif in
      debug test (Printf.sprintf "Adding VIF to physical network (%s)" (Client.Network.get_uuid !rpc s net));
      let (_: API.ref_VIF) = make_vif ~session_id:s ~vM:vm ~network:net ~device:"1" ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[] in
      ()
    | _ -> ()
  end;
  powercycle_test s vm;
  success test


let _ =
  let all_tests = [
    "storage";
    "http";
    "event";
    "vdi";
    "async";
    "import";
    "powercycle";
    "lifecycle";
    "vhd";
    "copy";
    "cbt";
    "import_raw_vdi";
    "pbd-bvt";
    "reconfigure-ip-cluster";
  ] in
  let default_tests = List.filter (fun x -> not(List.mem x [ "lifecycle"; "vhd" ])) all_tests in

  let tests_to_run = ref default_tests in (* default is everything *)
  Arg.parse [
    "-xe-path", Arg.String (fun x -> Quicktest_common.xe_path := x), "Path to xe command line executable";
    "-iso-sr-path", Arg.String (fun x -> Quicktest_storage.iso_path := x), "Path to ISO SR";
    "-single", Arg.String (fun x -> tests_to_run := [ x ]), Printf.sprintf "Only run one test (possibilities are %s)" (String.concat ", " all_tests) ;
    "-all", Arg.Unit (fun () -> tests_to_run := all_tests), Printf.sprintf "Run all tests (%s)" (String.concat ", " all_tests);
    "-default-sr", Arg.Unit (fun () -> Quicktest_storage.use_default_sr := true), "Only run SR tests on the pool's default SR";
    "-nocolour", Arg.Clear Quicktest_common.use_colour, "Don't use colour in the output" ]
    (fun x -> match !host, !username, !password with
       | "", _, _ -> host := x; rpc := rpc_remote; using_unix_domain_socket := false;
       | _, "", _ -> username := x
       | _, _, "" -> password := x
       | _, _, _ -> Printf.fprintf stderr "Skipping unrecognised argument: %s" x)
    "Perform some quick functional tests. The default is to test localhost over a Unix socket. For remote server supply <hostname> <username> and <password> arguments.";
  if !host = "" then host := "localhost";
  if !username = "" then username := "root";

  let maybe_run_test name f =
    assert (List.mem name all_tests);
    if List.mem name !tests_to_run then f () in

  Stunnel.set_good_ciphersuites "!EXPORT:RSA+AES128-SHA256";
  let s = init_session !username !password in
  let all_srs = all_srs_with_vdi_create s in
  let sr = List.hd all_srs in
  finally
    (fun () ->
       (try
          maybe_run_test "pbd-bvt" (fun () -> Quicktest_bvt.start s !rpc);
          maybe_run_test "cbt" (fun () -> Quicktest_cbt.test s);
          maybe_run_test "reconfigure-ip-cluster" (fun () -> Quicktest_cluster.test s);
          maybe_run_test "storage" (fun () -> Quicktest_storage.go s);
          if not !using_unix_domain_socket then maybe_run_test "http" Quicktest_http.run_from_within_quicktest;
          maybe_run_test "event" event_next_unblocking_test;
          maybe_run_test "event" (fun () -> event_next_test s);
          maybe_run_test "event" (fun () -> event_from_test s);
          maybe_run_test "event" (fun () -> event_from_parallel_test s);
          (*				maybe_run_test "event" (fun () -> object_level_event_test s);*)
          maybe_run_test "event" (fun () -> event_message_test s);
          maybe_run_test "event" (fun () -> event_inject_test s);
          maybe_run_test "event" (fun () -> event_from_number_test s);
          maybe_run_test "vdi" (fun () -> vdi_test s);
          maybe_run_test "async" (fun () -> async_test s);
          maybe_run_test "import" (fun () -> import_export_test s);
          maybe_run_test "vhd" (fun () -> with_vm s test_vhd_locking_hook);
          maybe_run_test "powercycle" (fun () -> with_vm s vm_powercycle_test);
          maybe_run_test "lifecycle" (fun () -> with_vm s Quicktest_lifecycle.test);
          maybe_run_test "copy" (fun () -> Quicktest_vdi_copy.start s sr);
          maybe_run_test "import_raw_vdi" (fun () -> Quicktest_import_raw_vdi.start s);
        with
        | Api_errors.Server_error (a,b) ->
          output_string stderr (Printf.sprintf "%s: %s" a (String.concat "," b));
        | e ->
          output_string stderr (Printexc.to_string e))
    ) (fun () -> summarise ())
