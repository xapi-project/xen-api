open OUnit
open Test_common
open Test_vgpu_common

module D=Debug.Make(struct let name="test_xapi_xenops" end)
open D

let test_enabled_in_xenguest () =
  let should_raise = ["foo";"";"banana";"2"] in
  let should_be_true = ["TRUE";"tRuE";"1";"true"] in
  let should_be_false = ["FALSE";"fAlSe";"0";"false"] in

  let err k =
    failwith (Printf.sprintf "Failed to parse '%s' correctly" k)
  in

  let k = "test_key" in
  let p v = [k,v] in
  let val_fn p = Vm_platform.is_true ~key:k ~platformdata:p ~default:false in
  let valid_fn p = Vm_platform.is_valid ~key:k ~platformdata:p in

  (* Empty list should be valid *)
  if not (valid_fn []) then err "[]";

  List.iter (fun x -> if valid_fn (p x) then err x) should_raise;

  List.iter (fun x ->
      let e = val_fn (p x) in
      if not e then err x) should_be_true;

  List.iter (fun x ->
      let e = val_fn (p x) in
      if e then err x) should_be_false

let simulator_setup = ref false

let setup_simulator () =
  if not !simulator_setup then begin
    Xapi_globs.xenopsd_queues := ["simulator"];
    Xapi_globs.default_xenopsd := "simulator";
    Printf.printf "XXX Starting simulator!!!\n%!";
    Xenops_utils.set_fs_backend (Some (module Xenops_utils.MemFS: Xenops_utils.FS));
    Xenops_server.register_objects ();
    Xenops_server.set_backend (Some (module Xenops_server_simulator: Xenops_server_plugin.S));
    Xenops_server.WorkerPool.start 16;
    Xcp_client.use_switch := true;
    Xapi_xenops_queue.queue_override := ["simulator",Xenopsd.rpc_fn];
    simulator_setup := true;
  end else ()

let unsetup_simulator () =
  if !simulator_setup then begin
    Xcp_client.use_switch := false;
  end

let test_xapi_restart () =
  let __context = make_test_database () in
  setup_simulator ();
  Debug.log_to_stdout ();
  Helpers.test_mode := true;
  let open Xenops_interface in
  let open Xapi_xenops_queue in
  let module Client = (val make_client "simulator" : XENOPS) in
  let _ = Client.VM.list "dbg" in
  let (cancel,th) =
    let cancel = ref false in
    let th = Thread.create (
        fun () ->
          try
            Xapi_xenops.events_watch ~__context cancel "simulator" None
          with
          | Api_errors.Server_error (x,[]) when x = Api_errors.task_cancelled -> ()
          | e -> raise e) () in
    (cancel,th) in
  let vm1 = make_vm ~__context ~name_label:"vm1" () in
  let vm2 = make_vm ~__context ~name_label:"vm2" () in
  let vm3 = make_vm ~__context ~name_label:"vm3" () in
  let vm4 = make_vm ~__context ~name_label:"vm4" () in
  let vm5 = make_vm ~__context ~name_label:"vm5" () in
  let host2 = make_host ~__context ~name_label:"host2" ~hostname:"localhost2" () in
  let flags = [
    Xapi_globs.cpu_info_vendor_key, "AuthenticAMD";
    Xapi_globs.cpu_info_features_key, "deadbeef-deadbeef";
  ] in
  let add_flags vm =
    Db.VM.set_last_boot_CPU_flags ~__context ~self:vm ~value:flags;
    Db.VM.add_to_other_config ~__context ~self:vm ~key:"xenops" ~value:"simulator"
  in
  List.iter add_flags [vm1; vm2; vm3; vm4; vm5];

  try
(* Start all 4 VMs *)
    Xapi_xenops.start ~__context ~self:vm1 false false;
    Xapi_xenops.start ~__context ~self:vm2 false false;
    Xapi_xenops.start ~__context ~self:vm3 false false;
    Xapi_xenops.start ~__context ~self:vm4 false false;
    Xapi_xenops.start ~__context ~self:vm5 false false;

(* Kill the event thread *)
    cancel := true;
    Client.UPDATES.inject_barrier "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm1) 0;
    let before = Unix.gettimeofday () in
    Thread.join th;
    let after = Unix.gettimeofday () in
    debug "Elapsed time for thread death: %f\n%!" (after -. before);
(* Check they're running *)
    let is_resident vm =
      Db.VM.get_resident_on ~__context ~self:vm = Helpers.get_localhost ~__context
    in
    assert_bool "Running here" (is_resident vm1);
    assert_bool "Running here" (is_resident vm2);
    assert_bool "Running here" (is_resident vm3);
    assert_bool "Running here" (is_resident vm4);
    assert_bool "Running here" (is_resident vm5);

(* Simulate various out-of-band VM operations by resetting the xapi state to halted, and stop one that was running *)
    Db.VM.set_resident_on ~__context ~self:vm1 ~value:(Ref.null);
    Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm2 ~value:`Halted;
    ignore(Client.VM.shutdown "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm3) None);
    Db.VM.set_resident_on ~__context ~self:vm4 ~value:host2;
    Db.VM.destroy ~__context ~self:vm5;

    (* Now run the on_xapi_restart logic *)
    debug "Resync resident on";
    Xapi_xenops.resync_resident_on ~__context;
    let (cancel,th) =
      let cancel = ref false in
      let th = Thread.create (
          fun () ->
            try
              Xapi_xenops.events_watch ~__context cancel "simulator" None
            with
            | Api_errors.Server_error (x,[]) when x = Api_errors.task_cancelled -> ()
            | e -> raise e) () in
      (cancel,th) in
    debug "Resync_all_vms";
    Xapi_xenops.resync_all_vms ~__context;

    cancel := true;
    Client.UPDATES.inject_barrier "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm1) 0;
    let before = Unix.gettimeofday () in
    Thread.join th;
    let after = Unix.gettimeofday () in
    debug "Elapsed time for thread death: %f\n%!" (after -. before);

    (* And check that the right thing has happened *)
    assert_bool "Running here" (is_resident vm1);
    assert_bool "Running here" (is_resident vm2);
    assert_bool "Not running here" (not (is_resident vm3));
    assert_bool "Not running here" (not (is_resident vm4));

    unsetup_simulator ()
  with e ->
    Printf.printf "Caught: %s\n" (Printexc.to_string e);
    Printf.printf "Backtrace: %s\n%!" (Backtrace.to_string_hum (Backtrace.get e));
      raise e


let test_nested_virt_licensing () =
  let __context = make_test_database () in
  (* Nested_virt is restricted in the default test database *)

  (* List of plaform keys and whether they should be restricted when 'Nested_virt' is restricted.
     true -> definitely should be restricted
     false -> definitely should be unrestricted
  *)

  let nested_virt_checks =
    [[],                        false;
     ["foo","bar";"baz","moo";
      "nested-virt","true"],    true;
     ["nested-virt","TRUE"],    true;
     ["nested-virt","false"],   false;
     ["nested-virt","1"],       true;
     ["nested-virt","0"],       false;
     ["nested-virt","true"],    true;
    ] in

  let string_of_platform p =
    Printf.sprintf "[%s]"
      (String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "'%s','%s'" k v) p))
  in

  let pool = Db.Pool.get_all ~__context |> List.hd in

  let check_one (platform,should_raise) =
    begin
      try
        Db.Pool.set_restrictions ~__context ~self:pool ~value:["restrict_nested_virt","true"];
        Vm_platform.check_restricted_flags ~__context platform;
        if should_raise
        then
          failwith
            (Printf.sprintf "Failed to raise an exception for platform map: '[%s]'"
               (string_of_platform platform));
      with Api_errors.Server_error(e,l) when e=Api_errors.license_restriction ->
        if not should_raise
        then
          failwith
            (Printf.sprintf "Raise an exception unexpectedly for platform map: '[%s]'"
               (string_of_platform platform));
    end;

    (* If the feature is unrestricted, nothing should raise an exception *)
    Db.Pool.set_restrictions ~__context ~self:pool ~value:["restrict_nested_virt","false"];
    Vm_platform.check_restricted_flags ~__context platform
  in

  List.iter check_one nested_virt_checks



let test =
  "test_xapi_xenops" >:::
  [
    "test_nested_virt_licensing" >:: test_nested_virt_licensing;
    "test_enabled_in_xenguest" >:: test_enabled_in_xenguest;
    "test_xapi_restart" >:: test_xapi_restart;
  ]
