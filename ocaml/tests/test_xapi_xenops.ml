open Test_common

module D = Debug.Make (struct let name = "test_xapi_xenops" end)

open D

module Vm_platform_key_values = struct
  let invalid_values = ["foo"; ""; "banana"; "2"]

  let true_values = ["TRUE"; "tRuE"; "1"; "true"]

  let false_values = ["FALSE"; "fAlSe"; "0"; "false"]

  let valid_platformdata = [[]]

  let key = "test_key"

  let test_truthiness p =
    Vm_platform.is_true ~key ~platformdata:p ~default:false

  let test_validity p = Vm_platform.is_valid ~key ~platformdata:p

  let test_value typ f value expected =
    let title = Printf.sprintf "platformdata %s values: %s" typ value in
    ( title
    , `Quick
    , fun () ->
        Alcotest.(check bool)
          (Printf.sprintf "'%s' must evaluate to %b" value expected)
          expected
          (f [(key, value)])
    )

  let truthy_value_tests =
    let test v = test_value "truthy" test_truthiness v true in
    List.map test true_values

  let falsy_value_tests =
    let test v = test_value "falsy" test_truthiness v false in
    List.map test false_values

  let invalid_value_tests =
    let test v = test_value "invalid" test_validity v false in
    List.map test invalid_values

  let empty_platformdata_tests =
    [
      ( "Validity with empty platformdata"
      , `Quick
      , fun () ->
          Alcotest.(check bool)
            "Any key in an empty platformdata must be valid" (test_validity [])
            true
      )
    ]
end

let vm_platform_values_parser_tests =
  let open Vm_platform_key_values in
  List.concat
    [
      truthy_value_tests
    ; falsy_value_tests
    ; invalid_value_tests
    ; empty_platformdata_tests
    ]

let simulator_setup = ref false

let setup_simulator () =
  if not !simulator_setup then (
    Xapi_globs.xenopsd_queues := ["simulator"] ;
    Xapi_globs.default_xenopsd := "simulator" ;
    Xenops_utils.set_fs_backend
      (Some (module Xenops_utils.MemFS : Xenops_utils.FS)) ;
    Xenops_server.register_objects () ;
    Xenops_server.set_backend
      (Some (module Xenops_server_simulator : Xenops_server_plugin.S)) ;
    Xenops_server.WorkerPool.start 16 ;
    Xcp_client.use_switch := true ;
    Xapi_xenops_queue.queue_override := [("simulator", Xenopsd.rpc_fn)] ;
    simulator_setup := true
  ) else
    ()

let unsetup_simulator () =
  if !simulator_setup then
    Xcp_client.use_switch := false

let test_xapi_restart_inner () =
  let __context = make_test_database () in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;
  setup_simulator () ;
  let open Xenops_interface in
  let open Xapi_xenops_queue in
  let module Client = (val make_client "simulator" : XENOPS) in
  let (_ : 'a list) = Client.VM.list "dbg" () in
  let cancel, th =
    let cancel = ref false in
    let th =
      Thread.create
        (fun () ->
          try Xapi_xenops.events_watch ~__context cancel "simulator" None with
          | Api_errors.Server_error (x, []) when x = Api_errors.task_cancelled
            ->
              ()
          | e ->
              raise e
        )
        ()
    in
    (cancel, th)
  in
  let vm0 = Helpers.get_domain_zero ~__context in
  let vm1 = make_vm ~__context ~name_label:"vm1" () in
  let vm2 = make_vm ~__context ~name_label:"vm2" () in
  let vm3 = make_vm ~__context ~name_label:"vm3" () in
  let vm4 = make_vm ~__context ~name_label:"vm4" () in
  let vm5 = make_vm ~__context ~name_label:"vm5" () in
  let vm6 = make_vm ~__context ~name_label:"vm6" () in
  let vm7 = make_vm ~__context ~name_label:"vm7" () in
  let host2 =
    make_host ~__context ~name_label:"host2" ~hostname:"localhost2" ()
  in
  let flags =
    [
      (Xapi_globs.cpu_info_vendor_key, "AuthenticAMD")
    ; (Xapi_globs.cpu_info_features_key, "deadbeef-deadbeef")
    ]
  in
  let add_flags vm =
    Db.VM.set_last_boot_CPU_flags ~__context ~self:vm ~value:flags ;
    Db.VM.add_to_other_config ~__context ~self:vm ~key:"xenops"
      ~value:"simulator"
  in
  List.iter add_flags [vm1; vm2; vm3; vm4; vm5; vm6; vm7] ;
  try
    (* Domain zero is running but not in xenopsd *)
    Db.VM.set_is_control_domain ~__context ~self:vm0 ~value:true ;
    Db.VM.set_resident_on ~__context ~self:vm0
      ~value:(Helpers.get_localhost ~__context) ;
    Db.VM.set_power_state ~__context ~self:vm0 ~value:`Running ;
    (* Start all 7 VMs *)
    Xapi_xenops.start ~__context ~self:vm1 false false ;
    Xapi_xenops.start ~__context ~self:vm2 false false ;
    Xapi_xenops.start ~__context ~self:vm3 false false ;
    Xapi_xenops.start ~__context ~self:vm4 false false ;
    Xapi_xenops.start ~__context ~self:vm5 false false ;
    Xapi_xenops.start ~__context ~self:vm6 false false ;
    Xapi_xenops.start ~__context ~self:vm7 false false ;
    (* vm6 is a ntnx CVM *)
    Db.VM.set_is_control_domain ~__context ~self:vm6 ~value:true ;
    (* Kill the event thread *)
    cancel := true ;
    Client.UPDATES.inject_barrier "dbg"
      (Xapi_xenops.id_of_vm ~__context ~self:vm1)
      0 ;
    let before = Unix.gettimeofday () in
    Thread.join th ;
    let after = Unix.gettimeofday () in
    debug "Elapsed time for thread death: %f\n%!" (after -. before) ;
    (* Check they're running *)
    let is_resident vm =
      Db.VM.get_resident_on ~__context ~self:vm
      = Helpers.get_localhost ~__context
    in
    let is_running_in_xenopsd vm =
      try
        let _, stat =
          Client.VM.stat "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm)
        in
        stat.Vm.power_state = Running
      with Xenopsd_error (Does_not_exist _) -> false
    in
    let assert_correct_state (vm, running) =
      let name = Db.VM.get_name_label ~__context ~self:vm in
      Alcotest.(check bool)
        (Printf.sprintf "State is correct in xapi (%s)" name)
        running (is_resident vm) ;
      Alcotest.(check bool)
        (Printf.sprintf "State is correct in xenopsd (%s)" name)
        running (is_running_in_xenopsd vm)
    in
    List.iter
      (fun vm -> assert_correct_state (vm, true))
      [vm1; vm2; vm3; vm4; vm5; vm6; vm7] ;
    (* Simulate various out-of-band VM operations by resetting the xapi state to halted, and stop one that was running *)
    Db.VM.set_resident_on ~__context ~self:vm1 ~value:Ref.null ;
    Db.VM.set_name_label ~__context ~self:vm1
      ~value:"vm1: resident-on set to null" ;
    Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm2 ~value:`Halted ;
    Db.VM.set_name_label ~__context ~self:vm2
      ~value:"vm2: force_state_reset to halted" ;
    ignore
      (Client.VM.shutdown "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm3) None) ;
    Db.VM.set_name_label ~__context ~self:vm3
      ~value:"vm3: shutdown in xenopsd while xapi was off" ;
    Db.VM.set_resident_on ~__context ~self:vm4 ~value:host2 ;
    Db.VM.set_name_label ~__context ~self:vm4
      ~value:"vm4: xapi thinks it's running somewhere else" ;
    Db.VM.destroy ~__context ~self:vm5 ;
    Db.VM.set_name_label ~__context ~self:vm6
      ~value:"vm6: is_control_domain=true" ;
    ignore
      (Client.VM.shutdown "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm7) None) ;
    Db.VM.set_name_label ~__context ~self:vm7
      ~value:
        "vm7: shutdown in xenopsd while xapi was off (and is_control_domain)" ;
    (* Now run the on_xapi_restart logic *)
    debug "Resync resident on" ;
    Xapi_xenops.resync_resident_on ~__context ;
    let cancel, th =
      let cancel = ref false in
      let th =
        Thread.create
          (fun () ->
            try Xapi_xenops.events_watch ~__context cancel "simulator" None with
            | Api_errors.Server_error (x, []) when x = Api_errors.task_cancelled
              ->
                ()
            | e ->
                raise e
          )
          ()
      in
      (cancel, th)
    in
    debug "Resync_all_vms" ;
    Xapi_xenops.resync_all_vms ~__context ;
    cancel := true ;
    Client.UPDATES.inject_barrier "dbg"
      (Xapi_xenops.id_of_vm ~__context ~self:vm1)
      0 ;
    let before = Unix.gettimeofday () in
    Thread.join th ;
    let after = Unix.gettimeofday () in
    debug "Elapsed time for thread death: %f\n%!" (after -. before) ;
    (* And check that the right thing has happened *)
    List.iter assert_correct_state
      [
        (vm1, true)
      ; (vm2, true)
      ; (vm3, false)
      ; (vm4, false)
      ; (vm6, true)
      ; (vm7, false)
      ]
  with e ->
    Printf.printf "Caught: %s\n" (Printexc.to_string e) ;
    Printf.printf "Backtrace: %s\n%!" (Backtrace.to_string_hum (Backtrace.get e)) ;
    raise e

let test_xapi_restart () =
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      match Backtrace.with_backtraces test_xapi_restart_inner with
      | `Ok x ->
          x
      | `Error (e, _b) ->
          raise e
    )
    unsetup_simulator

(* Nested_virt is restricted in the default test database *)

(* List of platform keys and whether they must be restricted when 'Nested_virt' is restricted.
   true -> must be restricted
   false -> must not be restricted
*)
let nested_virt_checks =
  [
    ([], false)
  ; ([("foo", "bar"); ("baz", "moo"); ("nested-virt", "true")], true)
  ; ([("nested-virt", "TRUE")], true)
  ; ([("nested-virt", "false")], false)
  ; ([("nested-virt", "1")], true)
  ; ([("nested-virt", "0")], false)
  ; ([("nested-virt", "true")], true)
  ]

let pp_platform = Fmt.Dump.(list @@ pair string string)

let test_nested_virt_licensing (platform, should_raise) () =
  let __context = make_test_database () in

  let pool = Db.Pool.get_all ~__context |> List.hd in
  let test_checks =
    if should_raise then
      Alcotest.check_raises
        (Format.asprintf "Failed to raise an exception for platform map: '%a'"
           pp_platform platform
        )
        Api_errors.(Server_error (license_restriction, ["Nested_virt"]))
    else
      fun f ->
    try f ()
    with
    | Api_errors.(Server_error (license_restriction, ["Nested_virt"])) as e ->
      Alcotest.fail
        (Format.asprintf "Unexpectedly raised '%a' for platform map: '%a'"
           Fmt.exn e pp_platform platform
        )
  in

  test_checks (fun () ->
      Db.Pool.set_restrictions ~__context ~self:pool
        ~value:[("restrict_nested_virt", "true")] ;
      Vm_platform.check_restricted_flags ~__context platform
  ) ;
  (* If the feature is unrestricted, nothing should raise an exception *)
  Db.Pool.set_restrictions ~__context ~self:pool
    ~value:[("restrict_nested_virt", "false")] ;
  Vm_platform.check_restricted_flags ~__context platform

let nested_virt_licensing_tests =
  let test (p, r) =
    ( Format.asprintf "Nested_virt licensing: %a" pp_platform p
    , `Quick
    , test_nested_virt_licensing (p, r)
    )
  in
  List.map test nested_virt_checks

let test =
  List.concat
    [
      nested_virt_licensing_tests
    ; vm_platform_values_parser_tests
    ; [("test_xapi_restart", `Quick, test_xapi_restart)]
    ]
