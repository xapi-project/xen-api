open Test_common

module D = Debug.Make (struct let name = "test_xapi_xenops" end)

open D
module Date = Clock.Date

(** Helper to create a Xenops VM state for testing *)
let make_xenops_state ~power_state ?(last_start_time = 0.0) () =
  let open Xenops_interface.Vm in
  {
    power_state
  ; domids= [0]
  ; consoles= []
  ; memory_target= 0L
  ; memory_actual= 0L
  ; memory_limit= 0L
  ; vcpu_target= 1
  ; shadow_multiplier_target= 1.0
  ; rtc_timeoffset= ""
  ; uncooperative_balloon_driver= false
  ; guest_agent= []
  ; xsdata_state= []
  ; pv_drivers_detected= false
  ; last_start_time
  ; hvm= false
  ; nomigrate= false
  ; nested_virt= false
  ; domain_type= Domain_PV
  ; featureset= ""
  }

(** Helper to set up VM for testing: sets pending guidances, resident host, and power state *)
let setup_vm_for_test ~__context ~vm ~guidances ~resident_on ~power_state =
  Db.VM.set_pending_guidances ~__context ~self:vm ~value:guidances ;
  Db.VM.set_resident_on ~__context ~self:vm ~value:resident_on ;
  Db.VM.set_power_state ~__context ~self:vm ~value:power_state

(** Helper to check pending guidances after an operation *)
let check_pending_guidances ~__context ~vm ~expect_restart_vm
    ~expect_restart_device_model ~test_description =
  let remaining = Db.VM.get_pending_guidances ~__context ~self:vm in
  Alcotest.(check bool)
    (Printf.sprintf "restart_vm guidance %s - %s"
       (if expect_restart_vm then "present" else "cleared")
       test_description
    )
    expect_restart_vm
    (List.mem `restart_vm remaining) ;
  Alcotest.(check bool)
    (Printf.sprintf "restart_device_model guidance %s - %s"
       (if expect_restart_device_model then "present" else "cleared")
       test_description
    )
    expect_restart_device_model
    (List.mem `restart_device_model remaining)

(** Helper to simulate a VM state update via update_vm_internal *)
let simulate_vm_state_update ~__context ~vm ~previous_power_state
    ~new_power_state ~localhost =
  let previous_state = make_xenops_state ~power_state:previous_power_state () in
  let new_state =
    make_xenops_state ~power_state:new_power_state ~last_start_time:100.0 ()
  in
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let metrics = Db.VM.get_metrics ~__context ~self:vm in
  Db.VM_metrics.set_start_time ~__context ~self:metrics
    ~value:(Date.of_unix_time 50.0) ;
  ignore
    (Xapi_xenops.update_vm_internal ~__context ~id:vm_uuid ~self:vm
       ~previous:(Some previous_state) ~info:(Some new_state) ~localhost
    )

(** Helper to set host software version *)
let set_host_software_version ~__context ~host ~platform_version ~xapi_version =
  Db.Host.remove_from_software_version ~__context ~self:host
    ~key:Xapi_globs._platform_version ;
  Db.Host.add_to_software_version ~__context ~self:host
    ~key:Xapi_globs._platform_version ~value:platform_version ;
  Db.Host.remove_from_software_version ~__context ~self:host
    ~key:Xapi_globs._xapi_version ;
  Db.Host.add_to_software_version ~__context ~self:host
    ~key:Xapi_globs._xapi_version ~value:xapi_version

(** Helper to get the pool from the test database *)
let get_pool ~__context =
  match Db.Pool.get_all ~__context with
  | pool :: _ ->
      pool
  | [] ->
      failwith "No pool found in test database"

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
  let host2 =
    make_host ~__context ~name_label:"host2" ~hostname:"localhost2" ()
  in
  let flags =
    [
      (Constants.cpu_info_vendor_key, "AuthenticAMD")
    ; (Constants.cpu_info_features_key, "deadbeef-deadbeef")
    ]
  in
  let add_flags vm =
    Db.VM.set_last_boot_CPU_flags ~__context ~self:vm ~value:flags ;
    Db.VM.add_to_other_config ~__context ~self:vm ~key:"xenops"
      ~value:"simulator"
  in
  List.iter add_flags [vm1; vm2; vm3; vm4; vm5; vm6] ;
  try
    (* Domain zero is running but not in xenopsd *)
    Db.VM.set_is_control_domain ~__context ~self:vm0 ~value:true ;
    Db.VM.set_resident_on ~__context ~self:vm0
      ~value:(Helpers.get_localhost ~__context) ;
    Db.VM.set_power_state ~__context ~self:vm0 ~value:`Running ;
    (* Start all 6 VMs *)
    Xapi_xenops.start ~__context ~self:vm1 false false ;
    Xapi_xenops.start ~__context ~self:vm2 false false ;
    Xapi_xenops.start ~__context ~self:vm3 false false ;
    Xapi_xenops.start ~__context ~self:vm4 false false ;
    Xapi_xenops.start ~__context ~self:vm5 false false ;
    Xapi_xenops.start ~__context ~self:vm6 false false ;
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
      [vm1; vm2; vm3; vm4; vm5; vm6] ;
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
    ignore
      (Client.VM.shutdown "dbg" (Xapi_xenops.id_of_vm ~__context ~self:vm6) None) ;
    Db.VM.set_name_label ~__context ~self:vm6
      ~value:
        "vm6: shutdown in xenopsd while xapi was off (and is_control_domain)" ;
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
      [(vm1, true); (vm2, true); (vm3, false); (vm4, false); (vm6, false)]
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

(** Test that RestartVM guidance is only cleared when VM starts on up-to-date host *)
let test_pending_guidance_vm_start () =
  let __context = make_test_database () in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;

  let localhost = Helpers.get_localhost ~__context in
  let host2 = make_host ~__context ~name_label:"host2" ~hostname:"host2" () in

  (* Set up software versions - localhost is up-to-date, host2 is not *)
  set_host_software_version ~__context ~host:localhost ~platform_version:"1.2.3"
    ~xapi_version:"4.5.6" ;
  set_host_software_version ~__context ~host:host2 ~platform_version:"1.2.2"
    ~xapi_version:"4.5.5" ;

  (* Set localhost as the pool coordinator *)
  let pool = get_pool ~__context in
  Db.Pool.set_master ~__context ~self:pool ~value:localhost ;

  let vm = make_vm ~__context () in

  (* Set up VM guidances - both restart_vm and restart_device_model *)
  let guidances = [`restart_vm; `restart_device_model] in

  (* Test 1: VM starting on up-to-date host - should clear restart_vm *)
  setup_vm_for_test ~__context ~vm ~guidances ~resident_on:localhost
    ~power_state:`Halted ;
  simulate_vm_state_update ~__context ~vm
    ~previous_power_state:Xenops_interface.Halted
    ~new_power_state:Xenops_interface.Running ~localhost ;
  check_pending_guidances ~__context ~vm ~expect_restart_vm:false
    ~expect_restart_device_model:false
    ~test_description:"VM started on up-to-date host" ;

  (* Test 2: VM starting on old host - should NOT clear restart_vm *)
  setup_vm_for_test ~__context ~vm ~guidances ~resident_on:host2
    ~power_state:`Halted ;
  simulate_vm_state_update ~__context ~vm
    ~previous_power_state:Xenops_interface.Halted
    ~new_power_state:Xenops_interface.Running ~localhost:host2 ;
  check_pending_guidances ~__context ~vm ~expect_restart_vm:true
    ~expect_restart_device_model:false
    ~test_description:"VM started on old host"

(** Test that NO guidance is cleared when suspended VM resumes *)
let test_pending_guidance_vm_resume () =
  let __context = make_test_database () in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;

  let localhost = Helpers.get_localhost ~__context in
  let host2 = make_host ~__context ~name_label:"host2" ~hostname:"host2" () in

  (* Set up software versions - localhost is up-to-date, host2 is not *)
  set_host_software_version ~__context ~host:localhost ~platform_version:"1.2.3"
    ~xapi_version:"4.5.6" ;
  set_host_software_version ~__context ~host:host2 ~platform_version:"1.2.2"
    ~xapi_version:"4.5.5" ;

  (* Set localhost as the pool coordinator *)
  let pool = get_pool ~__context in
  Db.Pool.set_master ~__context ~self:pool ~value:localhost ;

  (* Test 1: Suspended VM resumed on up-to-date host - should NOT clear any guidance *)
  let vm = make_vm ~__context () in
  let guidances = [`restart_vm; `restart_device_model] in
  setup_vm_for_test ~__context ~vm ~guidances ~resident_on:localhost
    ~power_state:`Suspended ;
  simulate_vm_state_update ~__context ~vm
    ~previous_power_state:Xenops_interface.Suspended
    ~new_power_state:Xenops_interface.Running ~localhost ;
  check_pending_guidances ~__context ~vm ~expect_restart_vm:true
    ~expect_restart_device_model:true
    ~test_description:"suspended VM resumed on up-to-date host" ;

  (* Test 2: Suspended VM resumed on old host - should NOT clear any guidance *)
  setup_vm_for_test ~__context ~vm ~guidances ~resident_on:host2
    ~power_state:`Suspended ;
  simulate_vm_state_update ~__context ~vm
    ~previous_power_state:Xenops_interface.Suspended
    ~new_power_state:Xenops_interface.Running ~localhost:host2 ;
  check_pending_guidances ~__context ~vm ~expect_restart_vm:true
    ~expect_restart_device_model:true
    ~test_description:"suspended VM resumed on old host"

(** Test that RestartVM guidance is always cleared when VM is halted *)
let test_pending_guidance_vm_halt () =
  let __context = make_test_database () in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;

  let localhost = Helpers.get_localhost ~__context in
  let host2 = make_host ~__context ~name_label:"host2" ~hostname:"host2" () in

  (* Set up software versions - localhost is up-to-date, host2 is not *)
  set_host_software_version ~__context ~host:localhost ~platform_version:"1.2.3"
    ~xapi_version:"4.5.6" ;
  set_host_software_version ~__context ~host:host2 ~platform_version:"1.2.2"
    ~xapi_version:"4.5.5" ;

  (* Set localhost as the pool coordinator *)
  let pool = get_pool ~__context in
  Db.Pool.set_master ~__context ~self:pool ~value:localhost ;

  let vm = make_vm ~__context () in
  let guidances = [`restart_vm; `restart_device_model] in

  (* Test 1: VM halted on up-to-date host - should clear both guidances *)
  setup_vm_for_test ~__context ~vm ~guidances ~resident_on:localhost
    ~power_state:`Running ;
  Xapi_vm_lifecycle.force_state_reset_keep_current_operations ~__context
    ~self:vm ~value:`Halted ;
  check_pending_guidances ~__context ~vm ~expect_restart_vm:false
    ~expect_restart_device_model:false
    ~test_description:"VM halted on up-to-date host" ;

  (* Test 2: VM halted on old host - should ALSO clear both guidances
     because VM.start_on will enforce host version check on next start *)
  setup_vm_for_test ~__context ~vm ~guidances ~resident_on:host2
    ~power_state:`Running ;
  Xapi_vm_lifecycle.force_state_reset_keep_current_operations ~__context
    ~self:vm ~value:`Halted ;
  check_pending_guidances ~__context ~vm ~expect_restart_vm:false
    ~expect_restart_device_model:false ~test_description:"VM halted on old host"

let test =
  [
    ("test_xapi_restart", `Quick, test_xapi_restart)
  ; ("test_pending_guidance_vm_start", `Quick, test_pending_guidance_vm_start)
  ; ("test_pending_guidance_vm_resume", `Quick, test_pending_guidance_vm_resume)
  ; ("test_pending_guidance_vm_halt", `Quick, test_pending_guidance_vm_halt)
  ]
