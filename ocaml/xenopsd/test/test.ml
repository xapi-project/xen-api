(*
 * Copyright (C) Citrix Systems Inc.
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
(** @group Storage *)

let default_path = "/var/xapi/xenopsd"

open Xenops_interface
open Xenops_utils

module Client = Xenops_interface.XenopsAPI (Idl.Exn.GenClient (struct
  let rpc = Xenopsd.rpc_fn
end))

let usage_and_exit () =
  Printf.fprintf stderr "Usage:\n" ;
  Printf.fprintf stderr "  %s" Sys.argv.(0) ;
  exit 1

let dbg = "test"

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let expect_exception pred f =
  let exn =
    try
      f () ;
      failwith "Unexpected success"
    with Xenopsd_error e -> e
  in
  if pred exn then () else raise (Xenopsd_error exn)

let fail_running f =
  expect_exception
    (function Bad_power_state (Running, Halted) -> true | _ -> false)
    f

let fail_not_built f =
  expect_exception (function Domain_not_built -> true | _ -> false) f

let fail_connected f =
  expect_exception (function Device_is_connected -> true | _ -> false) f

let event_wait p =
  let finished = ref false in
  let event_id = ref None in
  while not !finished do
    let _, deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    event_id := Some next_id ;
    List.iter (fun d -> if p d then finished := true) deltas
  done

let task_ended dbg id =
  match (Client.TASK.stat dbg id).Task.state with
  | Task.Completed _ | Task.Failed _ ->
      true
  | Task.Pending _ ->
      false

let wait_for_task id =
  let finished = function
    | Dynamic.Task id' ->
        id = id' && task_ended dbg id
    | _x ->
        false
  in
  event_wait finished ; id

let verbose_timings = ref false

let wait_for_tasks id =
  let module StringSet = Set.Make (struct
    type t = string

    let compare = compare
  end) in
  let ids =
    ref (List.fold_left (fun set x -> StringSet.add x set) StringSet.empty id)
  in
  let event_id = ref None in
  while not (StringSet.is_empty !ids) do
    let _, deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    if !verbose_timings then (
      Printf.fprintf stderr "next_id = %d; deltas = %d" next_id
        (List.length deltas) ;
      flush stderr
    ) ;
    if List.length deltas = 0 then
      failwith (Printf.sprintf "no deltas, next_id = %d" next_id) ;
    event_id := Some next_id ;
    List.iter
      (function
        | Dynamic.Task id' ->
            if task_ended dbg id' then ids := StringSet.remove id' !ids
        | _ ->
            ()
        )
      deltas
  done

let success_task id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id ;
  match t.Task.state with
  | Task.Completed _ ->
      ()
  | Task.Failed x -> (
    match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty x with
    | Ok x ->
        raise (Xenops_interface.Xenopsd_error x)
    | Error _ ->
        raise
          (Xenops_interface.Xenopsd_error
             (Errors.Internal_error (Jsonrpc.to_string x))
          )
  )
  | Task.Pending _ ->
      failwith "task pending"

let fail_not_built_task id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id ;
  match t.Task.state with
  | Task.Completed _ ->
      failwith "task completed successfully: expected Domain_not_built"
  | Task.Failed x -> (
    match Rpcmarshal.unmarshal Xenops_interface.Errors.error.Rpc.Types.ty x with
    | Ok Errors.Domain_not_built ->
        ()
    | Ok x ->
        raise (Xenops_interface.Xenopsd_error x)
    | Error _ ->
        raise
          (Xenops_interface.Xenopsd_error
             (Errors.Internal_error (Jsonrpc.to_string x))
          )
  )
  | Task.Pending _ ->
      failwith "task pending"

let fail_invalid_vcpus_task id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id ;
  match t.Task.state with
  | Task.Completed _ ->
      failwith "task completed successfully: expected Invalid_vcpus"
  | Task.Failed x -> (
    match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty x with
    | Ok (Invalid_vcpus _) ->
        ()
    | Ok x ->
        raise (Xenops_interface.Xenopsd_error x)
    | Error _ ->
        raise
          (Xenops_interface.Xenopsd_error
             (Errors.Internal_error (Jsonrpc.to_string x))
          )
  )
  | Task.Pending _ ->
      failwith "task pending"

let test_query _ =
  let (_ : Query.t) = Client.query dbg () in
  ()

let missing_vm = "missing"

let vm_test_remove_missing _ =
  let exn =
    try
      Client.VM.remove dbg missing_vm ;
      Some (Failure "VDI.remove succeeded")
    with
    | Xenopsd_error (Does_not_exist (_, _)) ->
        None
    | e ->
        Some e
  in
  Option.iter raise exn

let example_uuid = "c0ffeec0-ffee-c0ff-eec0-ffeec0ffeec0"

let ( ** ) = Int64.mul

let create_vm vmid =
  let open Vm in
  let _ =
    PV
      {
        framebuffer= false
      ; framebuffer_ip= Some "0.0.0.0"
      ; vncterm= true
      ; vncterm_ip= None
      ; pci_passthrough= false
      ; Vm.boot=
          Indirect
            {
              bootloader= "pygrub"
            ; extra_args= "extra"
            ; legacy_args= "legacy"
            ; bootloader_args= "bootloader"
            ; devices= [Local "0"; Local "1"]
            }
      }
  in
  let hvm =
    HVM
      {
        hap= true
      ; shadow_multiplier= 1.
      ; timeoffset= ""
      ; video_mib= 4
      ; video= Cirrus
      ; acpi= true
      ; serial= None
      ; keymap= Some "en-gb"
      ; vnc_ip= Some "hello"
      ; pci_emulations= ["1"]
      ; pci_passthrough= false
      ; boot_order= "boot"
      ; qemu_disk_cmdline= false
      ; qemu_stubdom= false
      ; firmware= Bios
      ; tpm= None
      }
  in
  {
    id= vmid
  ; name= "Example: " ^ vmid
  ; ssidref= 1l
  ; xsdata= [("xs", "data")]
  ; platformdata= [("platform", "data")]
  ; bios_strings= [("bios", "strings")]
  ; ty= hvm
  ; suppress_spurious_page_faults= true
  ; machine_address_size= None
  ; memory_static_max= 128L ** 1024L ** 1024L
  ; memory_dynamic_max= 128L ** 1024L ** 1024L
  ; memory_dynamic_min= 128L ** 1024L ** 1024L
  ; vcpu_max= 2
  ; vcpus= 2
  ; scheduler_params= {priority= None; affinity= []}
  ; on_crash= [Vm.Shutdown]
  ; on_shutdown= [Vm.Shutdown]
  ; on_reboot= [Vm.Start]
  ; on_softreboot= [Vm.Softreboot]
  ; pci_msitranslate= true
  ; pci_power_mgmt= false
  ; has_vendor_device= false
  ; generation_id= None
  }

let sl x =
  Printf.sprintf "[ %s ]"
    (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) x))

let rpc_of_action = Rpcmarshal.marshal Vm.action.Rpc.Types.ty

let rpc_of_video_card = Rpcmarshal.marshal Vm.video_card.Rpc.Types.ty

type disk_list = Xenops_types.TopLevel.disk list [@@deriving rpcty]

let rpc_of_disk = Rpcmarshal.marshal Xenops_types.TopLevel.disk.Rpc.Types.ty

let rpc_of_disk_list = Rpcmarshal.marshal disk_list.Rpc.Types.ty

let rpc_of_network_t = Rpcmarshal.marshal Network.t.Rpc.Types.ty

let rpc_of_qos_scheduler = Rpcmarshal.marshal Vbd.qos_scheduler.Rpc.Types.ty

let assert_equal (type a) ~msg ?printer (a : a) b =
  match printer with
  | None ->
      Alcotest.(check bool) msg true (a = b)
  | Some printer ->
      let module T = struct
        type t = a

        let pp ppf (v : t) = Fmt.of_to_string printer ppf v

        let equal = ( = )
      end in
      Alcotest.(check (module T)) msg a b

let vm_assert_equal vm vm' =
  let open Vm in
  assert_equal ~msg:"id" ~printer:(fun x -> x) vm.id vm'.id ;
  assert_equal ~msg:"name" ~printer:(fun x -> x) vm.name vm'.name ;
  assert_equal ~msg:"ssidref" ~printer:Int32.to_string vm.ssidref vm'.ssidref ;
  assert_equal ~msg:"xsdata" ~printer:sl vm.xsdata vm'.xsdata ;
  assert_equal ~msg:"platformdata" ~printer:sl vm.platformdata vm'.platformdata ;
  assert_equal ~msg:"bios_strings" ~printer:sl vm.bios_strings vm'.bios_strings ;
  assert_equal ~msg:"suppress_spurious_page_faults" ~printer:string_of_bool
    vm.suppress_spurious_page_faults vm'.suppress_spurious_page_faults ;
  assert_equal ~msg:"machine_address_size"
    ~printer:(function None -> "None" | Some x -> string_of_int x)
    vm.machine_address_size vm'.machine_address_size ;
  assert_equal ~msg:"memory_static_max" ~printer:Int64.to_string
    vm.memory_static_max vm'.memory_static_max ;
  assert_equal ~msg:"memory_dynamic_max" ~printer:Int64.to_string
    vm.memory_dynamic_max vm'.memory_dynamic_max ;
  assert_equal ~msg:"memory_dynamic_min" ~printer:Int64.to_string
    vm.memory_dynamic_min vm'.memory_dynamic_min ;
  assert_equal ~msg:"vcpu_max" ~printer:string_of_int vm.vcpu_max vm'.vcpu_max ;
  assert_equal ~msg:"vcpus" ~printer:string_of_int vm.vcpus vm'.vcpus ;
  assert_equal ~msg:"on_crash"
    ~printer:(fun x ->
      String.concat ", "
        (List.map (fun x -> x |> rpc_of_action |> Jsonrpc.to_string) x)
    )
    vm.on_crash vm'.on_crash ;
  assert_equal ~msg:"on_shutdown"
    ~printer:(fun x ->
      String.concat ", "
        (List.map (fun x -> x |> rpc_of_action |> Jsonrpc.to_string) x)
    )
    vm.on_shutdown vm'.on_shutdown ;
  assert_equal ~msg:"on_reboot"
    ~printer:(fun x ->
      String.concat ", "
        (List.map (fun x -> x |> rpc_of_action |> Jsonrpc.to_string) x)
    )
    vm.on_reboot vm'.on_reboot ;
  assert_equal ~msg:"has_vendor_device" ~printer:string_of_bool
    vm.has_vendor_device vm'.has_vendor_device ;
  let is_hvm vm =
    match vm.ty with HVM _ -> true | PV _ | PVinPVH _ | PVH _ -> false
  in
  assert_equal ~msg:"HVM-ness" ~printer:string_of_bool (is_hvm vm) (is_hvm vm') ;
  match (vm.ty, vm'.ty) with
  | HVM _, (PV _ | PVinPVH _ | PVH _) | (PV _ | PVinPVH _ | PVH _), HVM _ ->
      failwith "HVM-ness"
  | HVM h, HVM h' ->
      assert_equal ~msg:"HAP" ~printer:string_of_bool h.hap h'.hap ;
      assert_equal ~msg:"shadow_multipler" ~printer:string_of_float
        h.shadow_multiplier h'.shadow_multiplier ;
      assert_equal ~msg:"timeoffset"
        ~printer:(fun x -> x)
        h.timeoffset h'.timeoffset ;
      assert_equal ~msg:"video_mib" ~printer:string_of_int h.video_mib
        h'.video_mib ;
      assert_equal ~msg:"video"
        ~printer:(fun x -> x |> rpc_of_video_card |> Jsonrpc.to_string)
        h.video h'.video ;
      assert_equal ~msg:"acpi" ~printer:string_of_bool h.acpi h'.acpi ;
      assert_equal ~msg:"serial"
        ~printer:(Option.value ~default:"None")
        h.serial h'.serial ;
      assert_equal ~msg:"keymap"
        ~printer:(Option.value ~default:"None")
        h.keymap h'.keymap ;
      assert_equal ~msg:"vnc_ip"
        ~printer:(Option.value ~default:"None")
        h.vnc_ip h'.vnc_ip ;
      assert_equal ~msg:"pci_emulations" ~printer:(String.concat ";")
        h.pci_emulations h'.pci_emulations ;
      assert_equal ~msg:"pci_passthrough" ~printer:string_of_bool
        h.pci_passthrough h'.pci_passthrough ;
      assert_equal ~msg:"boot_order"
        ~printer:(fun x -> x)
        h.boot_order h'.boot_order ;
      assert_equal ~msg:"qemu_disk_cmdline" ~printer:string_of_bool
        h.qemu_disk_cmdline h'.qemu_disk_cmdline
  | (PV p | PVinPVH p | PVH p), (PV p' | PVinPVH p' | PVH p') -> (
      assert_equal ~msg:"framebuffer" ~printer:string_of_bool p.framebuffer
        p'.framebuffer ;
      assert_equal ~msg:"vncterm" ~printer:string_of_bool p.vncterm p'.vncterm ;
      assert_equal ~msg:"vncterm_ip"
        ~printer:(Option.value ~default:"None")
        p.vncterm_ip p'.vncterm_ip ;
      match (p.boot, p'.boot) with
      | Direct _, Indirect _ | Indirect _, Direct _ ->
          failwith "pv-boot-ness"
      | Direct x, Direct x' ->
          assert_equal ~msg:"kernel" ~printer:(fun x -> x) x.kernel x'.kernel ;
          assert_equal ~msg:"cmdline" ~printer:(fun x -> x) x.cmdline x'.cmdline ;
          assert_equal ~msg:"ramdisk"
            ~printer:(function None -> "None" | Some x -> x)
            x.ramdisk x'.ramdisk
      | Indirect x, Indirect x' ->
          assert_equal ~msg:"bootloader"
            ~printer:(fun x -> x)
            x.bootloader x'.bootloader ;
          assert_equal ~msg:"extra_args"
            ~printer:(fun x -> x)
            x.extra_args x'.extra_args ;
          assert_equal ~msg:"legacy_args"
            ~printer:(fun x -> x)
            x.legacy_args x'.legacy_args ;
          assert_equal ~msg:"bootloader_args"
            ~printer:(fun x -> x)
            x.bootloader_args x'.bootloader_args ;
          assert_equal ~msg:"devices"
            ~printer:(fun x -> x |> rpc_of_disk_list |> Jsonrpc.to_string)
            x.devices x'.devices
    )

let with_vm id f =
  let vm = create_vm id in
  let (id : Vm.id) = Client.VM.add dbg vm in
  finally
    (fun () -> f id)
    (fun () ->
      let _, state = Client.VM.stat dbg id in
      ( match state.Vm.power_state with
      | Running | Paused -> (
          Printf.fprintf stderr "VM is running or paused; shutting down" ;
          try Client.VM.shutdown dbg id None |> wait_for_task |> success_task
          with e ->
            Printf.fprintf stderr "Caught failure during with_vm cleanup: %s"
              (Printexc.to_string e) ;
            raise e
        )
      | _ ->
          ()
      ) ;
      try Client.VM.remove dbg id
      with e ->
        Printf.fprintf stderr "Caught failure during with_vm cleanup: %s"
          (Printexc.to_string e) ;
        raise e
    )

let vm_test_add_remove _ = with_vm example_uuid (fun _ -> ())

let vm_test_create_destroy _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.destroy dbg id |> wait_for_task |> success_task
  )

let vm_test_pause_unpause _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.pause dbg id |> wait_for_task |> fail_not_built_task ;
      Client.VM.destroy dbg id |> wait_for_task |> success_task
  )

let vm_test_build_pause_unpause _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.build dbg id false |> wait_for_task |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      Client.VM.create_device_model dbg id false
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      Client.VM.pause dbg id |> wait_for_task |> success_task ;
      Client.VM.destroy dbg id |> wait_for_task |> success_task
  )

let vm_test_build_vcpus _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.build dbg id false |> wait_for_task |> success_task ;
      Client.VM.set_vcpus dbg id 1 |> wait_for_task |> fail_not_built_task ;
      Client.VM.create_device_model dbg id false
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      Client.VM.set_vcpus dbg id 1 |> wait_for_task |> success_task ;
      let state = Client.VM.stat dbg id |> snd in
      if state.Vm.vcpu_target <> 1 then
        failwith (Printf.sprintf "vcpu_target %d <> 1" state.Vm.vcpu_target) ;
      Client.VM.set_vcpus dbg id 2 |> wait_for_task |> success_task ;
      let state = Client.VM.stat dbg id |> snd in
      if state.Vm.vcpu_target <> 2 then
        failwith (Printf.sprintf "vcpu_target %d <> 2" state.Vm.vcpu_target) ;
      Client.VM.set_vcpus dbg id 4 |> wait_for_task |> fail_invalid_vcpus_task ;
      Client.VM.set_vcpus dbg id 0 |> wait_for_task |> fail_invalid_vcpus_task ;
      Client.VM.destroy dbg id |> wait_for_task |> success_task
  )

let vm_test_add_list_remove _ =
  with_vm example_uuid (fun id ->
      let vm = create_vm example_uuid in
      let (vms : (Vm.t * Vm.state) list) = Client.VM.list dbg () in
      let vm' = List.find (fun x -> x.Vm.id = id) (List.map fst vms) in
      vm_assert_equal vm vm'
  )

let vm_remove_running _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.build dbg id false |> wait_for_task |> success_task ;
      Client.VM.create_device_model dbg id false
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      fail_running (fun () -> Client.VM.remove dbg id) ;
      Client.VM.destroy dbg id |> wait_for_task |> success_task
  )

let vm_test_start_shutdown _ =
  with_vm example_uuid (fun id ->
      Client.VM.start dbg id false |> wait_for_task |> success_task ;
      fail_running (fun () -> Client.VM.remove dbg id) ;
      Client.VM.shutdown dbg id None |> wait_for_task |> success_task
  )

let vm_test_parallel_start_shutdown _ =
  let rec ints start finish =
    if start > finish then [] else start :: ints (start + 1) finish
  in
  let ints = ints 0 1000 |> List.map string_of_int in
  let t = Unix.gettimeofday () in
  let ids =
    List.map
      (fun x ->
        let vm = create_vm x in
        Client.VM.add dbg vm
      )
      ints
  in
  if !verbose_timings then (
    Printf.fprintf stderr "VM.adds took %.1f\n" (Unix.gettimeofday () -. t) ;
    flush stderr
  ) ;
  let t = Unix.gettimeofday () in
  let tasks =
    List.map
      (fun id ->
        let id = Client.VM.start dbg id false in
        (* Printf.fprintf stderr "%s\n" id; flush stderr; *) id
      )
      ids
  in
  wait_for_tasks tasks ;
  if !verbose_timings then (
    Printf.fprintf stderr "Cleaning up tasks\n" ;
    flush stderr
  ) ;
  List.iter success_task tasks ;
  if !verbose_timings then (
    Printf.fprintf stderr "VM.starts took %.1f\n" (Unix.gettimeofday () -. t) ;
    flush stderr
  ) ;
  let t = Unix.gettimeofday () in
  let tasks = List.map (fun id -> Client.VM.shutdown dbg id None) ids in
  wait_for_tasks tasks ;
  if !verbose_timings then (
    Printf.fprintf stderr "Cleaning up tasks\n" ;
    flush stderr
  ) ;
  List.iter success_task tasks ;
  if !verbose_timings then (
    Printf.fprintf stderr "VM.shutdown took %.1f\n" (Unix.gettimeofday () -. t) ;
    flush stderr
  ) ;
  let t = Unix.gettimeofday () in
  List.iter (fun id -> Client.VM.remove dbg id) ids ;
  if !verbose_timings then (
    Printf.fprintf stderr "VM.remove took %.1f\n" (Unix.gettimeofday () -. t) ;
    flush stderr
  ) ;
  ()

let vm_test_consoles _ = ()

let vm_test_reboot _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.build dbg id false |> wait_for_task |> success_task ;
      Client.VM.create_device_model dbg id false
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      let state : Vm.state = Client.VM.stat dbg id |> snd in
      Client.DEBUG.trigger dbg "reboot" [id] ;
      (* ... need to wait for the domain id to change *)
      event_wait (function
        | Dynamic.Vm id' -> (
            id = id'
            &&
            match try Some (Client.VM.stat dbg id) with _ -> None with
            | Some (_, vm_state) ->
                vm_state.Vm.domids <> state.Vm.domids
            | _ ->
                false
          )
        | _ ->
            false
        ) ;
      Client.VM.shutdown dbg id None |> wait_for_task |> success_task
  )

let vm_test_halt _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.build dbg id false |> wait_for_task |> success_task ;
      Client.VM.create_device_model dbg id false
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      Client.DEBUG.trigger dbg "halt" [id] ;
      (* ... need to wait for the domain ids to disappear *)
      event_wait (function
        | Dynamic.Vm id' -> (
            id = id'
            &&
            match try Some (Client.VM.stat dbg id) with _ -> None with
            | Some (_, vm_state) ->
                vm_state.Vm.domids = []
            | _ ->
                false
          )
        | _ ->
            false
        )
  )

let vm_test_suspend_resume _ =
  with_vm example_uuid (fun id ->
      Client.VM.create dbg id |> wait_for_task |> success_task ;
      Client.VM.build dbg id false |> wait_for_task |> success_task ;
      Client.VM.create_device_model dbg id false
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      Client.VM.suspend dbg id (Local "/tmp/suspend-image")
      |> wait_for_task
      |> success_task ;
      Client.VM.resume dbg id (Local "/tmp/suspend-image")
      |> wait_for_task
      |> success_task ;
      Client.VM.unpause dbg id |> wait_for_task |> success_task ;
      Client.VM.shutdown dbg id None |> wait_for_task |> success_task ;
      Client.VM.destroy dbg id |> wait_for_task |> success_task
  )

module type DEVICE = sig
  type t

  type state

  val assert_equal : t -> t -> unit

  type position

  val positions : position list

  type id

  val ids : id list

  val create : id -> position -> t

  val add : t -> id

  val remove : id -> unit

  val plug : id -> Task.id

  val unplug : id -> Task.id

  val list : Vm.id -> (t * state) list

  val find : id -> (t * state) list -> t
end

module DeviceTests =
functor
  (D : DEVICE)
  ->
  struct
    open D

    let add_remove _ =
      with_vm example_uuid (fun _id ->
          let dev = create (List.hd ids) (List.hd positions) in
          let (dev_id : id) = add dev in
          remove dev_id
      )

    let with_added_vm id f =
      with_vm id (fun id ->
          Client.VM.create dbg id |> wait_for_task |> success_task ;
          finally
            (fun () -> f id)
            (fun () -> Client.VM.destroy dbg id |> wait_for_task |> success_task)
      )

    let add_plug_unplug_remove _ =
      with_added_vm example_uuid (fun _id ->
          let dev = create (List.hd ids) (List.hd positions) in
          let (dev_id : id) = add dev in
          plug dev_id |> wait_for_task |> success_task ;
          unplug dev_id |> wait_for_task |> success_task ;
          remove dev_id
      )

    let add_plug_unplug_many_remove _ =
      with_added_vm example_uuid (fun _id ->
          let ids =
            List.map
              (fun (id, position) ->
                let dev = create id position in
                let id = add dev in
                plug id |> wait_for_task |> success_task ;
                id
              )
              (List.combine ids positions)
          in
          List.iter
            (fun id ->
              unplug id |> wait_for_task |> success_task ;
              remove id
            )
            ids
      )

    let add_list_remove _ =
      with_vm example_uuid (fun id ->
          let dev = create (List.hd ids) (List.hd positions) in
          let (dev_id : id) = add dev in
          let (devs : (t * state) list) = list id in
          let dev' = find dev_id devs in
          assert_equal dev dev' ; remove dev_id
      )

    let add_vm_remove _ =
      with_vm example_uuid (fun _id ->
          let dev = create (List.hd ids) (List.hd positions) in
          let (_ : id) = add dev in
          ()
      )

    let remove_running _ =
      with_added_vm example_uuid (fun _id ->
          let dev = create (List.hd ids) (List.hd positions) in
          let (dev_id : id) = add dev in
          plug dev_id |> wait_for_task |> success_task ;
          (* no unplug *)
          fail_connected (fun () -> remove dev_id)
      )
  end

let default_of t =
  match Rpcmarshal.unmarshal t.Rpc.Types.ty Rpc.(Dict []) with
  | Ok x ->
      x
  | Error (`Msg m) ->
      failwith (Printf.sprintf "Error creating default_t: %s" m)

let vbd_default_t = default_of Vbd.t

module VbdDeviceTests = DeviceTests (struct
  type t = Vbd.t

  type id = Vbd.id

  type state = Vbd.state

  type position = Device_number.t option

  let positions = [None; None; None]

  let ids = List.map (fun x -> (example_uuid, x)) ["0"; "1"; "2"]

  let create vid position =
    let open Vbd in
    {
      vbd_default_t with
      Vbd.id= vid
    ; position
    ; mode= ReadWrite
    ; backend= Some (Local "/dev/zero")
    ; ty= Disk
    ; unpluggable= true
    ; extra_backend_keys= [("backend", "keys")]
    ; extra_private_keys= [("private", "keys")]
    ; qos= None
    }

  let add = Client.VBD.add dbg

  let remove = Client.VBD.remove dbg

  let plug = Client.VBD.plug dbg

  let unplug id = Client.VBD.unplug dbg id false

  let list = Client.VBD.list dbg

  let find id vbds = List.find (fun (x, _) -> x.Vbd.id = id) vbds |> fst

  let assert_equal vbd vbd' =
    let open Vbd in
    assert_equal ~msg:"id"
      ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b)
      vbd.id vbd'.id ;
    assert_equal ~msg:"mode"
      ~printer:(function ReadWrite -> "RW" | ReadOnly -> "RO")
      vbd.mode vbd'.mode ;
    assert_equal ~msg:"backend"
      ~printer:(fun x ->
        Option.value ~default:"None"
          (Option.map (fun x -> x |> rpc_of_disk |> Jsonrpc.to_string) x)
      )
      vbd.backend vbd'.backend ;
    assert_equal ~msg:"unpluggable" ~printer:string_of_bool vbd.unpluggable
      vbd'.unpluggable ;
    assert_equal ~msg:"extra_backend_keys" ~printer:sl vbd.extra_backend_keys
      vbd'.extra_backend_keys ;
    assert_equal ~msg:"extra_private_keys" ~printer:sl vbd.extra_private_keys
      vbd'.extra_private_keys
end)

module VifDeviceTests = DeviceTests (struct
  type t = Vif.t

  type id = Vif.id

  type state = Vif.state

  type position = int

  let positions = [0; 1; 2]

  let ids = List.map (fun x -> (example_uuid, x)) ["0"; "1"; "2"]

  let default_t = default_of Vif.t

  let create vid position =
    let open Vif in
    {
      default_t with
      id= vid
    ; position
    ; mac= "c0:ff:ee:c0:ff:ee"
    ; carrier= false
    ; mtu= 1450
    ; rate= Some (1L, 2L)
    ; backend= Network.Local "xenbr0"
    ; other_config= [("other", "config")]
    ; locking_mode= Vif.Unlocked
    ; extra_private_keys= [("private", "keys")]
    }

  let add = Client.VIF.add dbg

  let remove = Client.VIF.remove dbg

  let plug = Client.VIF.plug dbg

  let unplug id = Client.VIF.unplug dbg id false

  let list = Client.VIF.list dbg

  let find id vifs = List.find (fun (x, _) -> x.Vif.id = id) vifs |> fst

  let assert_equal vif vif' =
    let open Vif in
    assert_equal ~msg:"id"
      ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b)
      vif.id vif'.id ;
    assert_equal ~msg:"position" ~printer:string_of_int vif.position
      vif'.position ;
    assert_equal ~msg:"mac" ~printer:(fun x -> x) vif.mac vif'.mac ;
    assert_equal ~msg:"carrier" ~printer:string_of_bool vif.carrier vif'.carrier ;
    assert_equal ~msg:"mtu" ~printer:string_of_int vif.mtu vif'.mtu ;
    assert_equal ~msg:"rate"
      ~printer:(function
        | Some (a, b) -> Printf.sprintf "Some %Ld %Ld" a b | None -> "None"
        )
      vif.rate vif'.rate ;
    assert_equal ~msg:"backend"
      ~printer:(fun x -> x |> rpc_of_network_t |> Jsonrpc.to_string)
      vif.backend vif'.backend ;
    assert_equal ~msg:"other_config" ~printer:sl vif.other_config
      vif'.other_config ;
    assert_equal ~msg:"extra_private_keys" ~printer:sl vif.extra_private_keys
      vif'.extra_private_keys
end)

let vbd_plug_ordering_good _ =
  let open Vbd in
  let rw position vid =
    {
      vbd_default_t with
      Vbd.id= (vid, position)
    ; position= None
    ; mode= ReadWrite
    ; backend= Some (Local "/dev/zero")
    ; ty= Disk
    ; unpluggable= true
    ; extra_backend_keys= [("backend", "keys")]
    ; extra_private_keys= [("private", "keys")]
    ; qos= None
    }
  in
  let ro position id = {(rw position id) with mode= ReadOnly} in
  (* We'll try adding the VBDs in both a good order and a bad order. The
     VM.start should plug them in the correct order. *)
  let vbds = [[ro "0"; rw "1"]; [rw "0"; ro "1"]] in
  List.iter
    (fun vbds ->
      with_vm example_uuid (fun id ->
          List.iter
            (fun vbd ->
              let (_ : Vbd.id) = Client.VBD.add dbg (vbd id) in
              ()
            )
            vbds ;
          Client.VM.start dbg id false |> wait_for_task |> success_task ;
          Client.DEBUG.trigger dbg "check-vbd-plug-ordering" [id] ;
          Client.VM.shutdown dbg id None |> wait_for_task |> success_task
      )
    )
    vbds

let ionice_qos_scheduler _ =
  let open Vbd in
  (* Check that we can parse and print the qos_scheduler values *)
  let prios = [Highest; High; Normal; Low; Lowest; Other 499] in
  let xs =
    (Idle :: List.map (fun x -> RealTime x) prios)
    @ List.map (fun x -> BestEffort x) prios
  in
  List.iter
    (fun x ->
      let cls, param = Ionice.to_class_param x in
      let y =
        Ionice.of_class_param_exn (string_of_int cls) (string_of_int param)
      in
      assert_equal ~msg:"qos"
        ~printer:(fun x -> x |> rpc_of_qos_scheduler |> Jsonrpc.to_string)
        x y
    )
    xs

let ionice_output _ =
  let open Vbd in
  let equals =
    [
      ("none: prio 4", None)
    ; ("best-effort: prio 2", Some (BestEffort High))
    ; ("realtime: prio 6", Some (RealTime Low))
    ; ("idle: prio 7", Some Idle)
    ]
  in
  List.iter
    (fun (x, y) ->
      let x' = try Ionice.parse_result_exn x with _ -> None in
      assert_equal ~msg:"qos"
        ~printer:(function
          | None ->
              "None"
          | Some x ->
              x |> rpc_of_qos_scheduler |> Jsonrpc.to_string
          )
        x' y
    )
    equals

let barrier_ordering () =
  (* Test that barriers come out of Updates in the order they are injected *)
  let barrier_ids = [1; 2; 3] in
  List.iter (Client.UPDATES.inject_barrier dbg "barrier") barrier_ids ;
  let barriers, _, _ = Client.UPDATES.get dbg (Some 1) None in
  List.map fst barriers
  |> assert_equal ~msg:"Barriers not in correct order" barrier_ids

let test_ca351823 () =
  let open Suspend_image.Xenops_record in
  let s =
    {|((time 20210219T17:16:27Z)(word_size 64)(vm_str "((id 434d1235-c58b-99f4-f652-c79d57e50719)(name vm-generic-linux)(ssidref 0)(xsdata((vm-data \"\")))(platformdata((featureset 17cbfbff-f7fa3223-2d93fbff-00000023-00000001-000007ab-00000000-00000000-00001000-9c000400)(generation-id \"\")(timeoffset 0)(usb true)(usb_tablet true)(device-model qemu-upstream-compat)(acpi 1)(apic true)(pae true)(hpet true)(vga std)(videoram 8)(nx true)(viridian false)(device_id 0001)))(bios_strings((bios-vendor Xen)(bios-version \"\")(system-manufacturer Xen)(system-product-name \"HVM domU\")(system-version \"\")(system-serial-number \"\")(hp-rombios \"\")(oem-1 Xen)(oem-2 MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d)))(ty(HVM((hap true)(shadow_multiplier 1)(timeoffset 0)(video_mib 8)(video Standard_VGA)(acpi true)(serial(pty))(keymap())(vnc_ip())(pci_emulations())(pci_passthrough false)(boot_order c)(qemu_disk_cmdline false)(qemu_stubdom false))))(suppress_spurious_page_faults false)(machine_address_size())(memory_static_max 4294967296)(memory_dynamic_max 4294967296)(memory_dynamic_min 4294967296)(vcpu_max 2)(vcpus 2)(scheduler_params((priority((256 0)))(affinity())))(on_softreboot(Pause))(on_crash(Pause))(on_shutdown(Shutdown))(on_reboot(Start))(pci_msitranslate true)(pci_power_mgmt false)(has_vendor_device false))")(xs_subtree()))|}
  in
  match of_string s with
  | Ok _ ->
      ()
  | Error e ->
      Printf.sprintf "test_ca351823 failed: %s" (Printexc.to_string e)
      |> failwith

let _ =
  Xenops_utils.set_fs_backend
    (Some (module Xenops_utils.MemFS : Xenops_utils.FS)) ;
  Xenops_server.register_objects () ;
  Xenops_server.set_backend
    (Some (module Xenops_server_simulator : Xenops_server_plugin.S)) ;
  Xcp_client.use_switch := true ;
  Xenops_server.WorkerPool.start 16 ;
  Printexc.record_backtrace true ;
  let suite =
    ( "xenops test"
    , [
        ("test_query", `Quick, test_query)
      ; ("vm_test_remove_missing", `Quick, vm_test_remove_missing)
      ; ("vm_test_add_remove", `Quick, vm_test_add_remove)
      ; ("vm_test_create_destroy", `Quick, vm_test_create_destroy)
      ; ("vm_test_pause_unpause", `Quick, vm_test_pause_unpause)
      ; ("vm_test_build_pause_unpause", `Quick, vm_test_build_pause_unpause)
      ; ("vm_test_build_vcpus", `Quick, vm_test_build_vcpus)
      ; ("vm_test_add_list_remove", `Quick, vm_test_add_list_remove)
      ; ("vm_remove_running", `Quick, vm_remove_running)
      ; ("vm_test_start_shutdown", `Quick, vm_test_start_shutdown)
      ; (* This unit test seems to be non-deterministic, sometimes fails to find tasks
         * "vm_test_parallel_start_shutdown" , `Quick, vm_test_parallel_start_shutdown;
         * *)
        ("vm_test_consoles", `Quick, vm_test_consoles)
      ; ("vm_test_reboot", `Quick, vm_test_reboot)
      ; ("vm_test_halt", `Quick, vm_test_halt)
      ; ("vbd_test_add_remove", `Quick, VbdDeviceTests.add_remove)
      ; ("vbd_test_add_list_remove", `Quick, VbdDeviceTests.add_list_remove)
      ; ("vbd_test_add_vm_remove", `Quick, VbdDeviceTests.add_vm_remove)
      ; ( "vbd_test_add_plug_unplug_remove"
        , `Quick
        , VbdDeviceTests.add_plug_unplug_remove
        )
      ; ( "vbd_test_add_plug_unplug_many_remove"
        , `Quick
        , VbdDeviceTests.add_plug_unplug_many_remove
        )
      ; ("vbd_remove_running", `Quick, VbdDeviceTests.remove_running)
      ; ("vbd_plug_ordering_good", `Quick, vbd_plug_ordering_good)
      ; ("vif_test_add_remove", `Quick, VifDeviceTests.add_remove)
      ; ("vif_test_add_list_remove", `Quick, VifDeviceTests.add_list_remove)
      ; ("vif_test_add_vm_remove", `Quick, VifDeviceTests.add_vm_remove)
      ; ( "vif_test_add_plug_unplug_remove"
        , `Quick
        , VifDeviceTests.add_plug_unplug_remove
        )
      ; ( "vif_test_add_plug_unplug_many_remove"
        , `Quick
        , VifDeviceTests.add_plug_unplug_many_remove
        )
      ; ("vif_remove_running", `Quick, VifDeviceTests.remove_running)
      ; ("vm_test_suspend_resume", `Quick, vm_test_suspend_resume)
      ; ("ionice_qos_scheduler", `Quick, ionice_qos_scheduler)
      ; ("ionice_output", `Quick, ionice_output)
      ; ("barrier_ordering", `Quick, barrier_ordering)
      ; ("test_ca351823", `Quick, test_ca351823)
      ]
    )
  in
  Debug.log_to_stdout () ;
  Alcotest.run "xenops test" [suite; Test_topology.suite]
