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
(**
 * @group Storage
*)

open OUnit

let default_path = "/var/xapi/xenopsd"

open Xenops_interface
open Xenops_client
open Xenops_utils

let usage_and_exit () =
  Printf.fprintf stderr "Usage:\n";
  Printf.fprintf stderr "  %s" Sys.argv.(0);
  exit 1

let dbg = "test"

let expect_exception pred f =
  let exn = 
    try 
      f ();
      failwith "Unexpected success"
    with e ->
      e
  in
  if pred exn then () else raise exn

let fail_running f =
  expect_exception (function Bad_power_state(Running _, Halted) -> true | _ -> false) f

let fail_not_built f = 
  expect_exception (function Domain_not_built -> true | _ -> false) f

let fail_connected f =
  expect_exception (function Device_is_connected -> true | _ -> false) f

let event_wait p =
  let finished = ref false in
  let event_id = ref None in
  while not !finished do
    let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    event_id := Some next_id;
    List.iter (fun d -> if p d then finished := true) deltas;
  done

let wait_for_task id =
  (*	Printf.fprintf stderr "wait_for id = %s\n%!" id; *)
  let finished = function
    | Dynamic.Task id' ->
      id = id' && (task_ended dbg id)
    | x ->
      (*			Printf.fprintf stderr "ignore event on %s\n%!" (x |> Dynamic.rpc_of_id |> Jsonrpc.to_string); *)
      false in 
  event_wait finished;
  id

let verbose_timings = ref false

let wait_for_tasks id =
  let module StringSet = Set.Make(struct type t = string let compare = compare end) in
  let ids = ref (List.fold_left (fun set x -> StringSet.add x set) StringSet.empty id) in
  let event_id = ref None in
  while not(StringSet.is_empty !ids) do
    let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    if !verbose_timings
    then (Printf.fprintf stderr "next_id = %d; deltas = %d" next_id (List.length deltas); flush stderr);
    if List.length deltas = 0 then failwith (Printf.sprintf "no deltas, next_id = %d" next_id);
    event_id := Some next_id;
    List.iter (function
        | Dynamic.Task id' -> if task_ended dbg id' then ids := StringSet.remove id' !ids
        | _ -> ()
      ) deltas
  done

let success_task id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id;
  match t.Task.state with
  | Task.Completed _ -> ()
  | Task.Failed x -> raise (exn_of_exnty (Exception.exnty_of_rpc x))
  | Task.Pending _ -> failwith "task pending"

let fail_not_built_task id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id;
  match t.Task.state with
  | Task.Completed _ -> failwith "task completed successfully: expected Domain_not_built"
  | Task.Failed x -> 
    let exn = exn_of_exnty (Exception.exnty_of_rpc x) in
    begin match exn with 
      | Domain_not_built -> () 
      | _ -> raise exn
    end
  | Task.Pending _ -> failwith "task pending"

let fail_invalid_vcpus_task id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id;
  match t.Task.state with
  | Task.Completed _ -> failwith "task completed successfully: expected Invalid_vcpus"
  | Task.Failed x ->
    let exn = exn_of_exnty (Exception.exnty_of_rpc x) in
    begin match exn with 
      | Invalid_vcpus _ -> ()
      | _ -> raise exn
    end
  | Task.Pending _ -> failwith "task pending"

let test_query _ = let (_: Query.t) = Client.query dbg () in ()

let missing_vm = "missing"

let vm_test_remove_missing _ =
  let exn = 
    try 
      Client.VM.remove dbg missing_vm;
      Some (Failure "VDI.remove succeeded");
    with 
    | Does_not_exist (_,_) -> None
    | e -> Some e
  in
  Opt.iter raise exn

let example_uuid = "c0ffeec0-ffee-c0ff-eec0-ffeec0ffeec0"

let ( ** ) = Int64.mul

let create_vm id =
  let open Vm in
  let _ = PV {
      framebuffer = false;
      framebuffer_ip = Some "0.0.0.0";
      vncterm = true;
      vncterm_ip = None;
      Vm.boot = Indirect {
          bootloader = "pygrub";
          extra_args = "extra";
          legacy_args = "legacy";
          bootloader_args = "bootloader";
          devices = [ Local "0"; Local "1" ]
        }
    } in
  let hvm = HVM {
      hap = true;
      shadow_multiplier = 1.;
      timeoffset = "";
      video_mib = 4;
      video = Cirrus;
      acpi = true;
      serial = None;
      keymap = Some "en-gb";
      vnc_ip = Some "hello";
      pci_emulations = [ "1" ];
      pci_passthrough = false;
      boot_order = "boot";
      qemu_disk_cmdline = false;
      qemu_stubdom = false;
    } in {
    id = id;
    name = "Example: " ^ id;
    ssidref = 1l;
    xsdata = [ "xs", "data" ];
    platformdata = [ "platform", "data" ];
    bios_strings = [ "bios", "strings" ];
    ty = hvm;
    suppress_spurious_page_faults = true;
    machine_address_size = None;
    memory_static_max = 128L ** 1024L ** 1024L;
    memory_dynamic_max = 128L ** 1024L ** 1024L;
    memory_dynamic_min = 128L ** 1024L ** 1024L;
    vcpu_max = 2;
    vcpus = 2;
    scheduler_params = { priority = None; affinity = [] };
    on_crash = [ Vm.Shutdown ];
    on_shutdown = [ Vm.Shutdown ];
    on_reboot = [ Vm.Start ];
    pci_msitranslate = true;
    pci_power_mgmt = false;
    has_vendor_device = false;
  }

let sl x = Printf.sprintf "[ %s ]" (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) x))

let vm_assert_equal vm vm' =
  let open Vm in
  assert_equal ~msg:"id" ~printer:(fun x -> x) vm.id vm'.id;
  assert_equal ~msg:"name" ~printer:(fun x -> x) vm.name vm'.name;
  assert_equal ~msg:"ssidref" ~printer:Int32.to_string vm.ssidref vm'.ssidref;
  assert_equal ~msg:"xsdata" ~printer:sl vm.xsdata vm'.xsdata;
  assert_equal ~msg:"platformdata" ~printer:sl vm.platformdata vm'.platformdata;
  assert_equal ~msg:"bios_strings" ~printer:sl vm.bios_strings vm'.bios_strings;
  assert_equal ~msg:"suppress_spurious_page_faults" ~printer:string_of_bool vm.suppress_spurious_page_faults vm'.suppress_spurious_page_faults;
  assert_equal ~msg:"machine_address_size" ~printer:(function None -> "None" | Some x -> string_of_int x) vm.machine_address_size vm'.machine_address_size;
  assert_equal ~msg:"memory_static_max" ~printer:Int64.to_string vm.memory_static_max vm'.memory_static_max;
  assert_equal ~msg:"memory_dynamic_max" ~printer:Int64.to_string vm.memory_dynamic_max vm'.memory_dynamic_max;
  assert_equal ~msg:"memory_dynamic_min" ~printer:Int64.to_string vm.memory_dynamic_min vm'.memory_dynamic_min;
  assert_equal ~msg:"vcpu_max" ~printer:string_of_int vm.vcpu_max vm'.vcpu_max;
  assert_equal ~msg:"vcpus" ~printer:string_of_int vm.vcpus vm'.vcpus;
  assert_equal ~msg:"on_crash" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.on_crash vm'.on_crash;
  assert_equal ~msg:"on_shutdown" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.on_shutdown vm'.on_shutdown;
  assert_equal ~msg:"on_reboot" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.on_reboot vm'.on_reboot;
  assert_equal ~msg:"has_vendor_device" ~printer:(fun x -> String.concat ", " (List.map (fun x -> x |> Vm.rpc_of_action |> Jsonrpc.to_string) x)) vm.has_vendor_device vm'.has_vendor_device;
  let is_hvm vm = match vm.ty with
    | HVM _ -> true | PV _ -> false in
  assert_equal ~msg:"HVM-ness" ~printer:string_of_bool (is_hvm vm) (is_hvm vm');
  match vm.ty, vm'.ty with
  | HVM _, PV _
  | PV _, HVM _ -> failwith "HVM-ness"
  | HVM h, HVM h' ->
    assert_equal ~msg:"HAP" ~printer:string_of_bool h.hap h'.hap;
    assert_equal ~msg:"shadow_multipler" ~printer:string_of_float h.shadow_multiplier h'.shadow_multiplier;
    assert_equal ~msg:"timeoffset" ~printer:(fun x -> x) h.timeoffset h'.timeoffset;
    assert_equal ~msg:"video_mib" ~printer:string_of_int h.video_mib h'.video_mib;
    assert_equal ~msg:"video" ~printer:(fun x -> x |> rpc_of_video_card |> Jsonrpc.to_string) h.video h'.video;
    assert_equal ~msg:"acpi" ~printer:string_of_bool h.acpi h'.acpi;
    assert_equal ~msg:"serial" ~printer:(Opt.default "None") h.serial h'.serial;
    assert_equal ~msg:"keymap" ~printer:(Opt.default "None") h.keymap h'.keymap;
    assert_equal ~msg:"vnc_ip" ~printer:(Opt.default "None") h.vnc_ip h'.vnc_ip;
    assert_equal ~msg:"pci_emulations" ~printer:(String.concat ";")  h.pci_emulations h'.pci_emulations;
    assert_equal ~msg:"pci_passthrough" ~printer:string_of_bool  h.pci_passthrough h'.pci_passthrough;
    assert_equal ~msg:"boot_order" ~printer:(fun x -> x) h.boot_order h'.boot_order;
    assert_equal ~msg:"qemu_disk_cmdline" ~printer:string_of_bool h.qemu_disk_cmdline h'.qemu_disk_cmdline;
  | PV p, PV p' ->
    assert_equal ~msg:"framebuffer" ~printer:string_of_bool p.framebuffer p'.framebuffer;
    assert_equal ~msg:"vncterm" ~printer:string_of_bool p.vncterm p'.vncterm;
    assert_equal ~msg:"vncterm_ip" ~printer:(Opt.default "None") p.vncterm_ip p'.vncterm_ip;
    begin match p.boot, p'.boot with
      | Direct _, Indirect _
      | Indirect _, Direct _ -> failwith "pv-boot-ness"
      | Direct x, Direct x' ->
        assert_equal ~msg:"kernel" ~printer:(fun x -> x) x.kernel x'.kernel;
        assert_equal ~msg:"cmdline" ~printer:(fun x -> x) x.cmdline x'.cmdline;
        assert_equal ~msg:"ramdisk" ~printer:(function None -> "None" | Some x -> x) x.ramdisk x'.ramdisk		
      | Indirect x, Indirect x' ->
        assert_equal ~msg:"bootloader" ~printer:(fun x -> x) x.bootloader x'.bootloader;
        assert_equal ~msg:"extra_args" ~printer:(fun x -> x) x.extra_args x'.extra_args;
        assert_equal ~msg:"legacy_args" ~printer:(fun x -> x) x.legacy_args x'.legacy_args;
        assert_equal ~msg:"bootloader_args" ~printer:(fun x -> x) x.bootloader_args x'.bootloader_args;
        assert_equal ~msg:"devices" ~printer:(fun x -> x |> rpc_of_disk_list |> Jsonrpc.to_string) x.devices x'.devices;
    end

let with_vm id f =
  let vm = create_vm id in
  let (id: Vm.id) = Client.VM.add dbg vm in
  finally (fun () -> f id)
    (fun () ->
       let _, state = Client.VM.stat dbg id in
       begin match state.Vm.power_state with
         | Running
         | Paused ->
           Printf.fprintf stderr "VM is running or paused; shutting down";
           begin try
               Client.VM.shutdown dbg id None |> wait_for_task |> success_task
             with e ->
               Printf.fprintf stderr "Caught failure during with_vm cleanup: %s" (Printexc.to_string e);
               raise e
           end
         | _ -> ()
       end;
       try
         Client.VM.remove dbg id
       with e ->
         Printf.fprintf stderr "Caught failure during with_vm cleanup: %s" (Printexc.to_string e);
         raise e
    )

let vm_test_add_remove _ =
  with_vm example_uuid (fun _ -> ())


let vm_test_create_destroy _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.destroy dbg id |> wait_for_task |> success_task;
    )

let vm_test_pause_unpause _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> fail_not_built_task;
       Client.VM.pause dbg id |> wait_for_task |> fail_not_built_task;
       Client.VM.destroy dbg id |> wait_for_task |> success_task;
    )

let vm_test_build_pause_unpause _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.build dbg id |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> fail_not_built_task;
       Client.VM.create_device_model dbg id false |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       Client.VM.pause dbg id |> wait_for_task |> success_task;
       Client.VM.destroy dbg id |> wait_for_task |> success_task;
    )

let vm_test_build_vcpus _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.build dbg id |> wait_for_task |> success_task;
       Client.VM.set_vcpus dbg id 1 |> wait_for_task |> fail_not_built_task;
       Client.VM.create_device_model dbg id false |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       Client.VM.set_vcpus dbg id 1 |> wait_for_task |> success_task;
       let state = Client.VM.stat dbg id |> snd in
       if state.Vm.vcpu_target <> 1
       then failwith (Printf.sprintf "vcpu_target %d <> 1" state.Vm.vcpu_target);
       Client.VM.set_vcpus dbg id 2 |> wait_for_task |> success_task;
       let state = Client.VM.stat dbg id |> snd in
       if state.Vm.vcpu_target <> 2
       then failwith (Printf.sprintf "vcpu_target %d <> 2" state.Vm.vcpu_target);
       Client.VM.set_vcpus dbg id 4 |> wait_for_task |> fail_invalid_vcpus_task;
       Client.VM.set_vcpus dbg id 0 |> wait_for_task |> fail_invalid_vcpus_task;
       Client.VM.destroy dbg id |> wait_for_task |> success_task;
    )

let vm_test_add_list_remove _ =
  with_vm example_uuid
    (fun id ->
       let vm = create_vm example_uuid in
       let (vms: (Vm.t * Vm.state) list) = Client.VM.list dbg () in
       let vm' = List.find (fun x -> x.Vm.id = id) (List.map fst vms) in
       vm_assert_equal vm vm'
    )

let vm_remove_running _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.build dbg id |> wait_for_task |> success_task;
       Client.VM.create_device_model dbg id false |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       fail_running (fun () -> Client.VM.remove dbg id);
       Client.VM.destroy dbg id |> wait_for_task |> success_task;
    )

let vm_test_start_shutdown _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.start dbg id |> wait_for_task |> success_task;
       fail_running (fun () -> Client.VM.remove dbg id);
       Client.VM.shutdown dbg id None |> wait_for_task |> success_task;
    )

let vm_test_parallel_start_shutdown _ =
  let rec ints start finish = if start > finish then [] else start :: (ints (start + 1) finish) in
  let ints = ints 0 1000 |> List.map string_of_int in
  let t = Unix.gettimeofday () in
  let ids = List.map
      (fun x ->
         let vm = create_vm x in
         Client.VM.add dbg vm
      ) ints in
  if !verbose_timings
  then (Printf.fprintf stderr "VM.adds took %.1f\n" (Unix.gettimeofday () -. t); flush stderr);
  let t = Unix.gettimeofday () in
  let tasks = List.map (fun id -> let id = Client.VM.start dbg id in (* Printf.fprintf stderr "%s\n" id; flush stderr; *) id) ids in
  wait_for_tasks tasks;
  if !verbose_timings
  then (Printf.fprintf stderr "Cleaning up tasks\n"; flush stderr);
  List.iter success_task tasks;
  if !verbose_timings
  then (Printf.fprintf stderr "VM.starts took %.1f\n" (Unix.gettimeofday () -. t); flush stderr);
  let t = Unix.gettimeofday () in
  let tasks = List.map (fun id -> Client.VM.shutdown dbg id None) ids in
  wait_for_tasks tasks;
  if !verbose_timings
  then (Printf.fprintf stderr "Cleaning up tasks\n"; flush stderr);
  List.iter success_task tasks;
  if !verbose_timings
  then (Printf.fprintf stderr "VM.shutdown took %.1f\n" (Unix.gettimeofday () -. t); flush stderr);
  let t = Unix.gettimeofday () in
  List.iter (fun id -> Client.VM.remove dbg id) ids;
  if !verbose_timings
  then (Printf.fprintf stderr "VM.remove took %.1f\n" (Unix.gettimeofday () -. t); flush stderr);
  ()

let vm_test_consoles _ =
  ()
(*
	with_vm example_uuid
		(fun id ->
			success (Client.VM.start id);
			let (_: Console.t list) = success (Client.CONSOLE.list id) in
			success (Client.VM.shutdown id None);
		)
*)

let vm_test_reboot _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.build dbg id |> wait_for_task |> success_task;
       Client.VM.create_device_model dbg id false |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       let state : Vm.state = Client.VM.stat dbg id |> snd in
       Client.DEBUG.trigger dbg "reboot" [ id ];
       (* ... need to wait for the domain id to change *)
       event_wait
         (function
           | Dynamic.Vm id' ->
             id = id' && (match try Some (Client.VM.stat dbg id) with _ -> None with
                 | Some (_, vm_state) ->
                   vm_state.Vm.domids <> state.Vm.domids
                 | _ -> false
               )
           | _ -> false);
       Client.VM.shutdown dbg id None |> wait_for_task |> success_task;
    )

let vm_test_halt _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.build dbg id |> wait_for_task |> success_task;
       Client.VM.create_device_model dbg id false |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       Client.DEBUG.trigger dbg "halt" [ id ];
       (* ... need to wait for the domain ids to disappear *)
       event_wait
         (function
           | Dynamic.Vm id' ->
             id = id' && (match try Some (Client.VM.stat dbg id) with _ -> None with
                 | Some (_, vm_state) ->
                   vm_state.Vm.domids = []
                 | _ -> false
               )
           | _ -> false);
    )

let vm_test_suspend _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.create dbg id |> wait_for_task |> success_task;
       Client.VM.build dbg id |> wait_for_task |> success_task;
       Client.VM.create_device_model dbg id false |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       Client.VM.suspend dbg id (Local "/tmp/suspend-image") |> wait_for_task |> success_task;
    )

let vm_test_resume _ =
  with_vm example_uuid
    (fun id ->
       Client.VM.resume dbg id (Local "/tmp/suspend-image") |> wait_for_task |> success_task;
       Client.VM.unpause dbg id |> wait_for_task |> success_task;
       Client.VM.shutdown dbg id None |> wait_for_task |> success_task;
       Client.VM.destroy dbg id |> wait_for_task |> success_task;
    )


module type DEVICE = sig
  type t
  type state
  val assert_equal: t -> t -> unit
  type position
  val positions: position list
  type id
  val ids: id list
  val create: id -> position -> t
  val add: t -> id
  val remove: id -> unit
  val plug: id -> Task.id
  val unplug: id -> Task.id
  val list: Vm.id -> (t * state) list
  val find: id -> (t * state) list -> t
end

module DeviceTests = functor(D: DEVICE) -> struct
  open D
  let add_remove _ =
    with_vm example_uuid
      (fun id ->
         let dev = create (List.hd ids) (List.hd positions) in
         let (dev_id: id) = add dev in
         remove dev_id
      )

  let with_added_vm id f =
    with_vm id
      (fun id ->
         Client.VM.create dbg id |> wait_for_task |> success_task;
         finally
           (fun () -> f id)
           (fun () -> 
              Client.VM.destroy dbg id |> wait_for_task |> success_task
           )
      )

  let add_plug_unplug_remove _ =
    with_added_vm example_uuid
      (fun id ->
         let dev = create (List.hd ids) (List.hd positions) in
         let (dev_id: id) = add dev in
         plug dev_id |> wait_for_task |> success_task;
         unplug dev_id |> wait_for_task |> success_task;
         remove dev_id;
      )

  let add_plug_unplug_many_remove _ =
    with_added_vm example_uuid
      (fun id ->
         let ids = 
           List.map
             (fun (id, position) ->
                let dev = create id position in
                let id = add dev in
                plug id |> wait_for_task |> success_task;
                id
             ) (List.combine ids positions) in
         List.iter
           (fun id ->
              unplug id |> wait_for_task |> success_task;
              remove id;
           ) ids
      )

  let add_list_remove _ =
    with_vm example_uuid
      (fun id ->
         let dev = create (List.hd ids) (List.hd positions) in
         let (dev_id: id) = add dev in
         let (devs: (t * state) list) = list id in
         let dev' = find dev_id devs in
         assert_equal dev dev';
         remove dev_id;
      )

  let add_vm_remove _ =
    with_vm example_uuid
      (fun id ->
         let dev = create (List.hd ids) (List.hd positions) in
         let (_: id) = add dev in
         ()
      )

  let remove_running _ =
    with_added_vm example_uuid
      (fun id ->
         let dev = create (List.hd ids) (List.hd positions) in
         let (dev_id: id) = add dev in
         plug dev_id |> wait_for_task |> success_task;
         (* no unplug *)
         fail_connected (fun () -> remove dev_id);
      )
end

module VbdDeviceTests = DeviceTests(struct
    type t = Vbd.t
    type id = Vbd.id
    type state = Vbd.state
    type position = Device_number.t option
    let positions = [ None; None; None ]
    let ids = List.map (fun x -> example_uuid, x) [ "0"; "1"; "2" ]
    let create id position =
      let open Vbd in {
        Vbd.id = id;
        position = position;
        mode = ReadWrite;
        backend = Some (Local "/dev/zero");
        ty = Disk;
        unpluggable = true;
        extra_backend_keys = [ "backend", "keys" ];
        extra_private_keys = [ "private", "keys" ];
        qos = None;
      }
    let add = Client.VBD.add dbg
    let remove = Client.VBD.remove dbg
    let plug = Client.VBD.plug dbg
    let unplug id = Client.VBD.unplug dbg id false
    let list = Client.VBD.list dbg
    let find id vbds = List.find (fun (x, _) -> x.Vbd.id = id) vbds |> fst
    let assert_equal vbd vbd' =
      let open Vbd in
      assert_equal ~msg:"id" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vbd.id vbd'.id;
      assert_equal ~msg:"mode" ~printer:(function ReadWrite -> "RW" | ReadOnly -> "RO") vbd.mode vbd'.mode;
      assert_equal ~msg:"backend" ~printer:(fun x -> Opt.default "None" (Opt.map (fun x -> x |> rpc_of_disk |> Jsonrpc.to_string) x)) vbd.backend vbd'.backend;
      assert_equal ~msg:"unpluggable" ~printer:string_of_bool vbd.unpluggable vbd'.unpluggable;
      assert_equal ~msg:"extra_backend_keys" ~printer:sl vbd.extra_backend_keys vbd'.extra_backend_keys;
      assert_equal ~msg:"extra_private_keys" ~printer:sl vbd.extra_private_keys vbd'.extra_private_keys
  end)

module VifDeviceTests = DeviceTests(struct
    type t = Vif.t
    type id = Vif.id
    type state = Vif.state
    type position = int
    let positions = [ 0; 1; 2 ]
    let ids = List.map (fun x -> example_uuid, x) [ "0"; "1"; "2" ]
    let create id position =
      let open Vif in {
        id = id;
        position = position;
        mac = "c0:ff:ee:c0:ff:ee";
        carrier = false;
        mtu = 1450;
        rate = Some(1L, 2L);
        backend = Network.Local "xenbr0";
        other_config = [ "other", "config" ];
        locking_mode = Vif.Unlocked;
        extra_private_keys = [ "private", "keys" ];
      }
    let add = Client.VIF.add dbg
    let remove = Client.VIF.remove dbg
    let plug = Client.VIF.plug dbg
    let unplug id = Client.VIF.unplug dbg id false
    let list = Client.VIF.list dbg
    let find id vifs = List.find (fun (x, _) -> x.Vif.id = id) vifs |> fst
    let assert_equal vif vif' =
      let open Vif in
      assert_equal ~msg:"id" ~printer:(fun (a, b) -> Printf.sprintf "%s.%s" a b) vif.id vif'.id;
      assert_equal ~msg:"position" ~printer:string_of_int vif.position vif'.position;
      assert_equal ~msg:"mac" ~printer:(fun x -> x) vif.mac vif'.mac;
      assert_equal ~msg:"carrier" ~printer:string_of_bool vif.carrier vif'.carrier;
      assert_equal ~msg:"mtu" ~printer:string_of_int vif.mtu vif'.mtu;
      assert_equal ~msg:"rate" ~printer:(function Some (a, b) -> Printf.sprintf "Some %Ld %Ld" a b | None -> "None") vif.rate vif'.rate;
      assert_equal ~msg:"backend" ~printer:(fun x -> x |> Network.rpc_of_t |> Jsonrpc.to_string) vif.backend vif'.backend;
      assert_equal ~msg:"other_config" ~printer:sl vif.other_config vif'.other_config;
      assert_equal ~msg:"extra_private_keys" ~printer:sl vif.extra_private_keys vif'.extra_private_keys
  end)

let vbd_plug_ordering_good _ =
  let open Vbd in
  let rw position id = {
    Vbd.id = (id, position);
    position = None;
    mode = ReadWrite;
    backend = Some (Local "/dev/zero");
    ty = Disk;
    unpluggable = true;
    extra_backend_keys = [ "backend", "keys" ];
    extra_private_keys = [ "private", "keys" ];
    qos = None;
  } in
  let ro position id = { (rw position id) with mode = ReadOnly } in
  (* We'll try adding the VBDs in both a good order and a bad order.
     	   The VM.start should plug them in the correct order. *)
  let vbds = [
    [ ro "0"; rw "1" ];
    [ rw "0"; ro "1" ];
  ] in
  List.iter
    (fun vbds ->
       with_vm example_uuid
         (fun id ->
            List.iter
              (fun vbd ->
                 let (_: Vbd.id) = Client.VBD.add dbg (vbd id) in
                 ()
              ) vbds;
            Client.VM.start dbg id |> wait_for_task |> success_task;
            Client.DEBUG.trigger dbg "check-vbd-plug-ordering" [ id ];
            Client.VM.shutdown dbg id None |> wait_for_task |> success_task;
         ) 
    ) vbds

let ionice_qos_scheduler _ =
  let open Vbd in
  (* Check that we can parse and print the qos_scheduler values *)
  let prios = [ Highest; High; Normal; Low; Lowest; Other 499 ] in
  let xs = Idle :: (List.map (fun x -> RealTime x) prios) @ (List.map (fun x -> BestEffort x) prios) in
  List.iter
    (fun x ->
       let cls, param = Ionice.to_class_param x in
       let y = Ionice.of_class_param_exn (string_of_int cls) (string_of_int param) in
       assert_equal ~msg:"qos" ~printer:(fun x -> x |> rpc_of_qos_scheduler |> Jsonrpc.to_string) x y
    ) xs

let ionice_output _ =
  let open Vbd in
  let equals = [
    "none: prio 4", None;
    "best-effort: prio 2", Some(BestEffort High);
    "realtime: prio 6", Some(RealTime Low);
    "idle: prio 7", Some Idle;
  ] in
  List.iter
    (fun (x, y) ->
       let x' = try Ionice.parse_result_exn x with _ -> None in
       assert_equal ~msg:"qos" ~printer:(function None -> "None"
                                                | Some x -> x |> rpc_of_qos_scheduler |> Jsonrpc.to_string
         ) x' y
    ) equals

let barrier_ordering () =
  (* Test that barriers come out of Updates in the order they are injected *)
  let barrier_ids = [1; 2; 3] in
  List.iter (Client.UPDATES.inject_barrier dbg "barrier") barrier_ids;
  let (barriers, _, _) = Client.UPDATES.get dbg (Some 1) None in
  List.map fst barriers
  |> assert_equal ~msg:"Barriers not in correct order" barrier_ids

let _ =
  let verbose = ref false in

  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
    "-path", Arg.String Xenops_client.set_sockets_dir, "Set the directory containing the unix domain sockets";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
    "Test xenopd service";

  let suite = "xenops test" >::: 
              [
                "test_query" >:: test_query;
                "vm_test_remove_missing" >:: vm_test_remove_missing;
                "vm_test_add_remove" >:: vm_test_add_remove;
                "vm_test_create_destroy" >:: vm_test_create_destroy;
                "vm_test_pause_unpause" >:: vm_test_pause_unpause;
                "vm_test_build_pause_unpause" >:: vm_test_build_pause_unpause;
                "vm_test_build_vcpus" >:: vm_test_build_vcpus;
                "vm_test_add_list_remove" >:: vm_test_add_list_remove;
                "vm_remove_running" >:: vm_remove_running;
                "vm_test_start_shutdown" >:: vm_test_start_shutdown;
                "vm_test_parallel_start_shutdown" >:: vm_test_parallel_start_shutdown;
                "vm_test_consoles" >:: vm_test_consoles;
                "vm_test_reboot" >:: vm_test_reboot;
                "vm_test_halt" >:: vm_test_halt;
                "vbd_test_add_remove" >:: VbdDeviceTests.add_remove;
                "vbd_test_add_list_remove" >:: VbdDeviceTests.add_list_remove;
                "vbd_test_add_vm_remove" >:: VbdDeviceTests.add_vm_remove;
                "vbd_test_add_plug_unplug_remove" >:: VbdDeviceTests.add_plug_unplug_remove;
                "vbd_test_add_plug_unplug_many_remove" >:: VbdDeviceTests.add_plug_unplug_many_remove;
                "vbd_remove_running" >:: VbdDeviceTests.remove_running;
                "vbd_plug_ordering_good" >:: vbd_plug_ordering_good;
                "vif_test_add_remove" >:: VifDeviceTests.add_remove;
                "vif_test_add_list_remove" >:: VifDeviceTests.add_list_remove;
                "vif_test_add_vm_remove" >:: VifDeviceTests.add_vm_remove;
                "vif_test_add_plug_unplug_remove" >:: VifDeviceTests.add_plug_unplug_remove;
                "vif_test_add_plug_unplug_many_remove" >:: VifDeviceTests.add_plug_unplug_many_remove;
                "vif_remove_running" >:: VifDeviceTests.remove_running;
                "vm_test_suspend" >:: vm_test_suspend;
                "vm_test_resume" >:: vm_test_resume;
                "ionice_qos_scheduler" >:: ionice_qos_scheduler;
                "ionice_output" >:: ionice_output;
                "barrier_ordering" >:: barrier_ordering;
              ] in

  (*	if !verbose then Debug.log_to_stdout (); *)

  run_test_tt ~verbose:!verbose suite

