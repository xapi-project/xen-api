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
open Xenops_interface
open Xenops_server_plugin
open Xenops_utils
open Xenops_task

module D = Debug.Make(struct let name = "xenops_server_simulator" end)
open D

let simplified = false

module Domain = struct
  type t = {
    domid: int;
    uuid: string;
    hvm: bool;
    domain_action_request: domain_action_request option;
    paused: bool;
    built: bool;
    vcpus: int;
    shadow_multiplier: float;
    memory_dynamic_min: int64;
    memory_dynamic_max: int64;
    qemu_created: bool;
    suspended: bool;
    vbds: Vbd.t list; (* maintained in reverse-plug order *)
    vifs: Vif.t list;
    pcis: Pci.t list;
    vgpus: Vgpu.t list;
    vusbs: Vusb.t list;
    xsdata: (string * string) list;
    last_create_time: float;
  } [@@deriving rpcty]
end

module DB = TypedTable(struct
    include Domain
    let namespace = "domain"
    type key = string
    let key x = [ x ]
  end)

let updates = Updates.empty scheduler

let next_domid =
  let next = ref None in (* unknown *)
  let get_next =
    fun () -> match !next with
      | None ->
        let domains = List.filter_map DB.read (DB.list []) in
        let domids = List.map (fun x -> x.Domain.domid) domains in
        let highest = List.fold_left max 0 domids in
        next := Some (highest + 1);
        highest + 1
      | Some x -> x in
  let incr_next () = next := Opt.map (fun x -> x + 1) !next in
  fun () ->
    let result = get_next () in
    incr_next ();
    debug "using domid %d" result;
    result

let m = Mutex.create ()

let create_nolock _ vm () =
  debug "Domain.create vm=%s" vm.Vm.id;
  if DB.exists vm.Vm.id then begin
    debug "VM.create_nolock %s: Already_exists" vm.Vm.id;
    raise (Xenopsd_error (Already_exists("domain", vm.Vm.id)))
  end else begin
    let open Domain in
    let domain = {
      domid = next_domid ();
      uuid = vm.Vm.id;
      hvm = (match vm.Vm.ty with Vm.HVM _ -> true | _ -> false);
      domain_action_request = None;
      paused = true;
      built = false;
      vcpus = vm.Vm.vcpus;
      shadow_multiplier = (match vm.Vm.ty with Vm.HVM {
        Vm.shadow_multiplier = x ; _ } -> x | _ -> 1.);
      memory_dynamic_min = vm.Vm.memory_dynamic_min;
      memory_dynamic_max = vm.Vm.memory_dynamic_max;
      qemu_created = false;
      suspended = false;
      vifs = [];
      vbds = [];
      pcis = [];
      vgpus = [];
      vusbs = [];
      xsdata = vm.Vm.xsdata;
      last_create_time = Unix.gettimeofday ();
    } in
    DB.write vm.Vm.id domain
  end

let get_state_nolock vm () =
  if DB.exists vm.Vm.id then begin
    let d = DB.read_exn vm.Vm.id in
    { halted_vm with
      Vm.power_state = Running;
      domids = [ d.Domain.domid ];
      vcpu_target = d.Domain.vcpus;
      last_start_time = d.Domain.last_create_time;
    }
  end else halted_vm

let get_domain_action_request_nolock vm () =
  if DB.exists vm.Vm.id then begin
    let d = DB.read_exn vm.Vm.id in
    d.Domain.domain_action_request
  end else Some Needs_poweroff

let destroy_nolock vm () =
  debug "Domain.destroy vm=%s" vm.Vm.id;
  (* Idempotent *)
  if DB.exists vm.Vm.id then DB.delete vm.Vm.id

let build_nolock vm _vbds _vifs _vgpus _vusbs _extras () =
  debug "Domain.build vm=%s" vm.Vm.id;
  debug "setting built <- true";
  DB.write vm.Vm.id { (DB.read_exn vm.Vm.id) with Domain.built = true }

let create_device_model_nolock vm () =
  debug "Domain.create_device_model vm=%s" vm.Vm.id;
  DB.write vm.Vm.id { (DB.read_exn vm.Vm.id) with Domain.qemu_created = true }

let destroy_device_model_nolock vm () =
  debug "Domain.destroy_device_model vm=%s" vm.Vm.id;
  if DB.exists vm.Vm.id
  then DB.write vm.Vm.id { (DB.read_exn vm.Vm.id) with Domain.qemu_created = false }
  else warn "Domain.destroy_device_model vm=%s: no device model exists" vm.Vm.id

let request_shutdown_nolock vm reason () =
  DB.write vm.Vm.id { (DB.read_exn vm.Vm.id) with Domain.domain_action_request =
                                                    Some (match reason with
                                                        | Halt | PowerOff -> Needs_poweroff
                                                        | Reboot -> Needs_reboot
                                                        | Suspend | S3Suspend -> Needs_suspend)
                    };
  Updates.add (Dynamic.Vm vm.Vm.id) updates;
  true

let save_nolock vm _ _data _vgpu_data () =
  DB.write vm.Vm.id { (DB.read_exn vm.Vm.id) with Domain.suspended = true }

let restore_nolock vm _vbds _vifs _data _vgpu_data _extras () =
  DB.write vm.Vm.id { (DB.read_exn vm.Vm.id) with Domain.built = true }

let do_pause_unpause_nolock vm paused () =
  let d = DB.read_exn vm.Vm.id in
  if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
  then raise (Xenopsd_error Domain_not_built)
  else DB.write vm.Vm.id { d with Domain.paused = paused }

let do_set_xsdata_nolock vm xsdata () =
  let d = DB.read_exn vm.Vm.id in
  DB.write vm.Vm.id { d with Domain.xsdata = xsdata }

let do_set_vcpus_nolock vm n () =
  let d = DB.read_exn vm.Vm.id in
  if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
  then raise (Xenopsd_error Domain_not_built)
  else DB.write vm.Vm.id { d with Domain.vcpus = n }

let do_set_shadow_multiplier_nolock vm m () =
  let d = DB.read_exn vm.Vm.id in
  if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
  then raise (Xenopsd_error Domain_not_built)
  else DB.write vm.Vm.id { d with Domain.shadow_multiplier = m }

let do_set_memory_dynamic_range_nolock vm min max () =
  let d = DB.read_exn vm.Vm.id in
  DB.write vm.Vm.id { d with Domain.memory_dynamic_min = min; memory_dynamic_max = max }

let add_vif vm vif () =
  let d = DB.read_exn vm in
  let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
  if List.mem vif.Vif.position existing_positions then begin
    debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
    raise (Xenopsd_error (Already_exists("vif", string_of_int vif.Vif.position)))
  end else DB.write vm { d with Domain.vifs = vif :: d.Domain.vifs }

let add_vbd (vm: Vm.id) (vbd: Vbd.t) () =
  debug "add_vbd";
  let d = DB.read_exn vm in
  (* there shouldn't be any None values in here anyway *)
  let ps = List.map (fun vbd -> vbd.Vbd.position) d.Domain.vbds in
  assert (not (List.mem None ps));
  let dns = List.map (Opt.unbox) ps in
  let indices = List.map Device_number.to_disk_number dns in
  let next_index = List.fold_left max (-1) indices + 1 in
  let next_dn = Device_number.of_disk_number d.Domain.hvm next_index in
  let this_dn = Opt.default next_dn vbd.Vbd.position in
  if List.mem this_dn dns then begin
    debug "VBD.plug %s.%s: Already exists" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
    raise (Xenopsd_error (Already_exists("vbd", Device_number.to_debug_string this_dn)))
  end else DB.write vm { d with Domain.vbds = { vbd with Vbd.position = Some this_dn } :: d.Domain.vbds }

let move_vif vm vif network () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  match List.filter this_one d.Domain.vifs with
  | [ vif ] ->
    let vifs = List.filter (fun x -> not(this_one x)) d.Domain.vifs in
    let vif = { vif with Vif.backend = network } in
    DB.write vm { d with Domain.vifs = vif :: vifs }
  | [] ->
    raise (Xenopsd_error (Does_not_exist("VIF", Printf.sprintf "%s.%s" (fst vif.Vif.id) (snd vif.Vif.id))))
  | _ -> assert false (* at most one *)

let add_pci (vm: Vm.id) (pci: Pci.t) () =
  debug "add_pci";
  let d = DB.read_exn vm in
  let existing_positions = List.map (fun pci -> pci.Pci.position) d.Domain.pcis in
  if List.mem pci.Pci.position existing_positions then begin
    debug "PCI.plug %s.%s: Already exists" (fst pci.Pci.id) (snd pci.Pci.id);
    raise (Xenopsd_error (Already_exists("pci", string_of_int pci.Pci.position)))
  end else DB.write vm { d with Domain.pcis = pci :: d.Domain.pcis }

let pci_state vm pci () =
  if not (DB.exists vm)
  then unplugged_pci
  else
    let d = DB.read_exn vm in
    let this_one x = x.Pci.id = pci.Pci.id in
    match List.filter this_one d.Domain.pcis with
    | [ _pci ] ->
      {
        Pci.plugged = true
      }
    | [] -> unplugged_pci
    | _ -> assert false (* at most one *)

let vgpu_state vm vgpu () =
  if not (DB.exists vm)
  then unplugged_vgpu
  else
    let d = DB.read_exn vm in
    let this_one x = x.Vgpu.id = vgpu.Vgpu.id in
    match List.filter this_one d.Domain.vgpus with
    | [ _vgpu ] ->
      {
        unplugged_vgpu with
        Vgpu.plugged = true;
      }
    | [] -> unplugged_vgpu
    | _ -> assert false (* at most one *)

let vusb_state vm vusb () =
  if not (DB.exists vm)
  then unplugged_vusb
  else
    let d = DB.read_exn vm in
    let this_one x = x.Vusb.id = vusb.Vusb.id in
    match List.filter this_one d.Domain.vusbs with
    | [ _vusb ] ->
      {
        Vusb.plugged = true;
      }
    | [] -> unplugged_vusb
    | _ -> assert false (* at most one *)

let vbd_state vm vbd () =
  if not (DB.exists vm)
  then unplugged_vbd
  else
    let d = DB.read_exn vm in
    let this_one x = x.Vbd.id = vbd.Vbd.id in
    match List.filter this_one d.Domain.vbds with
    | [ vbd ] ->
      {
        unplugged_vbd with
        Vbd.plugged = true;
        backend_present = vbd.Vbd.backend
      }
    | [] -> unplugged_vbd
    | _ -> assert false (* at most one *)


let vif_state vm vif () =
  if not (DB.exists vm)
  then unplugged_vif
  else
    let d = DB.read_exn vm in
    let this_one x = x.Vif.id = vif.Vif.id in
    match List.filter this_one d.Domain.vifs with
    | [ _domain ] ->
      {
        unplugged_vif with
        Vif.plugged = true;
      }
    | [] -> unplugged_vif
    | _ -> assert false (* at most one *)

let remove_vif vm vif () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  if List.filter this_one d.Domain.vifs = []
  then raise (Xenopsd_error (Does_not_exist("VIF", Printf.sprintf "%s.%s" (fst vif.Vif.id) (snd vif.Vif.id))))
  else DB.write vm { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs }

let set_carrier vm vif carrier () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  let vifs = List.map (fun vif -> { vif with Vif.carrier = if this_one vif then carrier else vif.Vif.carrier }) d.Domain.vifs in
  DB.write vm { d with Domain.vifs = vifs }

let set_locking_mode vm vif mode () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  let vifs = List.map (fun vif -> { vif with Vif.locking_mode = if this_one vif then mode else vif.Vif.locking_mode }) d.Domain.vifs in
  DB.write vm { d with Domain.vifs = vifs }

let set_ipv4_configuration vm vif ipv4_configuration () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  let vifs = List.map (fun vif -> { vif with Vif.ipv4_configuration = if this_one vif then ipv4_configuration else vif.Vif.ipv4_configuration }) d.Domain.vifs in
  DB.write vm { d with Domain.vifs = vifs }

let set_ipv6_configuration vm vif ipv6_configuration () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  let vifs = List.map (fun vif -> { vif with Vif.ipv6_configuration = if this_one vif then ipv6_configuration else vif.Vif.ipv6_configuration }) d.Domain.vifs in
  DB.write vm { d with Domain.vifs = vifs }

let set_pvs_proxy vm vif proxy () =
  let d = DB.read_exn vm in
  let this_one x = x.Vif.id = vif.Vif.id in
  let vifs = List.map (fun vif -> { vif with Vif.pvs_proxy = if this_one vif then proxy else vif.Vif.pvs_proxy }) d.Domain.vifs in
  DB.write vm { d with Domain.vifs = vifs }

let remove_pci vm pci () =
  let d = DB.read_exn vm in
  let this_one x = x.Pci.id = pci.Pci.id in
  if List.filter this_one d.Domain.pcis = []
  then raise (Xenopsd_error (Does_not_exist("PCI", Printf.sprintf "%s.%s" (fst pci.Pci.id) (snd pci.Pci.id))))
  else DB.write vm { d with Domain.pcis = List.filter (fun x -> not (this_one x)) d.Domain.pcis }

let remove_vbd vm vbd () =
  let d = DB.read_exn vm in
  let this_one x = x.Vbd.id = vbd.Vbd.id in
  if List.filter this_one d.Domain.vbds = []
  then raise (Xenopsd_error (Does_not_exist("VBD", Printf.sprintf "%s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id))))
  else DB.write vm { d with Domain.vbds = List.filter (fun x -> not (this_one x)) d.Domain.vbds }

let set_qos_vbd vm vbd () =
  let d = DB.read_exn vm in
  let this_one x = x.Vbd.id = vbd.Vbd.id in
  if List.filter this_one d.Domain.vbds = []
  then raise (Xenopsd_error (Does_not_exist("VBD", Printf.sprintf "%s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id))));
  (* XXX *)
  ()

module HOST = struct
  include Xenops_server_skeleton.HOST

  let get_console_data () = "Xen simulator"
  let get_total_memory_mib () = Int64.mul 1024L 1024L (* 1 TiB *)
  let send_debug_keys _ = ()
end
module VM = struct
  include Xenops_server_skeleton.VM

  let add _vm = ()
  let remove _vm = ()
  let create _ memory_limit vm _ = Mutex.execute m (create_nolock memory_limit vm)
  let destroy _ vm = Mutex.execute m (destroy_nolock vm)
  let pause _ vm = Mutex.execute m (do_pause_unpause_nolock vm true)
  let unpause _ vm = Mutex.execute m (do_pause_unpause_nolock vm false)
  let set_xsdata _ vm xs = Mutex.execute m (do_set_xsdata_nolock vm xs)
  let set_vcpus _ vm n = Mutex.execute m (do_set_vcpus_nolock vm n)
  let set_shadow_multiplier _ vm n = Mutex.execute m (do_set_shadow_multiplier_nolock vm n)
  let set_memory_dynamic_range _ vm min max = Mutex.execute m (do_set_memory_dynamic_range_nolock vm min max)
  let build ?restore_fd:_ _ vm vbds vifs vgpus vusbs extras _force = Mutex.execute m (build_nolock vm vbds vifs vgpus vusbs extras)
  let create_device_model _ vm _vbds _vifs _vgpus _vusbs _ = Mutex.execute m (create_device_model_nolock vm)
  let destroy_device_model _ vm = Mutex.execute m (destroy_device_model_nolock vm)
  let request_shutdown _ vm reason _ack_delay = Mutex.execute m (request_shutdown_nolock vm reason)
  let wait_shutdown _ _vm _reason _timeout = true

  let save _ _cb vm flags data vgpu_data _pre_suspend_callback = Mutex.execute m (save_nolock vm flags data vgpu_data)
  let restore _ _cb vm vbds vifs data vgpu_data extras = Mutex.execute m (restore_nolock vm vbds vifs data vgpu_data extras)

  let s3suspend _ _vm = ()
  let s3resume _ _vm = ()

  let get_state vm = Mutex.execute m (get_state_nolock vm)

  let request_rdp _vm _enabled = ()
  let run_script _ _vm _script = Rpc.Dict [("rc", Rpc.Int 0L); ("stdout", Rpc.String ""); ("stderr", Rpc.String "")]
  let set_domain_action_request _vm _request = ()
  let get_domain_action_request vm = Mutex.execute m (get_domain_action_request_nolock vm)

  let generate_state_string _vm = ""
  let get_internal_state vdi_map vif_map vm =
    let state = Opt.unbox (DB.read vm.Vm.id) in
    let vbds = List.map (fun vbd -> {vbd with Vbd.backend = Opt.map (remap_vdi vdi_map) vbd.Vbd.backend}) state.Domain.vbds in
    let vifs = List.map (fun vif -> remap_vif vif_map vif) state.Domain.vifs in
    {state with Domain.vbds = vbds; Domain.vifs = vifs} |> rpc_of Domain.t |> Jsonrpc.to_string
  let set_internal_state vm s =
    match Rpcmarshal.unmarshal Domain.t.Rpc.Types.ty (Jsonrpc.of_string s) with
    | Ok s -> DB.write vm.Vm.id s
    | Error (`Msg m) -> raise (Xenopsd_error (Internal_error (Printf.sprintf "Failed to unmarshal Domain.t: %s" m)))

  let wait_ballooning _ _ = ()

  let minimum_reboot_delay = 0.
end

module PCI = struct
  let plug _ (vm: Vm.id) (pci: Pci.t) = Mutex.execute m (add_pci vm pci)
  let unplug _ vm pci = Mutex.execute m (remove_pci vm pci)

  let get_state vm pci = Mutex.execute m (pci_state vm pci)

  let get_device_action_request _vm _pci = None
end

module VGPU = struct
  include Xenops_server_skeleton.VGPU
  let get_state vm vgpu = Mutex.execute m (vgpu_state vm vgpu)
end

module VUSB = struct
  let plug _ _vm _vusb = ()
  let unplug _ _vm _vusb = ()
  let get_state vm vusb = Mutex.execute m (vusb_state vm vusb)
  let get_device_action_request _vm _vusb = None
end

module VBD = struct
  let set_active _ (_vm: Vm.id) (_vbd: Vbd.t) (_b: bool) = ()
  let epoch_begin _ (_vm: Vm.id) (_disk: disk) (_persistent: bool) = ()
  let epoch_end _ (_vm: Vm.id) (_disk: disk) = ()
  let plug _ (vm: Vm.id) (vbd: Vbd.t) = Mutex.execute m (add_vbd vm vbd)
  let unplug _ vm vbd _ = Mutex.execute m (remove_vbd vm vbd)

  let insert _ _vm _vbd _disk = ()
  let eject _ _vm _vbd = ()

  let set_qos _ vm vbd = Mutex.execute m (set_qos_vbd vm vbd)

  let get_state vm vbd = Mutex.execute m (vbd_state vm vbd)

  let get_device_action_request _vm _vbd = None
end

module VIF = struct
  let set_active _ (_vm: Vm.id) (_vif: Vif.t) (_b: bool) = ()
  let plug _ vm vif = Mutex.execute m (add_vif vm vif)
  let unplug _ vm vif _ = Mutex.execute m (remove_vif vm vif)
  let move _ vm vif network = Mutex.execute m (move_vif vm vif network)
  let set_carrier _ vm vif carrier = Mutex.execute m (set_carrier vm vif carrier)
  let set_locking_mode _ vm vif mode = Mutex.execute m (set_locking_mode vm vif mode)
  let set_ipv4_configuration _ vm vif ipv4_configuration = Mutex.execute m (set_ipv4_configuration vm vif ipv4_configuration)
  let set_ipv6_configuration _ vm vif ipv6_configuration = Mutex.execute m (set_ipv6_configuration vm vif ipv6_configuration)
  let set_pvs_proxy _ vm vif proxy = Mutex.execute m (set_pvs_proxy vm vif proxy)

  let get_state vm vif = Mutex.execute m (vif_state vm vif)

  let get_device_action_request _vm _vif = None
end

module UPDATES = struct
  let get last timeout = Updates.get "UPDATES.get" last timeout updates
end

module DEBUG = struct
  let trigger cmd args = match cmd, args with
    | "reboot", [ k ] ->
      let d = DB.read_exn k in
      DB.write k { d with Domain.domain_action_request = Some Needs_reboot };
      Updates.add (Dynamic.Vm k) updates
    | "halt", [ k ] ->
      let d = DB.read_exn k in
      DB.write k { d with Domain.domain_action_request = Some Needs_poweroff };
      Updates.add (Dynamic.Vm k) updates
    | "check-vbd-plug-ordering", [ k ] ->
      let d = DB.read_exn k in
      let open Vbd in
      let plug_order = List.rev d.Domain.vbds in
      let rw, ro = List.partition (fun vbd -> vbd.mode = ReadWrite) plug_order in
      if rw @ ro <> plug_order then begin
        debug "DEBUG.trigger: check-vbd-plug-ordering: ordering violation";
        raise (Xenopsd_error (Internal_error "check-vbd-plug-ordering"))
      end
    | _ ->
      debug "DEBUG.trigger cmd=%s Not_supported" cmd;
      raise (Xenopsd_error (Unimplemented(cmd)))
end

let init () = ()
