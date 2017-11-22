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

#include "../config.ml"

open Stdext
open Xenops_interface
open Xenops_utils
open Xenops_server_plugin
open Xenops_helpers
open Xenstore
open Xenops_utils
open Xenops_task

module D = Debug.Make(struct let name = service_name end)
open D
module DX = Debug.Make(struct let name = "libxl" end)

let simplified = true

(* libxl_internal.h:DISABLE_UDEV_PATH *)
let disable_udev_path = "libxl/disable_udev"

let store_domid = 0
let console_domid = 0

let suspend_save_signature = "XenSavedDomain\n"
exception Restore_signature_mismatch

exception Domain_stuck_in_dying_state of domid

let minimum_videoram mib =
  let minimum = if !Xenopsd.use_upstream_qemu then 8 else 4 in
  if mib < minimum
  then warn "VM configuration has illegal videoram value: %d (minimum is %d) MiB. Using the minimum value instead." mib minimum;
  max minimum mib

(* libxl logging and context *)

let vmessage min_level level errno ctx msg =
  let errno_str = match errno with None -> "" | Some s -> Printf.sprintf ": errno=%d" s
  and ctx_str = match ctx with None -> "" | Some s -> Printf.sprintf "%s" s in
  if compare min_level level <= 0 then
    let open Xentoollog in
    match level with
    | Debug ->
      DX.debug "%s%s: %s" ctx_str errno_str msg
    | Verbose
    | Detail
    | Progress
    | Info
    | Notice ->
      DX.info "%s%s: %s" ctx_str errno_str msg
    | Warn ->
      DX.warn "%s%s: %s" ctx_str errno_str msg
    | Error ->
      DX.error "%s%s: %s" ctx_str errno_str msg
    | Critical ->
      DX.error "CRITICAL: %s%s: %s" ctx_str errno_str msg

let progress ctx what percent dne total =
  let nl = if dne = total then "\n" else "" in
  DX.debug "\rProgress %s %d%% (%Ld/%Ld)%s" what percent dne total nl

let create_logger ?(level=Xentoollog.Info) () =
  let open Xentoollog in
  let cbs = {
    vmessage = vmessage level;
    progress = progress;
  } in
  create "Xentoollog.syslog_logger" cbs

(* NOTE: We need to keep the following two references "alive", such that the
 * OCaml GC does not clean up the accociated libxl C variables! *)
let logger = ref None
let ctx = ref None

let with_ctx f =
  match !ctx with
  | None ->
    error "No libxl context!";
    failwith "No libxl context!"
  | Some ctx' ->
    try
      f ctx'
    with Xenlight.Error (a, s) as e ->
      error "Xenlight error: %s: %s" (Xenlight.string_of_error a) s;
      raise e

let get_uuid domid =
  let open Xenlight.Dominfo in
  with_ctx (fun ctx -> (get ctx domid).uuid) |> Xenctrl_uuid.uuid_of_handle |> Uuidm.to_string

(* *)

let run cmd args =
  debug "%s %s" cmd (String.concat " " args);
  fst(Forkhelpers.execute_command_get_output cmd args)

type qemu_frontend =
  | Name of string (* block device path or bridge name *)
  | Device of Device_common.device
  [@@deriving rpc]

type attached_vdi = {
  domid: int;
  attach_info: Storage_interface.attach_info;
} [@@deriving rpc]

(* The following module contains left-overs from the "classic" domain.ml
   Note: "has_vendor_device" parameter won't do anything for the libxl backend
   	(i.e. the xenstore key won't be written) *)
module Domain = struct
  type create_info = {
    ssidref: int32;
    hvm: bool;
    hap: bool;
    name: string;
    xsdata: (string * string) list;
    platformdata: (string * string) list;
    bios_strings: (string * string) list;
    has_vendor_device: bool;
  } [@@deriving rpc]

  type build_hvm_info = {
    shadow_multiplier: float;
    video_mib: int;
  } [@@deriving rpc]

  type build_pv_info = {
    cmdline: string;
    ramdisk: string option;
  } [@@deriving rpc]

  type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info
    [@@deriving rpc]

  type build_info = {
    memory_max: int64;    (* memory max in kilobytes *)
    memory_target: int64; (* memory target in kilobytes *)
    kernel: string;       (* in hvm case, point to hvmloader *)
    vcpus: int;           (* vcpus max *)
    priv: builder_spec_info;
  } [@@deriving rpc]

  let allowed_xsdata_prefixes = [ "vm-data"; "FIST" ]

  let set_xsdata ~xs domid xsdata =
    (* disallowed by default; allowed only if it has one of a set of prefixes *)
    let filtered_xsdata =
      let allowed (x, _) =
        List.fold_left (||) false
          (List.map
             (fun p -> String.startswith (p ^ "/") x)
             allowed_xsdata_prefixes) in
      List.filter allowed in

    let dom_path = Printf.sprintf "/local/domain/%d" domid in
    Xs.transaction xs (fun t ->
        List.iter (fun x -> t.Xst.rm (dom_path ^ "/" ^ x)) allowed_xsdata_prefixes;
        t.Xst.writev dom_path (filtered_xsdata xsdata))

  let wait_xen_free_mem ?(maximum_wait_time_seconds=64) required_memory_kib : bool =
    let open Memory in
    let open Xenlight.Physinfo in
    let rec wait accumulated_wait_time_seconds =
      let host_info = with_ctx (fun ctx -> get ctx) in
      let free_memory_kib =
        kib_of_pages host_info.free_pages in
      let scrub_memory_kib =
        kib_of_pages host_info.scrub_pages in
      (* At exponentially increasing intervals, write *)
      (* a debug message saying how long we've waited: *)
      if is_power_of_2 accumulated_wait_time_seconds then debug
          "Waited %i second(s) for memory to become available: \
           			%Ld KiB free, %Ld KiB scrub, %Ld KiB required"
          accumulated_wait_time_seconds
          free_memory_kib scrub_memory_kib required_memory_kib;
      if free_memory_kib >= required_memory_kib
      (* We already have enough memory. *)
      then true else
      if scrub_memory_kib = 0L
      (* We'll never have enough memory. *)
      then false else
      if accumulated_wait_time_seconds >= maximum_wait_time_seconds
      (* We've waited long enough. *)
      then false else
        begin
          Thread.delay 1.0;
          wait (accumulated_wait_time_seconds + 1)
        end in
    wait 0

  let set_memory_dynamic_range ~xc ~xs ~min ~max domid =
    let kvs = [
      "dynamic-min", string_of_int min;
      "dynamic-max", string_of_int max;
    ] in
    let uuid = get_uuid domid in
    debug "VM = %s; domid = %d; set_memory_dynamic_range min = %d; max = %d"
      uuid domid min max;
    xs.Xs.writev (Printf.sprintf "%s/memory" (xs.Xs.getdomainpath domid)) kvs

  let set_action_request ~xs domid x =
    let path = xs.Xs.getdomainpath domid ^ "/action-request" in
    match x with
    | None -> xs.Xs.rm path
    | Some v -> xs.Xs.write path v

  let get_action_request ~xs domid =
    let path = xs.Xs.getdomainpath domid ^ "/action-request" in
    try
      Some (xs.Xs.read path)
    with Xs_protocol.Enoent _ -> None

end

module VmExtra = struct
  (** Extra data we store per VM. The persistent data is preserved when
      			the domain is suspended so it can be re-used in the following 'create'
      			which is part of 'resume'. The non-persistent data will be regenerated.
      			When a VM is shutdown for other reasons (eg reboot) we throw all this
      			information away and generate fresh data on the following 'create' *)
  type persistent_t = {
    build_info: Domain.build_info option;
    ty: Vm.builder_info option;
    last_start_time: float;
    nomigrate: bool;  (* platform:nomigrate   at boot time *)
    nested_virt: bool (* platform:nested_virt at boot time *)
  } [@@deriving rpc]

  type non_persistent_t = {
    create_info: Domain.create_info;
    vcpu_max: int;
    vcpus: int;
    shadow_multiplier: float;
    memory_static_max: int64;
    suspend_memory_bytes: int64;
    qemu_vbds: (Vbd.id * (int * qemu_frontend)) list;
    qemu_vifs: (Vif.id * (int * qemu_frontend)) list;
    pci_msitranslate: bool;
    pci_power_mgmt: bool;
    pv_drivers_detected: bool;
  } [@@deriving rpc]

  type t = {
    persistent: persistent_t;
    non_persistent: non_persistent_t;
  } [@@deriving rpc]

  let default_persistent_t =
    { build_info = None
    ; ty = None
    ; last_start_time = 0.0
    ; nomigrate = false
    ; nested_virt = false
    }

  (* override rpc code generated for persistent_t *)
  let persistent_t_of_rpc rpc =
    Rpc.struct_extend rpc (rpc_of_persistent_t default_persistent_t)
    |> persistent_t_of_rpc
end

module DB = struct
  include TypedTable(struct
      include VmExtra
      let namespace = "extra"
      type key = string
      let key vm = [ vm ]
    end)
end

let internal_updates = Updates.empty scheduler

let safe_rm xs path =
  debug "xenstore-rm %s" path;
  try
    xs.Xs.rm path
  with _ -> ()

let this_domid ~xs =
  (* If we're in dom0 then no-one will have created the "domid" key. *)
  try
    int_of_string (xs.Xs.read "domid")
  with _ -> 0

let uuid_of_string x = match Uuidm.of_string x with
  | Some x -> x
  | None ->
    let msg = Printf.sprintf "string '%s' is not a valid UUID" x in
    error "%s" msg;
    failwith msg

let uuid_of_vm vm = uuid_of_string vm.Vm.id

(* During a live migrate, there will be multiple domains with the same uuid.
   The convention is: we construct things on the newest domain (e.g. VBD.plug)
   and we destroy things on the oldest domain (e.g. VBD.unplug). In the normal
   case there is only one domain, so oldest = newest *)

type domain_selection =
  | Oldest (* operate on the oldest domain *)
  | Newest (* operate on the newest domain *)
  | Expect_only_one

let di_of_uuid domain_selection uuid =
  let open Xenlight.Dominfo in
  let uuid' = Uuidm.to_string uuid in
  let all = with_ctx (fun ctx -> list ctx) in
  let possible = List.filter (fun x -> Xenctrl_uuid.uuid_of_handle x.uuid = uuid) all in

  let oldest_first = List.sort (fun a b -> compare a.domid b.domid) possible in
  let domid_list = String.concat ", " (List.map (fun x -> string_of_int x.domid) oldest_first) in

  if List.length oldest_first > 2 then
    warn "VM %s: there are %d domains (%s) with the same uuid: one or more have leaked"
      uuid' (List.length oldest_first) domid_list;

  if domain_selection = Expect_only_one && (List.length oldest_first > 1) then
    raise (Internal_error (Printf.sprintf "More than one domain with uuid (%s): %s" uuid' domid_list));

  match (if domain_selection = Oldest then oldest_first else List.rev oldest_first) with
  | [] -> None, false
  | x :: [] ->
    Some x, false
  | x :: rest ->
    debug "VM = %s; domids = [ %s ]; we will operate on %d" uuid' domid_list x.domid;
    Some x, true

let domid_of_uuid domain_selection uuid =
  (* We don't fully control the domain lifecycle because libxenguest will actually
     	   destroy a domain on suspend. Therefore we only rely on state in xenstore *)
  match di_of_uuid domain_selection uuid with
  | None, _ ->
    error "Failed to find VM %s: has this domain already been cleaned up?" (Uuidm.to_string uuid);
    None
  | Some x, _ ->
    Some x.Xenlight.Dominfo.domid

module Storage = struct
  open Storage
  open Storage_interface
  module Client = Storage_client.Client

  let id_of = id_of
  let epoch_begin = epoch_begin
  let epoch_end = epoch_end

  (* We need to deal with driver domains here: *)
  let attach_and_activate ~xs task vm dp sr vdi read_write =
    let result = attach_and_activate task vm dp sr vdi read_write in
    let backend = Xenops_task.with_subtask task (Printf.sprintf "Policy.get_backend_vm %s %s %s" vm sr vdi)
        (transform_exception (fun () -> Client.Policy.get_backend_vm "attach_and_activate" vm sr vdi)) in
    match domid_of_uuid Newest (uuid_of_string backend) with
    | None ->
      failwith (Printf.sprintf "Driver domain disapppeared: %s" backend)
    | Some domid ->
      { domid = domid; attach_info = result }

  let deactivate = deactivate
  let dp_destroy = dp_destroy
  let get_disk_by_name = get_disk_by_name
end

module Mem = struct
  let wrap f =
    try Some (f ())
    with
    | Memory_interface.MemoryError (Memory_interface.Cannot_free_this_much_memory(needed, free)) ->
      let needed = Memory.bytes_of_kib needed in
      let free = Memory.bytes_of_kib free in
      raise (Cannot_free_this_much_memory(needed, free))
    | Memory_interface.MemoryError (Memory_interface.Domains_refused_to_cooperate domids) ->
      debug "Got error_domains_refused_to_cooperate_code from ballooning daemon";
      raise (Vms_failed_to_cooperate (List.map get_uuid domids))
    | Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
      info "ECONNREFUSED talking to squeezed: assuming it has been switched off";
      None
    | Unix.Unix_error(Unix.ENOENT, "connect", _) ->
      info "ENOENT talking to squeezed: assuming it has never been started";
      None
  open Memory_client
  let do_login dbg = wrap (fun () -> Client.login dbg "xenopsd")

  (* Each "login" causes all unused reservations to be freed, therefore we log in once *)
  let cached_session_id = ref None
  let cached_session_id_m = Mutex.create ()
  let get_session_id =
    fun dbg ->
      Mutex.execute cached_session_id_m
        (fun () ->
           match !cached_session_id with
           | Some x -> x
           | None ->
             let s = do_login dbg in
             cached_session_id := Some s;
             s
        )

  (** If we fail to allocate because VMs either failed to co-operate or because they are still booting
      		and haven't written their feature-balloon flag then retry for a while before finally giving up.
      		In particular this should help smooth over the period when VMs are booting and haven't loaded their balloon
      		drivers yet. *)
  let retry f =
    let start = Unix.gettimeofday () in
    let interval = 10. in
    let timeout = 0. in
    let rec loop () =
      try
        f ()
      with
      | Memory_interface.MemoryError Memory_interface.Domains_refused_to_cooperate _
      | Memory_interface.MemoryError (Memory_interface.Cannot_free_this_much_memory(_, _)) as e ->
        let now = Unix.gettimeofday () in
        if now -. start > timeout then raise e else begin
          debug "Sleeping %.0f before retrying" interval;
          Thread.delay interval;
          loop ()
        end in
    loop ()

  (** Reserve a particular amount of memory and return a reservation id *)
  let reserve_memory_range_exn dbg min max =
    Opt.map
      (fun session_id ->
         let reservation_id, reserved_memory  =
           retry
             (fun () ->
                debug "Requesting a host memory reservation between %Ld and %Ld" min max;
                let reservation_id, kib = Client.reserve_memory_range dbg session_id min max in
                debug "Memory reservation size = %Ld (reservation_id = %s)" kib reservation_id;
                reservation_id, kib
             )
         in
         (* Post condition: *)
         assert (reserved_memory >= min);
         assert (reserved_memory <= max);
         reserved_memory, (reservation_id, reserved_memory)
      ) (get_session_id dbg)

  let reserve_memory_range dbg min max : (int64 * (string * int64)) option =
    wrap (fun () -> reserve_memory_range_exn dbg min max) |> Opt.join

  (** Delete a reservation given by [reservation_id] *)
  let delete_reservation_exn dbg (reservation_id, _) =
    Opt.map
      (fun session_id ->
         debug "delete_reservation %s" reservation_id;
         Client.delete_reservation dbg session_id reservation_id
      ) (get_session_id dbg)
  let delete_reservation dbg r =
    let (_: unit option option) = wrap (fun () -> delete_reservation_exn dbg r) in
    ()

  (** Reserves memory, passes the id to [f] and cleans up afterwards. If the user
      		wants to keep the memory, then call [transfer_reservation_to_domain]. *)
  let with_reservation dbg min max f =
    let amount, id = Opt.default (min, ("none", min)) (reserve_memory_range dbg min max) in
    finally
      (fun () -> f amount id)
      (fun () -> delete_reservation dbg id)

  (** Transfer this 'reservation' to the given domain id *)
  let transfer_reservation_to_domain_exn dbg domid (reservation_id, amount) =
    match get_session_id dbg with
    | Some session_id ->
      begin
        try
          Client.transfer_reservation_to_domain dbg session_id reservation_id domid
        with Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
          (* This happens when someone manually runs 'service squeezed stop' *)
          Mutex.execute cached_session_id_m (fun () -> cached_session_id := None);
          error "Ballooning daemon has disappeared. Manually setting domain maxmem for domid = %d to %Ld KiB" domid amount;
          (* TODO: replace domain_setmaxmem with libxl_domain_setmaxmem (not yet written!) *)
          Xenctrl.with_intf (fun xc -> Xenctrl.domain_setmaxmem xc domid amount);
      end
    | None ->
      info "No ballooning daemon. Manually setting domain maxmem for domid = %d to %Ld KiB" domid amount;
      Xenctrl.with_intf (fun xc -> Xenctrl.domain_setmaxmem xc domid amount)

  let transfer_reservation_to_domain dbg domid r =
    let (_: unit option) = wrap (fun () -> transfer_reservation_to_domain_exn dbg domid r) in
    ()

  (** After an event which frees memory (eg a domain destruction), perform a one-off memory rebalance *)
  let balance_memory dbg =
    debug "rebalance_memory";
    Client.balance_memory dbg

end

(* We store away the device name so we can lookup devices by name later *)
let _device_id kind = Device_common.string_of_kind kind ^ "-id"

(* Return the xenstore device with [kind] corresponding to [id] *)
let device_by_id xs vm kind domain_selection id =
  match vm |> uuid_of_string |> domid_of_uuid domain_selection with
  | None ->
    debug "VM = %s; does not exist in domain list" vm;
    raise (Does_not_exist("domain", vm))
  | Some frontend_domid ->
    let open Device_common in
    let devices = list_frontends ~xs frontend_domid in

    let key = _device_id kind in
    let id_of_device device =
      let path = Device_common.get_private_data_path_of_device_by_uuid
          (vm |> uuid_of_string) (string_of_kind device.backend.kind) device.frontend.devid in
      try Some (xs.Xs.read (Printf.sprintf "%s/%s" path key))
      with _ -> None in
    let ids = List.map id_of_device devices in
    try
      List.assoc (Some id) (List.combine ids devices)
    with Not_found ->
      debug "VM = %s; domid = %d; Device is not active: kind = %s; id = %s; active devices = [ %s ]" vm frontend_domid (Device_common.string_of_kind kind) id (String.concat ", " (List.map (Opt.default "None") ids));
      raise (Device_not_connected)

(* Extra keys to store in VBD backends to allow us to deactivate VDIs: *)
type backend = disk option [@@deriving rpc]
let _vdi_id = "vdi-id"
let _dp_id = "dp-id"

let set_stubdom ~xs domid domid' =
  xs.Xs.write (Printf.sprintf "/local/domain/%d/stub-domid" domid) (string_of_int domid')

let get_stubdom ~xs domid =
  try Some (int_of_string (xs.Xs.read (Printf.sprintf "/local/domain/%d/stub-domid" domid))) with _ -> None

module HOST = struct
  include Xenops_server_skeleton.HOST

  let get_console_data () =
    with_ctx (fun ctx ->
        debug "Calling Xenlight.Host.xen_console_read_start";
        let reader = Xenlight.Host.xen_console_read_start ctx 0 in
        let rec read_lines () =
          try
            debug "Calling Xenlight.Host.xen_console_read_line";
            let line = Xenlight.Host.xen_console_read_line ctx reader in
            line :: read_lines ()
          with Xenlight.Host.End_of_file ->
            []
        in
        let raw = String.concat "" (List.rev (read_lines ())) in
        Xenlight.Host.xen_console_read_finish ctx reader;
        (* There may be invalid XML characters in the buffer, so remove them *)
        let is_printable chr =
          let x = int_of_char chr in
          x=13 || x=10 || (x >= 0x20 && x <= 0x7e) in
        for i = 0 to String.length raw - 1 do
          if not(is_printable raw.[i])
          then raw.[i] <- ' '
        done;
        raw
      )

  let get_total_memory_mib () =
    with_ctx (fun ctx ->
        let pages_per_mib = 256L in
        debug "Calling Xenlight.Physinfo_total_pages";
        let pages = (Xenlight.Physinfo.get ctx).Xenlight.Physinfo.total_pages in
        Int64.div pages pages_per_mib
      )
  let send_debug_keys keys =
    with_ctx (fun ctx ->
        debug "Calling Xenlight.Host.send_debug_keys";
        Xenlight.Host.send_debug_keys ctx keys
      )
end


(* TODO: libxl *)
let on_frontend f domain_selection frontend =
  with_xc_and_xs
    (fun xc xs ->
       let frontend_di = match frontend |> uuid_of_string |> di_of_uuid domain_selection with
         | None, _ -> raise (Does_not_exist ("domain", frontend))
         | Some x, _ -> x in
       let open Xenlight.Dominfo in
       let hvm = frontend_di.domain_type = Xenlight.DOMAIN_TYPE_HVM in
       f xc xs frontend_di.domid hvm
    )

module PCI = struct
  open Pci

  let id_of pci = snd pci.id

  let get_state vm pci =
    let domid =
      with_xs (fun xs ->
          domid_of_uuid Newest (uuid_of_string vm)
        )
    in
    let open Xenlight in
    with_ctx (fun ctx ->
        let all = match domid with
          | Some domid ->
            Device_pci.list ctx domid
          | None -> [] in
        {
          plugged = List.filter (function {Device_pci.domain; Device_pci.bus; Device_pci.dev; Device_pci.func} ->
              domain = pci.address.domain
              && bus = pci.address.bus
              && dev = pci.address.dev
              && func = pci.address.fn) all <> []
        }
      )

  let get_device_action_request vm pci =
    let state = get_state vm pci in
    (* If it has disappeared from xenstore then we assume unplug is needed if only
       		   to release resources/ deassign devices *)
    if not state.plugged then Some Needs_unplug else None

  let pre_plug vm pci =
    debug "PCI.pre_plug";
    let vm_t = DB.read_exn vm in
    let non_persistent = vm_t.VmExtra.non_persistent in
    let func = pci.address.fn in
    let dev = pci.address.dev in
    let bus = pci.address.bus in
    let domain = pci.address.domain in
    let msitranslate = if Opt.default non_persistent.VmExtra.pci_msitranslate pci.msitranslate then true else false in
    let power_mgmt = if Opt.default non_persistent.VmExtra.pci_power_mgmt pci.power_mgmt then true else false in
    let open Xenlight.Device_pci in
    let pci' = { (with_ctx (fun ctx -> default ctx ())) with
                 func; dev; bus; domain; msitranslate; power_mgmt;
               } in
    pci'

  let plug task vm pci =
    on_frontend (fun _ _ frontend_domid _ ->
        with_ctx (fun ctx ->
            let open Xenlight.Device_pci in
            let pci' = pre_plug vm pci in
            debug "Calling Xenlight.Device_pci.assignable_remove";
            assignable_add ctx pci' true;
            debug "Calling Xenlight.Device_pci.add";
            Xenlight_events.async (add ctx pci' frontend_domid);
            debug "Call Xenlight.Device_pci.add completed";
          )
      ) Newest vm

  let unplug task vm pci =
    let vm_t = DB.read_exn vm in
    let non_persistent = vm_t.VmExtra.non_persistent in
    let func = pci.address.fn in
    let dev = pci.address.dev in
    let bus = pci.address.bus in
    let domain = pci.address.domain in
    let msitranslate = if Opt.default non_persistent.VmExtra.pci_msitranslate pci.msitranslate then true else false in
    let power_mgmt = if Opt.default non_persistent.VmExtra.pci_power_mgmt pci.power_mgmt then true else false in
    on_frontend (fun _ _ frontend_domid _ ->
        with_ctx (fun ctx ->
            let open Xenlight.Device_pci in
            let pci' = {(default ctx ()) with
                        func; dev; bus; domain; msitranslate; power_mgmt
                       } in
            debug "Calling Xenlight.Device_pci.destroy";
            Xenlight_events.async (destroy ctx pci' frontend_domid);
            debug "Call Xenlight.Device_pci.destroy completed";
            debug "Calling Xenlight.Device_pci.assignable_remove";
            assignable_remove ctx pci' true;
          )
      ) Oldest vm
end

module VGPU = struct
  include Xenops_server_skeleton.VGPU
end

module VUSB = struct
  let plug _ vm vusb = ()
  let unplug _ vm vusb = ()
  let get_state vm vusb = failwith "Not implemented"
  let get_device_action_request vm vusb = failwith "Not implemented"
end

let set_active_device path active =
  with_xs
    (fun xs ->
       if active
       then xs.Xs.write path "1"
       else safe_rm xs path;
    )

module VBD = struct
  open Vbd

  let id_of vbd = snd vbd.id

  (* When we attach a VDI we remember the attach result so we can lookup
     	   details such as the device-kind later. *)

  let vdi_attach_path vbd = Printf.sprintf "/xapi/%s/private/vdis/%s" (fst vbd.id) (snd vbd.id)

  let attach_and_activate task xs vm vbd vdi =
    let attached_vdi = match vdi with
      | None ->
        (* XXX: do something better with CDROMs *)
        { domid = this_domid ~xs; attach_info = { Storage_interface.params=""; o_direct=true; o_direct_reason = ""; xenstore_data=[]; } }
      | Some (Local path) ->
        { domid = this_domid ~xs; attach_info = { Storage_interface.params=path; o_direct=true; o_direct_reason = ""; xenstore_data=[]; } }
      | Some (VDI path) ->
        let sr, vdi = Storage.get_disk_by_name task path in
        let dp = Storage.id_of vm vbd.id in
        let vm = fst vbd.id in
        Storage.attach_and_activate ~xs task vm dp sr vdi (vbd.mode = ReadWrite) in
    xs.Xs.write (vdi_attach_path vbd) (attached_vdi |> rpc_of_attached_vdi |> Jsonrpc.to_string);
    attached_vdi

  let frontend_domid_of_device device = device.Device_common.frontend.Device_common.domid

  let device_number_of_device d =
    Device_number.of_xenstore_key d.Device_common.frontend.Device_common.devid

  let active_path vm vbd = Printf.sprintf "/vm/%s/devices/vbd/%s" vm (snd vbd.Vbd.id)

  let set_active task vm vbd active =
    try
      set_active_device (active_path vm vbd) active
    with e ->
      debug "set_active %s.%s <- %b failed: %s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) active (Printexc.to_string e)

  let get_active vm vbd =
    try
      with_xs (fun xs -> xs.Xs.read (active_path vm vbd)) = "1"
    with _ -> false

  let epoch_begin task vm disk persistent = match disk with
    | VDI path ->
      let sr, vdi = Storage.get_disk_by_name task path in
      Storage.epoch_begin task sr vdi persistent
    | _ -> ()

  let epoch_end task vm disk = match disk with
    | VDI path ->
      let sr, vdi = Storage.get_disk_by_name task path in
      Storage.epoch_end task sr vdi
    | _ -> ()

  let _backend_kind = "backend-kind"

  let device_kind_of ~xs vbd =
    (* If the user has provided an override then use that *)
    if List.mem_assoc _backend_kind vbd.extra_backend_keys
    then Device_common.kind_of_string (List.assoc _backend_kind vbd.extra_backend_keys)
    else match (try Some(xs.Xs.read (vdi_attach_path vbd) |> Jsonrpc.of_string |> attached_vdi_of_rpc) with _ -> None) with
      | None ->
        (* An empty VBD has to be a CDROM: anything will do *)
        Device_common.Vbd !Xenopsd.default_vbd_backend_kind
      | Some vdi ->
        let xenstore_data = vdi.attach_info.Storage_interface.xenstore_data in
        (* Use the storage manager's preference *)
        if List.mem_assoc _backend_kind xenstore_data
        then Device_common.kind_of_string (List.assoc _backend_kind xenstore_data)
        else Device_common.Vbd !Xenopsd.default_vbd_backend_kind

  let vdi_path_of_device ~xs device = Device_common.backend_path_of_device ~xs device ^ "/vdi"

  let write_extra backend_domid frontend_domid devid kv_list =
    with_xs (fun xs ->
        let path = Printf.sprintf "/local/domain/%d/backend/vbd/%d/%d" backend_domid frontend_domid devid in
        xs.Xs.writev path kv_list;
      )

  let write_private backend_domid vm devid private_list =
    with_xs (fun xs ->
        let uuid = uuid_of_string vm in
        let private_data_path = Device_common.get_private_data_path_of_device_by_uuid uuid "vbd" devid in
        Xs.transaction xs (fun t ->
            t.Xst.mkdir private_data_path;
            t.Xst.setperms private_data_path
              Xs_protocol.ACL.({owner = backend_domid; other = NONE; acl = []});
            t.Xst.writev private_data_path
              (("backend-kind", "vbd") :: ("backend-id", string_of_int backend_domid) :: private_list);
          )
      )

  let free_device ~xs bus_type domid =
    let open Device_common in
    let disks = List.map
        (fun x -> x.frontend.devid
                  |> Device_number.of_xenstore_key
                  |> Device_number.spec
                  |> (fun (_, disk, _) -> disk))
        (Device_common.list_frontends ~xs domid) in
    let next = List.fold_left max 0 disks + 1 in
    bus_type, next, 0

  let devid_and_vdev_of_vbd vm vbd =
    let open Device_number in
    (* If no device number is provided then autodetect a free one *)
    let device_number =
      match vbd.position with
      | Some x ->
        (* If the 'position' is on the Ide bus, we "upgrade" to
           				   to the Xen bus instead *)
        make (match spec x with
            | Ide, disk, partition -> Xen, disk, partition
            | x -> x)
      | None ->
        on_frontend (fun _ xs domid hvm ->
            make (free_device ~xs Xen domid)
          ) Newest vm
    in
    let devid = to_xenstore_key device_number in
    let vdev = to_linux_device device_number in
    devid, vdev

  let format_of_string x = match String.lowercase_ascii x with
    | "qcow2" -> Xenlight.DISK_FORMAT_QCOW2
    | "raw" -> Xenlight.DISK_FORMAT_RAW
    | "vhd" -> Xenlight.DISK_FORMAT_VHD
    | _ -> Xenlight.DISK_FORMAT_UNKNOWN

  let can_surprise_remove ~xs (x: Device_common.device) =
    (* "(info key in xenstore) && 2" tells us whether a vbd can be surprised removed *)
    let key = Device_common.backend_path_of_device ~xs x ^ "/info" in
    try
      let info = Int64.of_string (xs.Xs.read key) in
      (Int64.logand info 2L) <> 0L
    with _ -> false

  let create_vbd_frontend ~xs task frontend_domid vdi =
    let frontend_vm_id = get_uuid frontend_domid in
    let backend_vm_id = get_uuid vdi.domid in
    match domid_of_uuid Expect_only_one (uuid_of_string backend_vm_id) with
    | None ->
      error "VM = %s; domid = %d; Failed to determine domid of backend VM id: %s" frontend_vm_id frontend_domid backend_vm_id;
      raise (Does_not_exist("domain", backend_vm_id))
    | Some backend_domid when backend_domid = frontend_domid && vdi.attach_info.Storage_interface.xenstore_data = [] -> (* FIXME *)
      (* There's no need to use a PV disk if we're in the same domain *)
      Name vdi.attach_info.Storage_interface.params
    | Some backend_domid ->
      let device_number = Device_number.make (free_device ~xs Device_number.Xen frontend_domid) in
      let devid = Device_number.to_xenstore_key device_number in
      let vdev = Device_number.to_linux_device device_number in
      let extra_backend_keys = List.map (fun (k, v) -> "sm-data/" ^ k, v) (vdi.attach_info.Storage_interface.xenstore_data) in
      with_ctx (fun ctx ->
          let open Xenlight.Device_disk in
          let disk = {(default ctx ()) with
                      backend_domid;
                      pdev_path = Some (vdi.attach_info.Storage_interface.params);
                      vdev = Some vdev;
                      backend = Xenlight.DISK_BACKEND_PHY;
                      format = Xenlight.DISK_FORMAT_RAW;
                      script = Some !Xl_resources.vbd_script;
                      removable = 1;
                      readwrite = 1;
                      is_cdrom = 0;
                     } in
          debug "Calling Xenlight.Device_disk.add";
          Xenlight_events.async (add ctx disk frontend_domid);
          debug "Call Xenlight.Device_disk.add completed"
        );
      (* write extra XS keys *)
      write_extra backend_domid frontend_domid devid extra_backend_keys;

      (* wait for plug *)
      let device =
        let open Device_common in
        let frontend = { domid = frontend_domid; kind = Vbd !Xenopsd.default_vbd_backend_kind; devid = devid } in
        let backend = { domid = backend_domid; kind = Vbd !Xenopsd.default_vbd_backend_kind; devid = devid } in
        { backend = backend; frontend = frontend }
      in
      with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);

      (* 'Normally' we connect devices to other domains, and cannot know whether the
         				   device is 'available' from their userspace (or even if they have a userspace).
         				   The best we can do is just to wait for the backend hotplug scripts to run,
         				   indicating that the backend has locked the resource.
         				   In the case of domain 0 we can do better: we have custom hotplug scripts
         				   which call us back when the device is actually available to userspace. We need
         				   to wait for this condition to make the template installers work.
         				   NB if the custom hotplug script fires this implies that the xenbus state
         				   reached "connected", so we don't have to check for that first. *)
      if frontend_domid = 0 then begin
        let path = "/dev/" ^ vdev in
        debug "Waiting for %s to appear" path;
        let start = Unix.gettimeofday () in
        while not (Sys.file_exists path) && (Unix.gettimeofday () -. start < 30.) do
          Thread.delay 0.2
        done
      end;

      Device device

  let block_device_of_vbd_frontend = function
    | Name x -> x
    | Device device ->
      let open Device_common in
      device.frontend.devid |> Device_number.of_xenstore_key |> Device_number.to_linux_device |> (fun x -> "/dev/" ^ x)

  let destroy_vbd_frontend ~xs task disk =
    match disk with
    | Name _ -> ()
    | Device device ->
      with_ctx (fun ctx ->
          let open Device_common in
          let vdev = device.frontend.devid |> Device_number.of_xenstore_key |> Device_number.to_linux_device in
          let domid = device.frontend.domid in
          let disk = Xenlight.Device_disk.of_vdev ctx domid vdev in
          Xenops_task.with_subtask task (Printf.sprintf "Vbd.clean_shutdown") (fun () ->
              debug "Calling Xenlight.Device_disk.remove";
              Xenlight_events.async (Xenlight.Device_disk.remove ctx disk domid);
              debug "Call Xenlight.Device_disk.remove completed"
            )
        )

  let ionice qos pid =
    try
      run !(Xl_resources.ionice) (Ionice.set_args qos pid) |> ignore_string
    with e ->
      error "Ionice failed on pid %d: %s" pid (Printexc.to_string e)

  let set_qos task vm vbd =
    with_xc_and_xs
      (fun xc xs ->
         Opt.iter (function
             | Ionice qos ->
               try
                 let (device: Device_common.device) = device_by_id xs vm (device_kind_of ~xs vbd) Newest (id_of vbd) in
                 let path = Device_common.kthread_pid_path_of_device ~xs device in
                 let kthread_pid = xs.Xs.read path |> int_of_string in
                 ionice qos kthread_pid
               with
               | Xs_protocol.Enoent _ ->
                 (* This means the kthread-pid hasn't been written yet. We'll be called back later. *)
                 ()
               | e ->
                 error "Failed to ionice kthread-pid: %s" (Printexc.to_string e)
           ) vbd.Vbd.qos
      )

  let get_qos xc xs vm vbd device =
    try
      let path = Device_common.kthread_pid_path_of_device ~xs device in
      let kthread_pid = xs.Xs.read path |> int_of_string in
      let i = run !(Xl_resources.ionice) (Ionice.get_args kthread_pid) |> Ionice.parse_result_exn in
      Opt.map (fun i -> Ionice i) i
    with
    | Ionice.Parse_failed x ->
      warn "Failed to parse ionice result: %s" x;
      None
    | _ ->
      None

  let string_of_qos = function
    | None -> "None"
    | Some x -> x |> Vbd.rpc_of_qos |> Jsonrpc.to_string

  let media_is_ejected ~xs ~device_number domid =
    let devid = Device_number.to_xenstore_key device_number in
    let back_dom_path = xs.Xs.getdomainpath 0 in
    let backend = Printf.sprintf "%s/backend/vbd/%u/%d" back_dom_path domid devid in
    let path = backend ^ "/params" in
    try xs.Xs.read path = "" with _ -> true

  let get_state vm vbd =
    with_xc_and_xs
      (fun xc xs ->
         try
           let (device: Device_common.device) = device_by_id xs vm (device_kind_of ~xs vbd) Newest (id_of vbd) in
           let qos_target = get_qos xc xs vm vbd device in

           let device_number = device_number_of_device device in
           let domid = device.Device_common.frontend.Device_common.domid in
           let backend_present =
             if media_is_ejected ~xs ~device_number domid
             then None
             else Some (vdi_path_of_device ~xs device |> xs.Xs.read |> Jsonrpc.of_string |> disk_of_rpc) in
           {
             Vbd.active = true;
             plugged = true;
             backend_present;
             qos_target = qos_target
           }
         with
         | (Does_not_exist(_, _))
         | Device_not_connected ->
           { unplugged_vbd with
             Vbd.active = get_active vm vbd
           }
      )

  let pre_plug task vm hvm vbd =
    debug "VBD.pre_plug";
    let vdi = with_xs (fun xs -> attach_and_activate task xs vm vbd vbd.backend) in
    (* override the backend-keys here *)
    let extra_backend_keys = List.fold_left (fun acc (k,v) ->
        let k = "sm-data/" ^ k in
        (k,v)::(List.remove_assoc k acc)) vbd.extra_backend_keys vdi.attach_info.Storage_interface.xenstore_data in

    let backend_domid = vdi.domid in
    let pdev_path = if vbd.backend = None then Some "" else Some (vdi.attach_info.Storage_interface.params) in
    let devid, vdev = devid_and_vdev_of_vbd vm vbd in

    let backend, format, script =
      if vbd.backend = None then
        (* empty CDROM *)
        Xenlight.DISK_BACKEND_PHY, Xenlight.DISK_FORMAT_EMPTY, Some !Xl_resources.vbd_script
      else
        let xd = vdi.attach_info.Storage_interface.xenstore_data in
        if xd = [] || not(List.mem_assoc "backend-kind" xd)
        then Xenlight.DISK_BACKEND_PHY, Xenlight.DISK_FORMAT_RAW, Some !Xl_resources.vbd_script
        else begin
          match List.assoc "backend-kind" xd with
          | "qdisk" ->
            let format =
              if List.mem_assoc "format" xd
              then format_of_string (List.assoc "format" xd)
              else Xenlight.DISK_FORMAT_QCOW2 in (* FIXME *)
            Xenlight.DISK_BACKEND_QDISK, format, None
          | "vbd" ->
            Xenlight.DISK_BACKEND_PHY, Xenlight.DISK_FORMAT_RAW, Some !Xl_resources.vbd_script
          | x ->
            failwith (Printf.sprintf "libxl doesn't support backend-kind=%s" x)
        end
    in

    let removable = if vbd.unpluggable then 1 else 0 in
    let readwrite = match vbd.mode with
      | ReadOnly -> 0
      | ReadWrite -> 1
    in
    let is_cdrom = match vbd.ty with
      | CDROM -> 1
      | Disk -> 0
      | Floppy -> 0
    in

    (* Remember the VBD id with the device *)
    let vbd_id = with_xs (fun xs -> _device_id (device_kind_of ~xs vbd), id_of vbd) in
    (* Remember the VDI with the device (for later deactivation) *)
    let vdi_id = _vdi_id, vbd.backend |> rpc_of_backend |> Jsonrpc.to_string in
    let dp_id = _dp_id, Storage.id_of vm vbd.Vbd.id in
    let no_phys_device = if vbd.ty = CDROM && hvm then ["no-physical-device", ""] else [] in
    let extra_private_keys = dp_id :: vdi_id :: vbd_id :: no_phys_device @ vbd.extra_private_keys in

    (* write private XS keys *)
    write_private backend_domid vm devid extra_private_keys;

    (* call libxenlight to plug vbd *)
    let open Xenlight.Device_disk in
    let disk = with_ctx (fun ctx ->
        {(default ctx ()) with
         backend_domid; pdev_path; vdev = Some vdev; backend; format; script; removable;
         readwrite; is_cdrom;
        }) in
    disk, devid, extra_backend_keys, backend_domid

  let plug task vm vbd =
    (* If the vbd isn't listed as "active" then we don't automatically plug this one in *)
    if not(get_active vm vbd)
    then debug "VBD %s.%s is not active: not plugging into VM" (fst vbd.Vbd.id) (snd vbd.Vbd.id)
    else on_frontend
        (fun _ xs frontend_domid hvm ->
           let state = get_state vm vbd in
           if state.plugged then
             info "VBD %s.%s is already plugged" (fst vbd.Vbd.id) (snd vbd.Vbd.id)
           else if vbd.backend = None && not hvm then
             info "VM = %s; an empty CDROM drive on a PV guest is simulated by unplugging the whole drive" vm
           else begin
             Xenops_task.with_subtask task (Printf.sprintf "Vbd.add %s" (id_of vbd))
               (fun () ->
                  let disk, devid, extra_backend_keys, backend_domid = pre_plug task vm hvm vbd in

                  (* call libxenlight to plug vbd *)
                  with_ctx (fun ctx ->
                      let open Xenlight.Device_disk in
                      debug "Calling Xenlight.Device_disk.add";
                      Xenlight_events.async (add ctx disk frontend_domid);
                      debug "Call Xenlight.Device_disk.add completed"
                    );
                  (* write extra XS keys *)
                  write_extra backend_domid frontend_domid devid extra_backend_keys;

                  (* wait for plug *)
                  let device =
                    let open Device_common in
                    let kind = device_kind_of ~xs vbd in
                    let frontend = { domid = frontend_domid; kind; devid } in
                    let backend = { domid = backend_domid; kind; devid } in
                    { backend = backend; frontend = frontend }
                  in
                  with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);

                  (* We store away the disk so we can implement VBD.stat *)
                  Opt.iter (fun disk -> xs.Xs.write (vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string)) vbd.backend;
               );
           end
        ) Newest vm

  let unplug task vm vbd force =
    let vm_t = DB.read vm in
    with_xs
      (fun xs ->
         try
           (* On destroying the datapath:
              					   1. if the device has already been shutdown and deactivated (as in suspend) we
              					      must call DP.destroy here to avoid leaks
              					   2. if the device is successfully shutdown here then we must call DP.destroy
              					      because no-one else will
              					   3. if the device shutdown is rejected then we should leave the DP alone and
              					      rely on the event thread calling us again later.
              					*)
           let domid = domid_of_uuid Oldest (uuid_of_string vm) in
           let kind = device_kind_of ~xs vbd in
           (* If the device is gone then we don't need to shut it down but we do need
              					   to free any storage resources. *)
           let device =
             try
               Some (device_by_id xs vm kind Oldest (id_of vbd))
             with
             | (Does_not_exist(_,_)) ->
               debug "VM = %s; VBD = %s; Ignoring missing domain" vm (id_of vbd);
               None
             | Device_not_connected ->
               debug "VM = %s; VBD = %s; Ignoring missing device" vm (id_of vbd);
               None in
           with_ctx (fun ctx ->
               let vdev = Opt.map Device_number.to_linux_device vbd.position in
               match vdev, domid, device with
               | Some vdev, Some domid, Some device ->
                 if force && (not (can_surprise_remove ~xs device)) then
                   debug "VM = %s; VBD = %s; Device is not surprise-removable" vm (id_of vbd); (* happens on normal shutdown too *)
                 let disk = Xenlight.Device_disk.of_vdev ctx domid vdev in
                 Xenops_task.with_subtask task (Printf.sprintf "Vbd.clean_shutdown %s" (id_of vbd)) (fun () ->
                     if force then begin
                       debug "Calling Xenlight.Device_disk.destroy";
                       Xenlight_events.async (Xenlight.Device_disk.destroy ctx disk domid);
                       debug "Call Xenlight.Device_disk.destroy completed"
                     end else begin
                       debug "Calling Xenlight.Device_disk.remove";
                       Xenlight_events.async (Xenlight.Device_disk.remove ctx disk domid);
                       debug "Call Xenlight.Device_disk.remove completed"
                     end
                   )
               | _ -> ()
             );

           (* We now have a shutdown device but an active DP: we should unconditionally destroy the DP *)
           finally
             (fun () ->
                (* If we have a qemu frontend, detach this too. *)
                Opt.iter (fun vm_t ->
                    let non_persistent = vm_t.VmExtra.non_persistent in
                    if List.mem_assoc vbd.Vbd.id non_persistent.VmExtra.qemu_vbds then begin
                      let _, qemu_vbd = List.assoc vbd.Vbd.id non_persistent.VmExtra.qemu_vbds in
                      (* destroy_vbd_frontend ignores 'refusing to close' transients' *)
                      destroy_vbd_frontend ~xs task qemu_vbd;
                      let non_persistent = { non_persistent with
                                             VmExtra.qemu_vbds = List.remove_assoc vbd.Vbd.id non_persistent.VmExtra.qemu_vbds } in
                      DB.write vm { vm_t with VmExtra.non_persistent = non_persistent }
                    end) vm_t
             )
             (fun () ->
                Opt.iter (fun domid ->
                    Storage.dp_destroy task (Storage.id_of vm vbd.Vbd.id)
                  ) domid
             )
         with
         | Device_common.Device_error(_, s) ->
           debug "Caught Device_error: %s" s;
           raise (Device_detach_rejected("VBD", id_of vbd, s))
      )

  let insert task vm vbd disk =
    on_frontend
      (fun _ xs frontend_domid hvm ->
         if not hvm
         then plug task vm { vbd with backend = Some disk }
         else begin
           with_ctx (fun ctx ->
               let vdev = Opt.map Device_number.to_linux_device vbd.position in
               let domid = domid_of_uuid Newest (uuid_of_string vm) in
               let kind = device_kind_of ~xs vbd in
               match vdev, domid with
               | Some vdev, Some domid ->
                 let open Xenlight.Device_disk in
                 let vdi = attach_and_activate task xs vm vbd (Some disk) in
                 let disk' = {(of_vdev ctx domid vdev) with
                              pdev_path = Some vdi.attach_info.Storage_interface.params;
                              format = Xenlight.DISK_FORMAT_RAW;
                             } in

                 (* We store away the disk so we can implement VBD.stat *)
                 let (device: Device_common.device) = device_by_id xs vm kind Newest (id_of vbd) in
                 xs.Xs.write (vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string);

                 Xenops_task.with_subtask task (Printf.sprintf "Vbd.insert %s" (id_of vbd)) (fun () ->
                     debug "Calling Xenlight.Device_disk.insert";
                     insert ctx disk' domid ()
                   );

                 (* Workaround the fact that libxl prepends "aio:" at the params field, before the pdev_path *)
                 (* QEMU does not seem to recognise this, and does not connect the CD *)
                 let device_number = device_number_of_device device in
                 let devid = Device_number.to_xenstore_key device_number in
                 let path = Printf.sprintf "/local/domain/%d/backend/vbd/%d/%d/params" 0 frontend_domid devid in
                 xs.Xs.write path vdi.attach_info.Storage_interface.params
               | _ -> ()
             )
         end
      ) Newest vm

  let eject task vm vbd =
    on_frontend
      (fun _ xs frontend_domid hvm ->
         with_ctx (fun ctx ->
             let vdev = Opt.map Device_number.to_linux_device vbd.position in
             let domid = domid_of_uuid Newest (uuid_of_string vm) in
             let kind = device_kind_of ~xs vbd in
             match vdev, domid with
             | Some vdev, Some domid ->
               let open Xenlight.Device_disk in
               let disk' = {(of_vdev ctx domid vdev) with
                            format = Xenlight.DISK_FORMAT_EMPTY;
                            script = Some !Xl_resources.vbd_script;
                           } in

               Xenops_task.with_subtask task (Printf.sprintf "Vbd.eject %s" (id_of vbd)) (fun () ->
                   debug "Calling Xenlight.Device_disk.insert";
                   insert ctx disk' domid ()
                 );

               let (device: Device_common.device) = device_by_id xs vm kind Oldest (id_of vbd) in
               safe_rm xs (vdi_path_of_device ~xs device);
               Storage.dp_destroy task (Storage.id_of vm vbd.Vbd.id)
             | _ -> ()
           )
      ) Oldest vm

  let get_device_action_request vm vbd =
    with_xc_and_xs
      (fun xc xs ->
         try
           let kind = device_kind_of ~xs vbd in
           let (device: Device_common.device) = device_by_id xs vm kind Newest (id_of vbd) in
           if Hotplug.device_is_online ~xs vm device
           then begin
             let qos_target = get_qos xc xs vm vbd device in
             if qos_target <> vbd.Vbd.qos then begin
               debug "VM = %s; VBD = %s; VBD_set_qos needed, current = %s; target = %s" vm (id_of vbd) (string_of_qos qos_target) (string_of_qos vbd.Vbd.qos);
               Some Needs_set_qos
             end else None
           end else begin
             debug "VM = %s; VBD = %s; VBD_unplug needed, device offline: %s" vm (id_of vbd) (Device_common.string_of_device device);
             Some Needs_unplug
           end
         with Device_not_connected ->
           debug "VM = %s; VBD = %s; Device_not_connected so no action required" vm (id_of vbd);
           None
      )
end

module VIF = struct
  include Xenops_server_skeleton.VIF

  open Vif

  let id_of vif = snd vif.id

  let backend_domid_of xs vif =
    match vif.backend with
    | Network.Local _ -> this_domid ~xs
    | Network.Remote (vm, _) ->
      begin match vm |> uuid_of_string |> domid_of_uuid Expect_only_one with
        | None -> raise (Does_not_exist ("domain", vm))
        | Some x -> x
      end

  let bridge_of_vif = function
    | Network.Local b -> b
    | Network.Remote (_, b) -> b

  let _locking_mode = "locking-mode"
  let _ipv4_allowed = "ipv4-allowed"
  let _ipv6_allowed = "ipv6-allowed"
  let _static_ip_setting = "static-ip-setting"

  let locking_mode_keys = [
    _locking_mode;
    _ipv4_allowed;
    _ipv6_allowed;
  ]

  let xenstore_of_locking_mode = function
    | Locked { ipv4 = ipv4; ipv6 = ipv6 } -> [
        _locking_mode, "locked";
        _ipv4_allowed, String.concat "," ipv4;
        _ipv6_allowed, String.concat "," ipv6;
      ]
    | Unlocked -> [
        _locking_mode, "unlocked";
      ]
    | Disabled -> [
        _locking_mode, "disabled";
      ]

  let xenstore_of_static_ip_setting vif =
    let constant_setting = [
      "mac", vif.mac;
      "error-code", "0";
      "error-msg", "";
    ] in
    let ipv4_setting = match vif.ipv4_configuration with
      | Unspecified4 -> [ "enabled", "0" ]
      | Static4 (address :: _, gateway) ->
        let enabled = "enabled" , "1" in
        let address = "address", address in
        let gateway = match gateway with Some value -> ["gateway", value] | None -> [] in
        enabled :: address :: gateway
      | Static4 ([], _) -> raise (Internal_error "Static IPv4 configuration selected, but no address specified.")
    in
    let ipv6_setting = match vif.ipv6_configuration with
      | Unspecified6 -> [ "enabled6", "0" ]
      | Static6 (address6 :: _, gateway6) ->
        let enabled6 = "enabled6" , "1" in
        let address6 = "address6", address6 in
        let gateway6 = match gateway6 with Some value -> ["gateway6", value] | None -> [] in
        enabled6 :: address6 :: gateway6
      | Static6 ([], _) -> raise (Internal_error "Static IPv6 configuration selected, but no address specified.")
    in
    let settings = constant_setting @ ipv4_setting @ ipv6_setting in
    List.map (fun (k,v) -> Printf.sprintf "%s/%s" _static_ip_setting k, v) settings

  let disconnect_flag device mode =
    let path = Hotplug.vif_disconnect_path device in
    let flag = match mode with Xenops_interface.Vif.Disabled -> "1" | _ -> "0" in
    path, flag

  let active_path vm vif = Printf.sprintf "/vm/%s/devices/vif/%s" vm (snd vif.Vif.id)

  let set_active task vm vif active =
    try
      set_active_device (active_path vm vif) active
    with e ->
      debug "set_active %s.%s <- %b failed: %s" (fst vif.Vif.id) (snd vif.Vif.id) active (Printexc.to_string e)

  let get_active vm vif =
    try
      with_xs (fun xs -> xs.Xs.read (active_path vm vif)) = "1"
    with _ -> false

  let write_private backend_domid vm devid private_list =
    with_xs (fun xs ->
        let uuid = uuid_of_string vm in
        let private_data_path = Device_common.get_private_data_path_of_device_by_uuid uuid "vif" devid in
        Xs.transaction xs (fun t ->
            t.Xst.mkdir private_data_path;
            t.Xst.setperms private_data_path
              Xs_protocol.ACL.({owner = backend_domid; other = NONE; acl = []});
            t.Xst.writev private_data_path
              (("bridge-MAC", "fe:ff:ff:ff:ff:ff") ::
               ("backend-kind", "vif") :: ("backend-id", string_of_int backend_domid) :: private_list);
          )
      )

  let write_extra_xenserver_keys device extra_xenserver_keys =
    let open Device_common in
    with_xs (fun xs ->
        let extra_xenserver_path = Device_common.extra_xenserver_path_of_device ~xs:xs device in
        Xs.transaction xs (fun t ->
            t.Xst.mkdir extra_xenserver_path;
            t.Xst.setperms extra_xenserver_path (Xenbus_utils.rwperm_for_guest device.frontend.domid);
            t.Xst.writev extra_xenserver_path extra_xenserver_keys;
          )
      )

  let remove_extra_xenserver_keys device =
    let open Device_common in
    with_xs (fun xs ->
        let extra_xenstore_path = Device_common.extra_xenserver_path_of_device ~xs:xs device in
        xs.Xs.rm extra_xenstore_path
      )

  let pre_plug vm hvm vif =
    debug "VIF.pre_plug";
    let backend_domid = with_xs (fun xs -> backend_domid_of xs vif) in
    let rate_bytes_per_interval, rate_interval_usecs =
      match vif.rate with
      | None -> 0L, 0l
      | Some (kbytes_per_s, timeslice_us) ->
        let ( ^* ) = Int64.mul and ( ^/ ) = Int64.div in
        let timeslice_us =
          if timeslice_us > 0L then
            timeslice_us
          else
            50000L (* 50ms by default *) in
        let bytes_per_interval = ((kbytes_per_s ^* 1024L) ^* timeslice_us) ^/ 1000000L in
        if bytes_per_interval > 0L && bytes_per_interval < 0xffffffffL then
          bytes_per_interval, timeslice_us |> Int64.to_int |> Int32.of_int
          (* [ "rate", sprintf "%Lu,%Lu" bytes_per_interval timeslice_us ] *)
        else (
          debug "VIF qos: invalid value for byte/interval: %Lu" bytes_per_interval;
          0L, 0l
        )
    in
    let devid = vif.position in
    let mtu = vif.mtu in
    let mac = Scanf.sscanf vif.mac "%02x:%02x:%02x:%02x:%02x:%02x" (fun a b c d e f -> [| a; b; c; d; e; f|]) in
    let bridge = bridge_of_vif vif.backend in
    let script = !Xl_resources.vif_script in
    let nictype = if hvm then Xenlight.NIC_TYPE_VIF_IOEMU else Xenlight.NIC_TYPE_VIF in

    let locking_mode = xenstore_of_locking_mode vif.locking_mode in
    let id = _device_id Device_common.Vif, id_of vif in
    let extra_private_keys =
      List.map (fun (k, v) -> "other-config/" ^ k, v) vif.other_config @
      (if mtu > 0 then [ "MTU", string_of_int mtu ] else []) @
      [ "xenopsd-backend", "xenlight" ] @
      [ "network-backend", get_network_backend () ] @
      [ "setup-vif-rules", !Xl_resources.setup_vif_rules ] @
      (id :: vif.extra_private_keys @ locking_mode)
    in
    (* write private XS keys *)
    write_private backend_domid vm devid extra_private_keys;

    let open Xenlight.Device_nic in
    let nic = with_ctx (fun ctx ->
        {(default ctx ()) with
         backend_domid; devid; mtu; model = None; mac; ip = None; bridge = Some bridge; ifname = None;
         script = Some script; nictype; rate_bytes_per_interval; rate_interval_usecs;
        }) in
    nic

  let plug_exn task vm vif =
    (* If the vif isn't listed as "active" then we don't automatically plug this one in *)
    if not(get_active vm vif)
    then debug "VIF %s.%s is not active: not plugging into VM" (fst vif.Vif.id) (snd vif.Vif.id)
    else
      on_frontend (fun _ xs frontend_domid hvm ->
          Xenops_task.with_subtask task (Printf.sprintf "Vif.add %s" (id_of vif))
            (fun () ->
               let nic = pre_plug vm hvm vif in

               (* call libxenlight to plug vif *)
               with_ctx (fun ctx ->
                   let open Xenlight.Device_nic in
                   debug "Calling Xenlight.Device_nic.add";
                   add ctx nic frontend_domid ()
                 );

               (* wait for plug (to be removed if possible) *)
               let open Device_common in
               let devid = vif.position in
               (* let mac = vif.mac in *)
               let backend_domid = with_xs (fun xs -> backend_domid_of xs vif) in
               let frontend = { domid = frontend_domid; kind = Vif; devid = devid } in
               let backend = { domid = backend_domid; kind = Vif; devid = devid } in
               let device = { backend = backend; frontend = frontend } in
               let static_ip_setting = xenstore_of_static_ip_setting vif in
               with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);
               write_extra_xenserver_keys device static_ip_setting;

               (* add disconnect flag *)
               let disconnect_path, flag = disconnect_flag device vif.locking_mode in
               with_xs (fun xs -> xs.Xs.write disconnect_path flag);
            )
        ) Newest vm

  let plug task vm = plug_exn task vm

  let unplug task vm vif force =
    with_ctx (fun ctx ->
        on_frontend (fun _ xs frontend_domid _ ->
            try
              let nic = Xenlight.Device_nic.of_devid ctx frontend_domid vif.position in
              Xenops_task.with_subtask task (Printf.sprintf "Vif.hard_shutdown %s" (id_of vif)) (fun () ->
                  if force then begin
                    debug "Calling Xenlight.Device_nic.destroy";
                    Xenlight.Device_nic.destroy ctx nic frontend_domid ()
                  end else begin
                    debug "Calling Xenlight.Device_nic.remove";
                    Xenlight.Device_nic.remove ctx nic frontend_domid ()
                  end
                );
              let device = device_by_id xs vm Device_common.Vif Oldest (id_of vif) in
              Xenops_task.with_subtask task (Printf.sprintf "Vif.release %s" (id_of vif))
                (fun () -> Hotplug.release' task ~xs device vm "vif" vif.position);
              remove_extra_xenserver_keys device
            with
            | _ ->
              debug "VM = %s; Ignoring missing device" (id_of vif)
          ) Oldest vm
      )

  let move' xs device bridge =
    let open Device_common in
    let xs_bridge_path = Printf.sprintf "/local/domain/%d/backend/vif/%d/%d/bridge" device.backend.domid
        device.frontend.domid device.frontend.devid in
    xs.Xs.write xs_bridge_path bridge;
    let domid = string_of_int device.frontend.domid in
    let devid = string_of_int device.frontend.devid in
    ignore (Forkhelpers.execute_command_get_output !Xl_resources.vif_script ["move"; "vif"; domid; devid])

  let move task vm vif network =
    let vm_t = DB.read_exn vm in
    with_xs
      (fun xs ->
         try
           (* If the device is gone then this is ok *)
           let device = device_by_id xs vm Device_common.Vif Oldest (id_of vif) in
           let bridge = match network with
             | Network.Local x -> x
             | Network.Remote (_, _) -> raise (Unimplemented("network driver domains")) in

           move' xs device bridge;

           (* If we have a qemu frontend, detach this too. *)
           let non_persistent = vm_t.VmExtra.non_persistent in
           if List.mem_assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs then begin
             match (List.assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs) with
             | _, Device device ->
               move' xs device bridge;
               let non_persistent = { non_persistent with
                                      VmExtra.qemu_vifs = List.remove_assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs } in
               DB.write vm { vm_t with VmExtra.non_persistent = non_persistent }
             | _, _ -> ()
           end

         with
         | (Does_not_exist(_,_)) ->
           debug "VM = %s; Ignoring missing domain" (id_of vif)
         | (Device_not_connected) ->
           debug "VM = %s; Ignoring missing device" (id_of vif)
      );
    ()

  let set_carrier task vm vif carrier =
    with_xs
      (fun xs ->
         try
           (* If the device is gone then this is ok *)
           let device = device_by_id xs vm Device_common.Vif Newest (id_of vif) in
           let disconnect_path = Hotplug.vif_disconnect_path device in
           xs.Xs.write disconnect_path (if carrier then "0" else "1");
         with
         | (Does_not_exist(_,_)) ->
           debug "VM = %s; Ignoring missing domain" (id_of vif)
         | (Device_not_connected) ->
           debug "VM = %s; Ignoring missing device" (id_of vif)
      )

  let set_locking_mode task vm vif mode =
    let open Device_common in
    with_xs
      (fun xs ->
         (* If the device is gone then this is ok *)
         let device = device_by_id xs vm Vif Newest (id_of vif) in
         let path = Device_common.get_private_data_path_of_device device in
         (* Delete the old keys *)
         List.iter (fun x -> safe_rm xs (path ^ "/" ^ x)) locking_mode_keys;
         List.iter (fun (x, y) -> xs.Xs.write (path ^ "/" ^ x) y) (xenstore_of_locking_mode mode);
         let disconnect_path, flag = disconnect_flag device mode in
         xs.Xs.write disconnect_path flag;

         let devid = string_of_int device.frontend.devid in
         let vif_interface_name = Printf.sprintf "vif%d.%s" device.frontend.domid devid in
         let tap_interface_name = Printf.sprintf "vif%d.%s-emu" device.frontend.domid devid in
         ignore (run !Xl_resources.setup_vif_rules ["xenlight"; vif_interface_name; vm; devid; "filter"]);
         (* Update rules for the tap device if the VM has booted HVM with no PV drivers. *)
         let di = with_ctx (fun ctx -> Xenlight.Dominfo.get ctx device.frontend.domid) in
         if di.Xenlight.Dominfo.domain_type = Xenlight.DOMAIN_TYPE_HVM
         then ignore (run !Xl_resources.setup_vif_rules ["xenlight"; tap_interface_name; vm; devid; "filter"])
      )

  let set_ip_unspecified xs xenstore_path suffix =
    Xs.transaction xs (fun t ->
        let ip_setting_enabled = Printf.sprintf "%s/%s%s" xenstore_path "enabled" suffix in
        t.Xst.write ip_setting_enabled "0";

        let ip_setting_address = Printf.sprintf "%s/%s%s" xenstore_path "address" suffix in
        t.Xst.rm ip_setting_address;

        let ip_setting_gateway = Printf.sprintf "%s/%s%s" xenstore_path "gateway" suffix in
        t.Xst.rm ip_setting_gateway
      )

  let set_ip_static xs xenstore_path suffix address gateway =
    Xs.transaction xs (fun t ->
        let ip_setting_enabled = Printf.sprintf "%s/%s%s" xenstore_path "enabled" suffix in
        t.Xst.write ip_setting_enabled "1";

        let ip_setting_address = Printf.sprintf "%s/%s%s" xenstore_path "address" suffix in
        t.Xst.write ip_setting_address address;

        let ip_setting_gateway = Printf.sprintf "%s/%s%s" xenstore_path "gateway" suffix in
        match gateway with
        | None ->
          t.Xst.rm ip_setting_gateway
        | Some value ->
          debug "xenstore-write %s <- %s" ip_setting_gateway value;
          t.Xst.write ip_setting_gateway value
      )

  let set_ipv4_configuration task vm vif ipv4_configuration =
    let open Device_common in
    with_xs
      (fun xs ->
         let device = device_by_id xs vm Vif Newest (id_of vif) in
         let xenstore_path =
           Printf.sprintf "%s/%s"
             (Device_common.extra_xenserver_path_of_device ~xs device)
             _static_ip_setting
         in
         match ipv4_configuration with
         | Unspecified4 ->
           set_ip_unspecified xs xenstore_path ""
         | Static4 (address :: _, gateway) ->
           set_ip_static xs xenstore_path "" address gateway
         | Static4 ([], _) ->
           raise (Internal_error "Static IPv4 configuration selected, but no address specified.")
      )

  let set_ipv6_configuration task vm vif ipv6_configuration =
    let open Device_common in
    with_xs
      (fun xs ->
         let device = device_by_id xs vm Vif Newest (id_of vif) in
         let xenstore_path =
           Printf.sprintf "%s/%s"
             (Device_common.extra_xenserver_path_of_device ~xs device)
             _static_ip_setting
         in
         match ipv6_configuration with
         | Unspecified6 ->
           set_ip_unspecified xs xenstore_path "6"
         | Static6 (address :: _, gateway) ->
           set_ip_static xs xenstore_path "6" address gateway
         | Static6 ([], _) ->
           raise (Internal_error "Static IPv6 configuration selected, but no address specified.")
      )

  let get_state vm vif =
    with_xs
      (fun xs ->
         try
           let (d: Device_common.device) = device_by_id xs vm Device_common.Vif Newest (id_of vif) in
           let path = Device_common.kthread_pid_path_of_device ~xs d in
           let kthread_pid = try xs.Xs.read path |> int_of_string with _ -> 0 in
           let pra_path = Device_common.vif_pvs_rules_active_path_of_device ~xs d in
           let pvs_rules_active = try (ignore (xs.Xs.read pra_path); true) with _ -> false in
           (* We say the device is present unless it has been deleted
              					   from xenstore. The corrolary is that: only when the device
              					   is finally deleted from xenstore, can we remove bridges or
              					   switch configuration. *)
           let domid = d.Device_common.frontend.Device_common.domid in
           let device = "vif" ^ (string_of_int domid) ^ "." ^ (string_of_int vif.position) in
           {
             Vif.active = true;
             plugged = true;
             media_present = true;
             kthread_pid = kthread_pid;
             device = Some device;
             pvs_rules_active = pvs_rules_active;
           }
         with
         | (Does_not_exist(_,_))
         | Device_not_connected ->
           { unplugged_vif with
             Vif.active = get_active vm vif
           }
      )

  let get_device_action_request vm vif =
    with_xs
      (fun xs ->
         try
           let (device: Device_common.device) = device_by_id xs vm Device_common.Vif Newest (id_of vif) in
           if Hotplug.device_is_online ~xs vm device
           then None
           else Some Needs_unplug
         with Device_not_connected ->
           None
      )

end

module ShutdownWatchers = struct
  type watcher = {
    m: Mutex.t;
    c: Condition.t;
    mutable finished: bool;
  }

  let m = Mutex.create ()
  let domid_to_watcher = Hashtbl.create 128

  let make domid =
    let w = { m = Mutex.create (); c = Condition.create (); finished = false } in
    Mutex.execute m (fun () ->
        let d = with_ctx (fun ctx -> Xenlight.Dominfo.get ctx domid) in
        if d.Xenlight.Dominfo.shutdown
        then w.finished <- true
        else Hashtbl.add domid_to_watcher domid w
      );
    w

  let wait w =
    Mutex.execute w.m
      (fun () ->
         while not w.finished do
           Condition.wait w.c w.m
         done
      )
  let broadcast domid =
    let all = Mutex.execute m (fun () ->
        let result = Hashtbl.find_all domid_to_watcher domid in
        List.iter (fun _ -> Hashtbl.remove domid_to_watcher domid) result;
        result
      ) in
    List.iter (fun w ->
        Mutex.execute w.m (fun () ->
            w.finished <- true;
            Condition.signal w.c;
          )
      ) all
end

let with_disk ~xs task disk write f = match disk with
  | Local path -> f path
  | VDI path ->
    let open Storage_interface in
    let open Storage in
    let sr, vdi = get_disk_by_name task path in
    let dp = Client.DP.create "with_disk" (Printf.sprintf "xenopsd/task/%s" (Xenops_task.id_of_handle task)) in
    finally
      (fun () ->
         let frontend_domid = this_domid ~xs in
         let frontend_vm = get_uuid frontend_domid in
         let vdi = attach_and_activate ~xs task frontend_vm dp sr vdi write in
         let device = VBD.create_vbd_frontend ~xs task frontend_domid vdi in
         finally
           (fun () ->
              device |> VBD.block_device_of_vbd_frontend |> f
           )
           (fun () ->
              VBD.destroy_vbd_frontend ~xs task device
           )
      )
      (fun () -> dp_destroy task dp)

let get_private_key ~xs vm kind devid x =
  let uuid = uuid_of_string vm in
  let private_data_path = Device_common.get_private_data_path_of_device_by_uuid uuid kind devid in
  let key = private_data_path ^ "/" ^x in
  try
    xs.Xs.read key
  with e ->
    error "read %s: Noent" key;
    raise e

module VM = struct
  include Xenops_server_skeleton.VM
  open Vm

  let compute_overhead domain =
    let static_max_mib = Memory.mib_of_bytes_used domain.VmExtra.memory_static_max in
    let memory_overhead_mib =
      (if domain.VmExtra.create_info.Domain.hvm then Memory.HVM.overhead_mib else Memory.Linux.overhead_mib)
        static_max_mib domain.VmExtra.vcpu_max domain.VmExtra.shadow_multiplier in
    Memory.bytes_of_mib memory_overhead_mib

  (* We compute our initial target at memory reservation time, done before the domain
     	   is created. We consume this information later when the domain is built. *)
  let set_initial_target ~xs domid initial_target =
    xs.Xs.write (Printf.sprintf "/local/domain/%d/memory/initial-target" domid)
      (Int64.to_string initial_target)
  let get_initial_target ~xs domid =
    Int64.of_string (xs.Xs.read (Printf.sprintf "/local/domain/%d/memory/initial-target" domid))

  (* Called from a xenops client if it needs to resume a VM that was suspended on a pre-xenopsd host. *)
  let generate_state_string vm =
    let open Memory in
    let builder_spec_info =
      match vm.ty with
      | HVM hvm_info ->
        Domain.BuildHVM {
          Domain.shadow_multiplier = hvm_info.shadow_multiplier;
          video_mib = minimum_videoram hvm_info.video_mib;
        }
      | PV { boot = Direct direct } ->
        Domain.BuildPV {
          Domain.cmdline = direct.cmdline;
          ramdisk = direct.ramdisk;
        }
      | PV { boot = Indirect { devices = [] } } ->
        raise (No_bootable_device)
      | PV { boot = Indirect ( { devices = d :: _ } ) } ->
        Domain.BuildPV {
          Domain.cmdline = "";
          ramdisk = None;
        }
      | PVinPVH _ -> failwith "not implemented"
    in
    let build_info = {
      Domain.memory_max = vm.memory_static_max /// 1024L;
      memory_target = vm.memory_dynamic_min /// 1024L;
      kernel = "";
      vcpus = vm.vcpu_max;
      priv = builder_spec_info;
    } in
    {
      VmExtra.build_info = Some build_info;
      ty = Some vm.ty;
      (* Earlier than the PV drivers update time, therefore
         			   any cached PV driver information will be kept. *)
      last_start_time = 0.;
      nomigrate = false;
      nested_virt = false
    } |> VmExtra.rpc_of_persistent_t |> Jsonrpc.to_string

  (* Could use fold_left to get the same value, but that would necessarily go through the whole list everytime, instead of the first n items, only. *)
  (* ToDo: This is complicated enough to warrant a test. *)
  (* Is it wise to fail silently on negative values?  (They are treated as zero, here.)
     	 Pro: Would mask fewer bugs.
     	 Con: Less robust.
     	*)
  let take n list =
    let ($) f a = f a in
    let rec helper i acc list =
      if i <= 0 || list = []
      then acc
      else helper (i-1)  (List.hd list :: acc) (List.tl list)
    in List.rev $ helper n [] list

  let generate_non_persistent_state xs vm =
    let hvm = match vm.ty with HVM _ -> true | _ -> false in
    (* XXX add per-vcpu information to the platform data *)
    (* VCPU configuration *)
    let pcpus = with_ctx (fun ctx ->
        Xenlight.Physinfo.((get ctx).nr_cpus)) |> Int32.to_int in
    let all_pcpus = pcpus |> Range.make 0 |> Range.to_list in
    let all_vcpus = vm.vcpu_max |> Range.make 0 |> Range.to_list in
    let masks = match vm.scheduler_params.affinity with
      | [] ->
        (* Every vcpu can run on every pcpu *)
        List.map (fun _ -> all_pcpus) all_vcpus
      | m :: ms ->
        (* Treat the first as the template for the rest *)
        let defaults = List.map (fun _ -> m) all_vcpus in
        take vm.vcpu_max (m :: ms @ defaults) in
    (* convert a mask into a binary string, one char per pCPU *)
    let bitmap cpus: string =
      let cpus = List.filter (fun x -> x >= 0 && x < pcpus) cpus in
      let result = String.make pcpus '0' in
      List.iter (fun cpu -> result.[cpu] <- '1') cpus;
      result in
    let affinity =
      List.mapi (fun idx mask ->
          Printf.sprintf "vcpu/%d/affinity" idx, bitmap mask
        ) masks in
    let weight = Opt.default [] (Opt.map
                                   (fun (w, c) -> [
                                        "vcpu/weight", string_of_int w;
                                        "vcpu/cap", string_of_int c
                                      ])
                                   vm.scheduler_params.priority
                                ) in
    let vcpus = [
      "vcpu/number", string_of_int vm.vcpu_max;
      "vcpu/current", string_of_int vm.vcpus;
    ] @ affinity @ weight in
    let create_info = {
      Domain.ssidref = vm.ssidref;
      hvm = hvm;
      hap = hvm;
      name = vm.name;
      xsdata = vm.xsdata;
      platformdata = vm.platformdata @ vcpus;
      bios_strings = vm.bios_strings;
      has_vendor_device = vm.has_vendor_device;
    } in
    {
      VmExtra.create_info = create_info;
      vcpu_max = vm.vcpu_max;
      vcpus = vm.vcpus;
      shadow_multiplier = (match vm.Vm.ty with Vm.HVM { Vm.shadow_multiplier = sm } -> sm | _ -> 1.);
      memory_static_max = vm.memory_static_max;
      suspend_memory_bytes = 0L;
      qemu_vbds = [];
      qemu_vifs = [];
      pci_msitranslate = vm.Vm.pci_msitranslate;
      pci_power_mgmt = vm.Vm.pci_power_mgmt;
      pv_drivers_detected = false;
    }


  (* TODO: libxl *)
  let on_domain f domain_selection (task: Xenops_task.task_handle) vm =
    let uuid = uuid_of_vm vm in
    with_xc_and_xs
      (fun xc xs ->
         match di_of_uuid domain_selection uuid with
         | None, _ -> raise (Does_not_exist("domain", vm.Vm.id))
         | Some di, multiple -> f xc xs task vm di multiple
      )

  let on_domain_if_exists f domain_selection (task: Xenops_task.task_handle) vm =
    try
      on_domain f domain_selection task vm
    with Does_not_exist("domain", _) ->
      debug "Domain for VM %s does not exist: ignoring" vm.Vm.id

  let add vm =
    let open Xenlight.Dominfo in
    with_xs
      (fun xs ->
         match di_of_uuid Newest (uuid_of_vm vm) with
         | None, _ -> () (* Domain doesn't exist so no setup required *)
         | Some di, _ ->
           debug "VM %s exists with domid=%d; checking whether xenstore is intact" vm.Vm.id di.domid;
           (* Minimal set of keys and values expected by tools like xentop (CA-24231) *)
           let minimal_local_kvs = [
             "name", vm.Vm.name;
             "domid", string_of_int di.domid;
             "vm", "/vm/" ^ vm.Vm.id;
             "memory/dynamic-min", Int64.(to_string (div vm.Vm.memory_dynamic_min 1024L));
             "memory/target", Int64.(to_string (div vm.Vm.memory_dynamic_min 1024L));
             "memory/dynamic-max", Int64.(to_string (div vm.Vm.memory_dynamic_max 1024L))
           ] |> List.map (fun (k, v) -> Printf.sprintf "/local/domain/%d/%s" di.domid k, v) in
           let minimal_vm_kvs = [
             "uuid", vm.Vm.id;
             "name", vm.Vm.name;
             (*							Printf.sprintf "domains/%d" di.domid, Printf.sprintf "/local/domain/%d" di.domid;
               							Printf.sprintf "domains/%d/create-time" di.domid, "0"*)
           ] |> List.map (fun (k, v) -> Printf.sprintf "/vm/%s/%s" vm.Vm.id k, v) in
           List.iter
             (fun (k, v) ->
                if try ignore(xs.Xs.read k); false with _ -> true then begin
                  debug "xenstore-write %s <- %s" k v;
                  xs.Xs.write k v
                end
             ) (minimal_local_kvs @ minimal_vm_kvs)
      )

  let remove vm =
    with_xs
      (fun xs ->
         (*				safe_rm xs (Printf.sprintf "/vm/%s" vm.Vm.id);*)
         safe_rm xs (Printf.sprintf "/vss/%s" vm.Vm.id);
      )

  let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg

  let destroy = on_domain_if_exists (fun xc xs task vm di multiple ->
      let open Xenlight.Dominfo in
      let domid = di.domid in
      let uuid = uuid_of_string vm.id in
      let devices = Device_common.list_frontends ~xs domid in
      let vbds = List.filter (fun device -> match Device_common.(device.frontend.kind) with Device_common.Vbd _ -> true | _ -> false) devices in
      let vbds = List.map (fun device -> Device_common.(device.frontend.devid)) vbds in
      let dps = List.map (fun devid -> get_private_key ~xs vm.id "vbd" devid _dp_id) vbds in

      (* Normally we throw-away our domain-level information. If the domain
         		   has suspended then we preserve it. *)
      if di.shutdown && (di.shutdown_reason = Xenlight.SHUTDOWN_REASON_SUSPEND)
      then debug "VM = %s; domid = %d; domain has suspended; preserving domain-level information" vm.Vm.id di.domid
      else begin
        debug "VM = %s; domid = %d; will not have domain-level information preserved" vm.Vm.id di.domid;
        if DB.exists vm.Vm.id then DB.remove vm.Vm.id;
      end;
      debug "Calling Xenlight.domain_destroy domid=%d" domid;
      with_ctx (fun ctx -> Xenlight.Domain.destroy ctx domid ());
      debug "Call Xenlight.domain_destroy domid=%d completed" domid;

      let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg in
      let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x in
      if not multiple then
        log_exn_rm ~xs (Device_common.get_private_path_by_uuid uuid)
      else
        log_exn_rm ~xs (Hotplug.get_hotplug_base_by_uuid uuid domid);

      (* Detach any remaining disks *)
      List.iter (fun dp ->
          try
            Storage.dp_destroy task dp
          with e ->
            warn "Ignoring exception in VM.destroy: %s" (Printexc.to_string e)) dps;
      (* The domain may still exist in the dying state. If someone calls 'get_state'
         		   then they may see the dying domain and think it's still running.
         		   If the domain persists in the dying state then this indicates a bug in a backend --
         		   we want to catch this failure early and not wait until the host is out of
         		   resources. *)
      begin
        try
          let still_exists () = try let (_: Xenctrl.domaininfo) = Xenctrl.domain_getinfo xc domid in true with _ -> false in
          let start = Unix.gettimeofday () in
          let timeout = 60. in
          while still_exists () && (Unix.gettimeofday () -. start < timeout) do
            Thread.delay 5.
          done;
          if still_exists () then begin
            error "VM = %s; domid = %d; Domain stuck in dying state after %.0f seconds. This probably indicates a backend driver bug." vm.Vm.id domid timeout;
            raise (Domain_stuck_in_dying_state domid)
          end
        with _ -> ()
      end
    ) Oldest

  let pause = on_domain (fun _ xs _ _ di _ ->
      let open Xenlight.Dominfo in
      if di.current_memkb = 0L then raise (Domain_not_built);
      with_ctx (fun ctx -> Xenlight.Domain.pause ctx di.domid)
    ) Newest

  let unpause = on_domain (fun _ xs _ _ di _ ->
      let open Xenlight.Dominfo in
      if di.current_memkb = 0L then raise (Domain_not_built);
      with_ctx (fun ctx -> Xenlight.Domain.unpause ctx di.domid)
    ) Newest

  let set_xsdata task vm xsdata = on_domain (fun _ xs _ _ di _ ->
      let open Xenlight.Dominfo in
      Domain.set_xsdata ~xs di.domid xsdata
    ) Newest task vm

  let set_vcpus task vm target = on_domain (fun _ xs _ _ di _ ->
      let open Xenlight.Dominfo in

      let set ~xs ~devid domid online =
        let path = Printf.sprintf "/local/domain/%d/cpu/%d/availability" domid devid in
        xs.Xs.write path (if online then "online" else "offline")
      in

      let status ~xs ~devid domid =
        let path = Printf.sprintf "/local/domain/%d/cpu/%d/availability" domid devid in
        try match xs.Xs.read path with
          | "online"  -> true
          | "offline" -> false
          | _         -> (* garbage, assuming false *) false
        with Xs_protocol.Enoent _ -> false
      in

      let domid = di.domid in
      (* Returns the instantaneous CPU number from xenstore *)
      let current =
        let n = ref (-1) in
        for i = 0 to vm.Vm.vcpu_max - 1
        do if status ~xs ~devid:i domid then n := i
        done;
        !n + 1 in

      if current > target then (
        (* need to deplug cpus *)
        for i = current - 1 downto target
        do
          set ~xs ~devid:i domid false
        done
      ) else if current < target then (
        (* need to plug cpus *)
        for i = current to (target - 1)
        do
          set ~xs ~devid:i domid true
        done
      )
    ) Newest task vm

  (* TODO: libxl *)
  let set_shadow_multiplier task vm target = on_domain (fun xc xs _ _ di _ ->
      let open Xenlight.Dominfo in
      if di.domain_type = Xenlight.DOMAIN_TYPE_PV then
        raise (Unimplemented "shadow_multiplier for PV domains");
      let domid = di.domid in
      let static_max_mib = Memory.mib_of_bytes_used vm.Vm.memory_static_max in
      let newshadow = Int64.to_int (Memory.HVM.shadow_mib static_max_mib vm.Vm.vcpu_max target) in
      (* NB: Xenctrl.shadow_allocation_{get,set} do not yet have a libxl replacement *)
      let curshadow = Xenctrl.shadow_allocation_get xc domid in
      let needed_mib = newshadow - curshadow in
      debug "VM = %s; domid = %d; Domain has %d MiB shadow; an increase of %d MiB requested" vm.Vm.id domid curshadow needed_mib;
      if not(Domain.wait_xen_free_mem (Int64.mul (Int64.of_int needed_mib) 1024L)) then begin
        error "VM = %s; domid = %d; Failed waiting for Xen to free %d MiB: some memory is not properly accounted" vm.Vm.id domid needed_mib;
        raise (Not_enough_memory (Memory.bytes_of_mib (Int64.of_int needed_mib)))
      end;
      debug "VM = %s; domid = %d; shadow_allocation_setto %d MiB" vm.Vm.id domid newshadow;
      Xenctrl.shadow_allocation_set xc domid newshadow;
    ) Newest task vm

  (* TODO: libxl *)
  let set_memory_dynamic_range task vm min max = on_domain (fun xc xs _ _ di _ ->
      let open Xenlight.Dominfo in
      let domid = di.domid in
      Domain.set_memory_dynamic_range ~xc ~xs
        ~min:(Int64.to_int (Int64.div min 1024L))
        ~max:(Int64.to_int (Int64.div max 1024L))
        domid;
      Mem.balance_memory (Xenops_task.get_dbg task)
    ) Newest task vm

  (*let create task memory_upper_bound vm vbds =*)
  let build ?restore_fd task vm vbds vifs vgpus vusbs extras force =
    let memory_upper_bound = None in
    let k = vm.Vm.id in

    (* We should prevent leaking files in our filesystem *)
    let kernel_to_cleanup = ref None in
    finally (fun () -> with_xc_and_xs (fun xc xs ->
        let persistent, non_persistent =
          match DB.read k with
          | Some x ->
            debug "VM = %s; reloading stored domain-level configuration" vm.Vm.id;
            x.VmExtra.persistent, x.VmExtra.non_persistent
          | None -> begin
              debug "VM = %s; has no stored domain-level configuration, regenerating" vm.Vm.id;
              let persistent =
                { VmExtra.build_info = None
                ; ty = None
                ; last_start_time = Unix.gettimeofday ()
                ; nomigrate = Platform.is_true
                      ~key:"nomigrate"
                      ~platformdata:vm.Xenops_interface.Vm.platformdata
                      ~default:false
                ; nested_virt=Platform.is_true
                      ~key:"nested-virt"
                      ~platformdata:vm.Xenops_interface.Vm.platformdata
                      ~default:false
                } in
              let non_persistent = generate_non_persistent_state xs vm in
              persistent, non_persistent
            end in
        let open Memory in
        let overhead_bytes = compute_overhead non_persistent in
        let resuming = non_persistent.VmExtra.suspend_memory_bytes <> 0L in
        (* If we are resuming then we know exactly how much memory is needed. If we are
           			   live migrating then we will only know an upper bound. If we are starting from
           			   scratch then we have a free choice. *)
        let min_bytes, max_bytes = match memory_upper_bound with
          | Some x ->
            debug "VM = %s; using memory_upper_bound = %Ld" vm.Vm.id x;
            x, x
          | None ->
            if resuming then begin
              debug "VM = %s; using stored suspend_memory_bytes = %Ld" vm.Vm.id non_persistent.VmExtra.suspend_memory_bytes;
              non_persistent.VmExtra.suspend_memory_bytes, non_persistent.VmExtra.suspend_memory_bytes
            end else begin
              debug "VM = %s; using memory_dynamic_min = %Ld and memory_dynamic_max = %Ld" vm.Vm.id vm.memory_dynamic_min vm.memory_dynamic_max;
              vm.memory_dynamic_min, vm.memory_dynamic_max
            end in
        let min_kib = kib_of_bytes_used (min_bytes +++ overhead_bytes)
        and max_kib = kib_of_bytes_used (max_bytes +++ overhead_bytes) in
        (* XXX: we would like to be able to cancel an in-progress with_reservation *)
        Mem.with_reservation (Xenops_task.get_dbg task) min_kib max_kib (fun target_plus_overhead_kib reservation_id ->
            DB.write k {
              VmExtra.persistent = persistent;
              VmExtra.non_persistent = non_persistent
            };

            let max_vcpus = vm.vcpu_max in
            let avail_vcpus = Array.init max_vcpus (fun i -> i < vm.vcpus) in
            let max_memkb = vm.memory_static_max /// 1024L in
            let target_memkb =
              let target_plus_overhead_bytes = bytes_of_kib target_plus_overhead_kib in
              let target_bytes = target_plus_overhead_bytes --- overhead_bytes in
              let target_bytes = min vm.memory_dynamic_max target_bytes in
              target_bytes /// 1024L
            in
            let video_memkb, shadow_memkb =
              match vm.ty with
              | HVM hvm_info ->
                Int64.mul (Int64.of_int (minimum_videoram hvm_info.video_mib)) 1024L,
                Int64.mul
                  (Memory.HVM.shadow_mib (max_memkb /// 1024L) max_vcpus hvm_info.shadow_multiplier)
                  1024L
              | PV _ -> 0L, 0L
              | PVinPVH _ -> failwith "not implemented"
            in
            let b_info =
              let open Xenlight.Domain_build_info in
              match vm.Vm.ty with
              | HVM hvm_info ->
                let b_info_default = with_ctx (fun ctx -> default ctx ~xl_type:Xenlight.DOMAIN_TYPE_HVM ()) in
                let b_info_hvm_default =
                  match b_info_default with
                  | { xl_type = Hvm b_info_hvm_default } ->
                    b_info_hvm_default
                  | _ -> failwith "Expected HVM build_info here!"
                in
                let vnc_info =
                  let listen =
                    if !Xenopsd.use_upstream_qemu then
                      let console_path = Printf.sprintf "unix:%s/%s" !Xl_resources.vnc_dir vm.Vm.id in
                      Some console_path
                    else None
                  in
                  let open Xenlight.Vnc_info in
                  let vnc_info_default = with_ctx (fun ctx -> default ctx ()) in
                  { vnc_info_default with enable = Some true; listen = listen }
                in
                { b_info_default with
                  xl_type = Hvm { b_info_hvm_default with
                                  pae = Some true;
                                  apic = Some true;
                                  acpi = Some hvm_info.Xenops_interface.Vm.acpi;
                                  nx = Some true;
                                  timeoffset = Some hvm_info.Xenops_interface.Vm.timeoffset;
                                  nested_hvm = Some true;
                                  vnc = vnc_info;
                                  keymap = hvm_info.Xenops_interface.Vm.keymap;
                                  boot = Some hvm_info.Xenops_interface.Vm.boot_order;
                                  usb = Some true;
                                  usbdevice_list = [ "tablet" ];
                                  #if xen45 = 1
                                          serial_list = begin
                                                match hvm_info.Xenops_interface.Vm.serial with
                                                | Some x -> [x] | None -> []
                                              end;
                                    serial = None;
                                    #else
  serial = hvm_info.Xenops_interface.Vm.serial
           #endif
}
}
| PV { Xenops_interface.Vm.boot = Direct direct } ->
  let b_info_default = with_ctx (fun ctx -> default ctx ~xl_type:Xenlight.DOMAIN_TYPE_PV ()) in
  let b_info_pv_default =
    match b_info_default with
    | { xl_type = Pv b_info_pv_default } -> b_info_pv_default
    | _ -> failwith "Expected PV build_info here!"
  in
  { b_info_default with
    xl_type = Pv { b_info_pv_default with
                   kernel = Some direct.Xenops_interface.Vm.kernel;
                   cmdline = Some direct.Xenops_interface.Vm.cmdline;
                   ramdisk = direct.Xenops_interface.Vm.ramdisk;
                 }
  }
| PV { Xenops_interface.Vm.boot = Indirect { devices = [] } } ->
  raise (No_bootable_device)
| PV { Xenops_interface.Vm.boot = Indirect ( { devices = d :: _ } as i ) } ->
  let b_info_default = with_ctx (fun ctx -> default ctx ~xl_type:Xenlight.DOMAIN_TYPE_PV ()) in
  let b_info_pv_default =
    match b_info_default with
    | { xl_type = Pv b_info_pv_default } -> b_info_pv_default
    | _ -> failwith "Expected PV build_info here!"
  in
  if restore_fd = None then
    (* We can't use libxl's builtin support for a bootloader because it
       							   doesn't understand the convention used by eliloader. *)
    with_disk ~xs task d false
      (fun dev ->
         let b = Bootloader.extract task
             ~bootloader:i.Xenops_interface.Vm.bootloader
             ~legacy_args:i.legacy_args ~extra_args:i.extra_args
             ~pv_bootloader_args:i.Xenops_interface.Vm.bootloader_args
             ~disk:dev ~vm:vm.Vm.id () in
         kernel_to_cleanup := Some b;
         { b_info_default with
           xl_type = Pv { b_info_pv_default with
                          kernel = Some b.Bootloader.kernel_path;
                          cmdline = Some b.Bootloader.kernel_args;
                          ramdisk = b.Bootloader.initrd_path
                        }
         }
      )
  else
    { b_info_default with
      xl_type = Pv { b_info_pv_default with
                     bootloader = Some i.Xenops_interface.Vm.bootloader;
                     bootloader_args = (*i.bootloader_args :: i.legacy_args :: i.extra_args ::*) [];
                   }
    }
    | PVinPVH _ -> failwith "not implemented"
in
let k = vm.Vm.id in
let d = DB.read_exn vm.Vm.id in
let persistent = { d.VmExtra.persistent with
                   VmExtra.build_info = None; (* !!! *)
                   ty = Some vm.ty;
                 } in
DB.write k {
  VmExtra.persistent = persistent;
  VmExtra.non_persistent = d.VmExtra.non_persistent;
};
let hvm = match vm.ty with HVM _ | PVinPVH _ -> true | PV _ -> false in

(* devices *)
let disks, vbds_extra = List.split (List.map (fun vbd ->
    match VBD.pre_plug task vm.Vm.id hvm vbd with
    | (disk, devid, extra_backend_keys, backend_domid) -> disk, (vbd, devid, extra_backend_keys, backend_domid)) vbds) in
let disks = Array.of_list disks in
let nics = Array.of_list (List.map (VIF.pre_plug vm.Vm.id hvm) vifs) in
let vfbs = [||] in
let vkbs = [||] in

(* choice of QEMU *)
let device_model_version =
  if !Xenopsd.use_upstream_qemu then
    Xenlight.DEVICE_MODEL_VERSION_QEMU_XEN
  else
    Xenlight.DEVICE_MODEL_VERSION_QEMU_XEN_TRADITIONAL
in

(* create and build structures *)
let c_info = Xenlight.Domain_create_info.({ (with_ctx (fun ctx -> default ctx ())) with
                                            xl_type = (if hvm then Xenlight.DOMAIN_TYPE_HVM else Xenlight.DOMAIN_TYPE_PV);
                                            hap = Some hvm;
                                            ssidref = vm.Vm.ssidref;
                                            name = if restore_fd = None then Some vm.Vm.name else Some (vm.Vm.name ^ "--incoming");
                                            uuid = vm |> uuid_of_vm |> Xenctrl_uuid.handle_of_uuid;
                                            xsdata = vm.Vm.xsdata;
                                            platformdata = non_persistent.VmExtra.create_info.Domain.platformdata;
                                            run_hotplug_scripts = Some !Xenopsd.run_hotplug_scripts;
                                          }) in
let b_info = Xenlight.Domain_build_info.({ b_info with
                                           max_vcpus;
                                           avail_vcpus;
                                           max_memkb;
                                           target_memkb;
                                           video_memkb;
                                           shadow_memkb;
                                           rtc_timeoffset = 0l;
                                           device_model_version;
                                           (* device_model_stubdomain = None; *)
                                           (* device_model = Some "/usr/lib/xen/bin/qemu-system-i386"; *)
                                           extra = [];
                                           extra_pv = [];
                                           extra_hvm = [];
                                           sched_params = Xenlight.Domain_sched_params.({(with_ctx (fun ctx -> default ctx ())) with weight = -1; cap = -1; period = -1; slice = -1; latency = -1; extratime = -1});
                                         }) in
let domain_config = Xenlight.Domain_config.({(with_ctx (fun ctx -> default ctx ())) with
                                             c_info;
                                             b_info;
                                             disks;
                                             nics;
                                             pcidevs = [||];
                                             vfbs;
                                             vkbs;
                                             on_poweroff = Xenlight.ACTION_ON_SHUTDOWN_DESTROY;
                                             on_reboot = Xenlight.ACTION_ON_SHUTDOWN_RESTART;
                                             on_watchdog = Xenlight.ACTION_ON_SHUTDOWN_DESTROY;
                                             on_crash = Xenlight.ACTION_ON_SHUTDOWN_RESTART;
                                            }) in

(* Start or resume *)
let domid =
  match restore_fd with
  | None ->
    debug "Calling Xenlight.Domain.create_new";
    (*						let domid = with_ctx (fun ctx -> Xenlight_events.async (Xenlight.Domain.create_new ctx domain_config)) in*)
    let domid = with_ctx (fun ctx -> Xenlight.Domain.create_new ctx domain_config ()) in
    debug "Call Xenlight.Domain.create_new completed";
    domid
  | Some fd ->
    debug "Calling Xenlight.domain_create_restore";
    with_ctx (fun ctx ->
        let params = Xenlight.Domain_restore_params.default ctx () in
        (*							let domid = Xenlight_events.async (Xenlight.Domain.create_restore ctx domain_config (fd, params)) in*)
        let domid = Xenlight.Domain.create_restore ctx domain_config (fd, params) () in
        debug "Call Xenlight.Domain.create_restore completed";
        domid
      )
in
debug "Xenlight has created domain %d" domid;

(* Wait for device hotplugs to finish, and write remaining xenstore keys *)

List.iter (fun vif ->
    (* wait for plug (to be removed if possible) *)
    let open Device_common in
    let open Vif in
    let devid = vif.position in
    let backend_domid = with_xs (fun xs -> VIF.backend_domid_of xs vif) in
    let frontend = { domid; kind = Vif; devid = devid } in
    let backend = { domid = backend_domid; kind = Vif; devid = devid } in
    let device = { backend = backend; frontend = frontend } in
    with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);

    (* add disconnect flag *)
    let disconnect_path, flag = VIF.disconnect_flag device vif.locking_mode in
    with_xs (fun xs -> xs.Xs.write disconnect_path flag);
  ) vifs;

List.iter (function (vbd, devid, _, backend_domid) ->
    (* wait for plug *)
    let device =
      let open Device_common in
      let kind = VBD.device_kind_of ~xs vbd in
      let frontend = { domid; kind; devid } in
      let backend = { domid = backend_domid; kind; devid } in
      { backend = backend; frontend = frontend }
    in
    with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);
  ) vbds_extra;

if restore_fd <> None then begin
  let dom_path = xs.Xs.getdomainpath domid in
  xs.Xs.write (dom_path ^ "/name") vm.Vm.name;
end;

Mem.transfer_reservation_to_domain (Xenops_task.get_dbg task) domid reservation_id;

Int64.(
  let min = to_int (div vm.Vm.memory_dynamic_min 1024L)
  and max = to_int (div vm.Vm.memory_dynamic_max 1024L) in
  Domain.set_memory_dynamic_range ~xc ~xs ~min ~max domid
);

(* Create read/write nodes for the guest to use *)
let dom_path = xs.Xs.getdomainpath domid in
let rwperm = Xenbus_utils.rwperm_for_guest domid in
List.iter (fun dir ->
    let ent = Printf.sprintf "%s/%s" dom_path dir in
    with_xs (fun xs ->
        xs.Xs.mkdir ent;
        xs.Xs.setperms ent rwperm
      )
  ) (
  let dev_kinds = [ "vbd"; "vif" ] in
  [ "feature"; "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages"; "vm-data" ]
  @ List.map (fun dev_kind -> "device/"^dev_kind) dev_kinds
);
(* Write extra VBD XS keys *)
List.iter (fun (vbd, devid, extra_backend_keys, backend_domid) ->
    VBD.write_extra backend_domid domid devid extra_backend_keys;

    (* We store away the disk so we can implement VBD.stat *)
    let kind = VBD.device_kind_of ~xs vbd in
    let device =
      let open Device_common in
      let frontend = { domid = domid; kind; devid } in
      let backend = { domid = backend_domid; kind; devid } in
      { backend = backend; frontend = frontend }
    in
    with_xs (fun xs ->
        Opt.iter (fun disk -> xs.Xs.write (VBD.vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string)) vbd.Vbd.backend;
      )
  ) vbds_extra;

(* Starts vncterm for a PV guest *)
(match vm.Vm.ty with
 | Vm.PV { vncterm = true; vncterm_ip = ip } -> Device.PV_Vnc.start ~xs ?ip domid
 | _ -> ());
)
)
) (fun () -> Opt.iter Bootloader.delete !kernel_to_cleanup)

let request_shutdown task vm reason ack_delay =
  on_domain
    (fun _ xs task vm di _ ->
       let open Xenlight.Dominfo in
       let domid = di.domid in
       try
         match reason with
         | Reboot ->
           debug "Calling Xenlight.Domain.reboot domid=%d" domid;
           with_ctx (fun ctx -> Xenlight.Domain.reboot ctx domid);
           true
         | PowerOff -> false
         | Suspend -> false
         | Halt ->
           debug "Calling Xenlight.Domain.shutdown domid=%d" domid;
           with_ctx (fun ctx -> Xenlight.Domain.shutdown ctx domid);
           true
         | S3Suspend -> false
       with Watch.Timeout _ ->
         false
    ) Oldest task vm

let wait_shutdown task vm reason timeout =
  on_domain
    (fun _ _ _ _ di _ ->
       let open Xenlight.Dominfo in
       let domid = di.domid in
       debug "Registering for domid %d shutdown" domid;
       let w = ShutdownWatchers.make domid in
       (* The shutdown might have happened before we registered our interest *)
       ShutdownWatchers.wait w;
       true
    ) Oldest task vm

(* Mount a filesystem somewhere, with optional type *)
let mount ?ty:(ty = None) src dest =
  let ty = match ty with None -> [] | Some ty -> [ "-t"; ty ] in
  run !Xl_resources.mount (ty @ [ src; dest ]) |> ignore_string

let timeout = 300. (* 5 minutes: something is seriously wrong if we hit this timeout *)
exception Umount_timeout

(** Unmount a mountpoint. Retries every 5 secs for a total of 5mins before returning failure *)
let umount ?(retry=true) dest =
  let finished = ref false in
  let start = Unix.gettimeofday () in

  while not(!finished) && (Unix.gettimeofday () -. start < timeout) do
    try
      run !Xl_resources.umount [dest] |> ignore_string;
      finished := true
    with e ->
      if not(retry) then raise e;
      debug "Caught exception (%s) while unmounting %s: pausing before retrying"
        (Printexc.to_string e) dest;
      Thread.delay 5.
  done;
  if not(!finished) then raise Umount_timeout

let with_mounted_dir device f =
  let mount_point = Filename.temp_file "xenops_mount_" "" in
  Unix.unlink mount_point;
  Unix.mkdir mount_point 0o640;
  finally
    (fun () ->
       mount ~ty:(Some "ext2") device mount_point;
       f mount_point)
    (fun () ->
       (try umount mount_point with e -> debug "Caught %s" (Printexc.to_string e));
       (try Unix.rmdir mount_point with e -> debug "Caught %s" (Printexc.to_string e))
    )

(** open a file, and make sure the close is always done *)
let with_data ~xs task data write f = match data with
  | Disk disk ->
    with_disk ~xs task disk write (fun path ->
        let with_fd_of_path p f =
          let is_raw_image =
            Stdext.Unixext.with_file path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o400 (fun fd ->
                match Suspend_image.read_save_signature fd with
                | `Ok _ -> true | _ -> false
              )
          in
          match (write, is_raw_image) with
          | true, _ -> (* Always write raw *)
            Stdext.Unixext.with_file path [Unix.O_WRONLY; Unix.O_CLOEXEC] 0o600 f
          | false, true -> (* We're reading raw *)
            Stdext.Unixext.with_file path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o600 f
          | false, false -> (* Assume reading from filesystem *)
            with_mounted_dir p (fun dir ->
                let filename = dir ^ "/suspend-image" in
                Unixext.with_file filename [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o600 f
              )
        in
        with_fd_of_path path (fun fd ->
            finally
              (fun () -> f fd)
              (fun () ->
                 try Xapi_stdext_unix.Unixext.fsync fd;
                 with Unix.Unix_error(Unix.EIO, _, _) ->
                   error "Caught EIO in fsync after suspend; suspend image may be corrupt";
                   raise (IO_error)
              )
          )
      )
  | FD fd -> f fd

let save task progress_callback vm flags data vgpu_data =
  let open Xenlight.Dominfo in
  on_domain
    (fun xc xs (task:Xenops_task.task_handle) vm di _ ->
       let domid = di.domid in
       with_data ~xs task data true
         (fun fd ->
            debug "Writing save signature";
            Io.write fd suspend_save_signature;
            debug "Calling Xenlight.Domain.suspend domid=%d" domid;
            with_ctx (fun ctx -> Xenlight.Domain.suspend ctx domid fd ());
            debug "Call Xenlight.Domain.suspend domid=%d completed" domid;
            ignore (wait_shutdown task vm Suspend 1200.);

            (* Record the final memory usage of the domain so we know how
               						   much to allocate for the resume *)
            let pages = Memory.pages_of_kib_used di.current_memkb in
            debug "VM = %s; domid = %d; Final memory usage of the domain = %Ld pages" vm.Vm.id domid pages;
            (* Flush all outstanding disk blocks *)

            let k = vm.Vm.id in
            let d = DB.read_exn vm.Vm.id in

            with_ctx (fun ctx ->
                let open Xenlight.Device_disk in
                let disks = list ctx domid in
                List.iter (fun disk ->
                    debug "Calling Xenlight.Device_disk.destroy";
                    destroy ctx disk domid ();
                    debug "Call Xenlight.Device_disk.destroy completed";
                  ) disks;
                debug "VM = %s; domid = %d; Disk backends have all been flushed" vm.Vm.id domid;

                List.iter (fun disk ->
                    match disk.vdev with
                    | Some vdev ->
                      let devid = vdev |> Device_number.of_linux_device |> Device_number.to_xenstore_key in
                      let backend = get_private_key ~xs k "vbd" devid _vdi_id |> Jsonrpc.of_string |> backend_of_rpc in
                      let dp = get_private_key ~xs k "vbd" devid _dp_id in
                      begin match backend with
                        | None (* can never happen due to 'filter' above *)
                        | Some (Local _) -> ()
                        | Some (VDI path) ->
                          let sr, vdi = Storage.get_disk_by_name task path in
                          Storage.deactivate task dp sr vdi
                      end
                    | None -> warn "No vdev found for disk!"
                  ) disks;
              );

            debug "VM = %s; domid = %d; Storing final memory usage" vm.Vm.id domid;
            let non_persistent = { d.VmExtra.non_persistent with
                                   VmExtra.suspend_memory_bytes = Memory.bytes_of_pages pages;
                                 } in
            DB.write k { d with
                         VmExtra.non_persistent = non_persistent;
                       }
         )
    ) Oldest task vm

let restore task progress_callback vm vbds vifs data vgpu_data extras =
  with_xs (fun xs ->
      with_data ~xs task data false (fun fd ->
          let vbds = List.filter (fun vbd -> vbd.Vbd.mode = Vbd.ReadOnly) vbds in
          debug "Reading save signature";
          let read_signature = Io.read fd (String.length suspend_save_signature) in
          if read_signature <> suspend_save_signature then begin
            error "VM = %s; read invalid save file signature: \"%s\"" vm.Vm.id read_signature;
            raise Restore_signature_mismatch
          end;
          build ~restore_fd:fd task vm vbds vifs [] [] extras false
        )
    )

(* TODO: libxl *)
let s3suspend task vm =
  raise (Unimplemented "s3suspend")

(* TODO: libxl *)
let s3resume task vm =
  raise (Unimplemented "s3resume")

let get_state vm =
  let open Xenlight.Dominfo in
  let uuid = uuid_of_vm vm in
  let vme = vm.Vm.id |> DB.read in (* may not exist *)
  (* TODO: libxl *)
  with_xc_and_xs
    (fun xc xs ->
       match di_of_uuid Newest uuid with
       | None, _ ->
         (* XXX: we need to store (eg) guest agent info *)
         begin match vme with
           | Some vmextra when vmextra.VmExtra.non_persistent.VmExtra.suspend_memory_bytes = 0L ->
             halted_vm
           | Some _ ->
             { halted_vm with Vm.power_state = Suspended }
           | None ->
             halted_vm
         end
       | Some di, _ ->
         let hvm = di.domain_type = Xenlight.DOMAIN_TYPE_HVM in
         let vnc = Opt.map (fun port -> { Vm.protocol = Vm.Rfb; port = port; path = "" })
             (Device.get_vnc_port ~xs di.domid) in
         (* Using the upstream qemu we access the console over a Unix domain socket *)
         let qemu_unix_vnc =
           if hvm
           then [ { Vm.protocol = Vm.Rfb; port = 0; path = Filename.concat !Xl_resources.vnc_dir (Uuidm.to_string uuid) } ]
           else [] in
         let tc = Opt.map (fun port -> { Vm.protocol = Vm.Vt100; port = port; path = "" })
             (Device.get_tc_port ~xs di.domid) in
         let local x = Printf.sprintf "/local/domain/%d/%s" di.domid x in
         let uncooperative = try ignore_string (xs.Xs.read (local "memory/uncooperative")); true with Xs_protocol.Enoent _ -> false in
         let memory_target = try xs.Xs.read (local "memory/target") |> Int64.of_string |> Int64.mul 1024L with Xs_protocol.Enoent _ -> 0L in
         let memory_actual =
           Memory.bytes_of_kib di.current_memkb in

         let memory_limit =
           (* The maximum amount of memory the domain can consume is the max of memory_actual
              							   and max_memory_pages (with our overheads subtracted). *)
           let max_memory_bytes =
             let overhead_bytes = Memory.bytes_of_mib (if hvm then Memory.HVM.xen_max_offset_mib else Memory.Linux.xen_max_offset_mib) in
             let raw_bytes = Memory.bytes_of_kib di.max_memkb in
             Int64.sub raw_bytes overhead_bytes in
           (* CA-31764: may be larger than static_max if maxmem has been increased to initial-reservation. *)
           max memory_actual max_memory_bytes in

         let rtc = try xs.Xs.read (Printf.sprintf "/vm/%s/rtc/timeoffset" (Uuidm.to_string uuid)) with Xs_protocol.Enoent _ -> "" in
         let rec ls_lR root dir =
           let this = try [ dir, xs.Xs.read (root ^ "/" ^ dir) ] with _ -> [] in
           let subdirs = try xs.Xs.directory (root ^ "/" ^ dir) |> List.filter (fun x -> x <> "") |> List.map (fun x -> dir ^ "/" ^ x) with _ -> [] in
           this @ (List.concat (List.map (ls_lR root) subdirs)) in
         let guest_agent =
           [ "drivers"; "attr"; "data"; "control"; "feature" ] |> List.map (ls_lR (Printf.sprintf "/local/domain/%d" di.domid)) |> List.concat in
         let xsdata_state =
           Domain.allowed_xsdata_prefixes |> List.map (ls_lR (Printf.sprintf "/local/domain/%d" di.domid)) |> List.concat in
         let shadow_multiplier_target =
           if not hvm
           then 1.
           else
             let static_max_mib = Memory.mib_of_bytes_used vm.Vm.memory_static_max in
             let default_shadow_mib = Memory.HVM.shadow_mib static_max_mib vm.Vm.vcpu_max 1. in
             let actual_shadow_mib =
               Int64.of_int (Xenctrl.shadow_allocation_get xc di.domid) in
             (Int64.to_float actual_shadow_mib) /. (Int64.to_float default_shadow_mib) in
         {
           Vm.power_state = if di.paused then Paused else Running;
           domids = [ di.domid ];
           consoles = Opt.to_list vnc @ (Opt.to_list tc) @ qemu_unix_vnc;
           uncooperative_balloon_driver = uncooperative;
           guest_agent = guest_agent;
           pv_drivers_detected = begin match vme with
             | Some x -> x.VmExtra.non_persistent.VmExtra.pv_drivers_detected
             | None -> false
           end;
           xsdata_state = xsdata_state;
           vcpu_target = begin match vme with
             | Some x -> x.VmExtra.non_persistent.VmExtra.vcpus
             | None -> 0
           end;
           memory_target = memory_target;
           memory_actual = memory_actual;
           memory_limit = memory_limit;
           rtc_timeoffset = rtc;
           last_start_time = begin match vme with
             | Some x -> x.VmExtra.persistent.VmExtra.last_start_time
             | None -> 0.
           end;
           shadow_multiplier_target = shadow_multiplier_target;
           hvm;
           nomigrate = begin match vme with
             | None   -> false
             | Some x -> x.VmExtra.persistent.VmExtra.nomigrate
           end;
           nested_virt = begin match vme with
             | None   -> false
             | Some x -> x.VmExtra.persistent.VmExtra.nested_virt
           end;
           domain_type = if hvm then Domain_HVM else Domain_PVinPVH;
         }
    )

let request_rdp vm enabled =
  let open Xenlight.Dominfo in
  let uuid = uuid_of_vm vm in
  with_xs
    (fun xs ->
       match di_of_uuid Newest uuid with
       | None, _ -> raise (Does_not_exist("domain", vm.Vm.id))
       | Some di, _ ->
         let path = Printf.sprintf "/local/domain/%d/control/ts" di.domid in
         xs.Xs.write path (if enabled then "1" else "0")
    )

let set_domain_action_request vm request =
  let open Xenlight.Dominfo in
  let uuid = uuid_of_vm vm in
  with_xs
    (fun xs ->
       match di_of_uuid Newest uuid with
       | None, _ -> raise (Does_not_exist("domain", vm.Vm.id))
       | Some di, _ ->
         Domain.set_action_request ~xs di.domid (match request with
             | None -> None
             | Some Needs_poweroff -> Some "poweroff"
             | Some Needs_reboot -> Some "reboot"
             | _ ->
               error "VM = %s; Unknown domain action requested. Will set to poweroff" vm.Vm.id;
               Some "poweroff"
           )
    )

let get_domain_action_request vm =
  let open Xenlight.Dominfo in
  let uuid = uuid_of_vm vm in
  with_xs
    (fun xs ->
       match di_of_uuid Newest uuid with
       | None, _ -> Some Needs_poweroff
       | Some d, _ ->
         if d.shutdown
         then Some (match d.shutdown_reason with
             | Xenlight.SHUTDOWN_REASON_POWEROFF -> Needs_poweroff
             | Xenlight.SHUTDOWN_REASON_REBOOT -> Needs_reboot
             | Xenlight.SHUTDOWN_REASON_SUSPEND -> Needs_suspend
             | Xenlight.SHUTDOWN_REASON_CRASH -> Needs_crashdump
             | Xenlight.SHUTDOWN_REASON_WATCHDOG -> Needs_reboot
             | _ -> Needs_poweroff) (* unexpected *)
         else begin match Domain.get_action_request ~xs d.domid with
           | Some "poweroff" -> Some Needs_poweroff
           | Some "reboot" -> Some Needs_reboot
           | Some x ->
             error "VM = %s; Unknown domain action requested (%s). Will poweroff" vm.Vm.id x;
             Some Needs_poweroff
           | None -> None
         end
    )

let get_internal_state vdi_map vif_map vm =
  let state = DB.read_exn vm.Vm.id in
  state.VmExtra.persistent |> VmExtra.rpc_of_persistent_t |> Jsonrpc.to_string

let set_internal_state vm state =
  let k = vm.Vm.id in
  let persistent = state |> Jsonrpc.of_string |> VmExtra.persistent_t_of_rpc in
  let non_persistent = match DB.read k with
    | None -> with_xs (fun xs -> generate_non_persistent_state xs vm)
    | Some vmextra -> vmextra.VmExtra.non_persistent
  in
  DB.write k { VmExtra.persistent = persistent; VmExtra.non_persistent = non_persistent; }

let minimum_reboot_delay = 120.
end

module UPDATES = struct
  let get last timeout = Updates.get "UPDATES.get" last timeout internal_updates
end

let _introduceDomain = "@introduceDomain"
let _releaseDomain = "@releaseDomain"

(* CA-76600: the rtc/timeoffset needs to be maintained over a migrate. *)
let store_rtc_timeoffset vm timeoffset =
  Opt.iter
    (function { VmExtra.persistent; non_persistent } ->
    match persistent with
    | { VmExtra.ty = Some ( Vm.HVM hvm_info ) } ->
      let persistent = { persistent with VmExtra.ty = Some (Vm.HVM { hvm_info with Vm.timeoffset = timeoffset }) } in
      debug "VM = %s; rtc/timeoffset <- %s" vm timeoffset;
      DB.write vm { VmExtra.persistent; non_persistent }
    | _ -> ()
    ) (DB.read vm)

let maybe_update_pv_drivers_detected ~xc ~xs domid path =
  let vm = get_uuid domid in
  Opt.iter
    (function { VmExtra.persistent; non_persistent } ->
      if not non_persistent.VmExtra.pv_drivers_detected then begin
        (* If the new value for this device is 4 then PV drivers are present *)
        try
          let value = xs.Xs.read path in
          if value = "4" (* connected *) then begin
            let non_persistent = { non_persistent with VmExtra.pv_drivers_detected = true } in
            debug "VM = %s; found PV driver evidence on %s (value = %s)" vm path value;
            DB.write vm { VmExtra.persistent; non_persistent }
          end
        with Xs_protocol.Enoent _ ->
          warn "Watch event on %s fired but couldn't read from it" path;
          () (* the path must have disappeared immediately after the watch fired. Let's treat this as if we never saw it. *)
      end
    ) (DB.read vm)

module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

let list_domains xc =
  let dis = with_ctx (fun ctx -> Xenlight.Dominfo.list ctx) in
  let ids = List.map (fun x -> x.Xenlight.Dominfo.domid) dis in
  List.fold_left (fun map (k, v) -> IntMap.add k v map) IntMap.empty (List.combine ids dis)


let domain_looks_different a b = match a, b with
  | None, Some _ -> true
  | Some _, None -> true
  | None, None -> false
  | Some a', Some b' ->
    let open Xenlight.Dominfo in
    a'.shutdown <> b'.shutdown
    || (a'.shutdown && b'.shutdown && (a'.shutdown_reason <> b'.shutdown_reason))

let list_different_domains a b =
  let c = IntMap.merge (fun _ a b -> if domain_looks_different a b then Some () else None) a b in
  List.map fst (IntMap.bindings c)

let all_domU_watches domid uuid =
  let open Printf in [
    sprintf "/local/domain/%d/data/updated" domid;
    sprintf "/local/domain/%d/data/ts" domid;
    sprintf "/local/domain/%d/memory/target" domid;
    sprintf "/local/domain/%d/memory/uncooperative" domid;
    sprintf "/local/domain/%d/console/vnc-port" domid;
    sprintf "/local/domain/%d/console/tc-port" domid;
    sprintf "/local/domain/%d/control" domid;
    sprintf "/local/domain/%d/device" domid;
    sprintf "/local/domain/%d/vm-data" domid;
    sprintf "/local/domain/%d/feature" domid;
    sprintf "/vm/%s/rtc/timeoffset" uuid;
  ]

let watches_of_device device =
  let interesting_backend_keys = [
    "kthread-pid";
    "tapdisk-pid";
    "shutdown-done";
    "params";
    "state";
  ] in
  let open Device_common in
  let be = device.backend.domid in
  let fe = device.frontend.domid in
  let kind = string_of_kind device.backend.kind in
  let devid = device.frontend.devid in
  List.map (fun k -> Printf.sprintf "/local/domain/%d/backend/%s/%d/%d/%s" be kind fe devid k) interesting_backend_keys

let domains = ref IntMap.empty
let watches = ref IntMap.empty
let uuids = ref IntMap.empty

let watch xs path =
  debug "xenstore watch %s" path;
  xs.Xs.watch path path

let unwatch xs path =
  try
    debug "xenstore unwatch %s" path;
    xs.Xs.unwatch path path
  with Xs_protocol.Enoent _ ->
    debug "xenstore unwatch %s threw Xb.Noent" path

let add_domU_watches xs domid uuid =
  debug "Adding watches for: domid %d" domid;
  List.iter (watch xs) (all_domU_watches domid uuid);
  uuids := IntMap.add domid uuid !uuids;
  watches := IntMap.add domid [] !watches

let remove_domU_watches xs domid =
  debug "Removing watches for: domid %d" domid;
  if IntMap.mem domid !uuids then begin
    let uuid = IntMap.find domid !uuids in
    List.iter (unwatch xs) (all_domU_watches domid uuid);
    List.iter (fun d ->
        List.iter (unwatch xs) (watches_of_device d)
      ) (try IntMap.find domid !watches with Not_found -> []);
    watches := IntMap.remove domid !watches;
    uuids := IntMap.remove domid !uuids;
  end

let cancel_domU_operations xs domid =
  (* Anyone blocked on a domain/device operation which won't happen because the domain
     	   just shutdown should be cancelled here. *)
  debug "Cancelling watches for: domid %d" domid;
  Cancel_utils.on_shutdown ~xs domid

let add_device_watch xs device =
  let open Device_common in
  debug "Adding watches for: %s" (string_of_device device);
  let domid = device.frontend.domid in
  List.iter (watch xs) (watches_of_device device);
  watches := IntMap.add domid (device :: (IntMap.find domid !watches)) !watches

let remove_device_watch xs device =
  let open Device_common in
  debug "Removing watches for: %s" (string_of_device device);
  let domid = device.frontend.domid in
  let current = IntMap.find domid !watches in
  List.iter (unwatch xs) (watches_of_device device);
  watches := IntMap.add domid (List.filter (fun x -> x <> device) current) !watches


let look_for_different_domains xc xs =
  let domains' = list_domains xc in
  let different = list_different_domains !domains domains' in
  List.iter
    (fun domid ->
       debug "Domain %d may have changed state" domid;
       (* The uuid is either in the new domains map or the old map. *)
       let di = IntMap.find domid (if IntMap.mem domid domains' then domains' else !domains) in
       let id = di.Xenlight.Dominfo.uuid |> Xenctrl_uuid.uuid_of_handle |> Uuidm.to_string in
       if domid > 0 && not (DB.exists id)
       then begin
         debug "However domain %d is not managed by us: ignoring" domid;
         if IntMap.mem domid !uuids then begin
           debug "Cleaning-up the remaining watches for: domid %d" domid;
           cancel_domU_operations xs domid;
           remove_domU_watches xs domid;
         end;
       end else begin
         ShutdownWatchers.broadcast domid;
         Updates.add (Dynamic.Vm id) internal_updates;
         (* A domain is 'running' if we know it has not shutdown *)
         let running = IntMap.mem domid domains' && (not (IntMap.find domid domains').Xenlight.Dominfo.shutdown) in
         match IntMap.mem domid !watches, running with
         | true, true -> () (* still running, nothing to do *)
         | false, false -> () (* still offline, nothing to do *)
         | false, true ->
           add_domU_watches xs domid id
         | true, false ->
           cancel_domU_operations xs domid;
           remove_domU_watches xs domid
       end
    ) different;
  domains := domains'

(* Watches are generated by concurrent activity on the system. We must decide whether
   to let them queue up in xenstored, or here. Since xenstored is more important for
   system reliability, we choose to drain its queue as quickly as possible and put the
   queue here. If this queue gets too large we should throw it away, disconnect and
   reconnect. *)
let incoming_watches = Queue.create ()
let queue_overflowed = ref false
let incoming_watches_m = Mutex.create ()
let incoming_watches_c = Condition.create ()

let enqueue_watches event =
  Mutex.execute incoming_watches_m
    (fun () ->
       if Queue.length incoming_watches = !Xenopsd.watch_queue_length
       then queue_overflowed := true
       else Queue.push event incoming_watches;
       Condition.signal incoming_watches_c
    )

exception Watch_overflow

let dequeue_watches callback =
  try
    while true do
      let event = Mutex.execute incoming_watches_m
          (fun () ->
             while Queue.is_empty incoming_watches && not(!queue_overflowed) do
               Condition.wait incoming_watches_c incoming_watches_m
             done;
             if !queue_overflowed then begin
               error "xenstore watch event queue overflow: this suggests the processing thread deadlocked somehow.";
               raise Watch_overflow;
             end;
             Queue.pop incoming_watches
          ) in
      let () = callback event in
      ()
    done
  with Watch_overflow -> ()

let process_one_watch xc xs (path, token) =
  let set_difference a b = List.fold_left (fun acc a ->
      if not(List.mem a b) then a :: acc else acc
    ) [] a in

  let look_for_different_devices domid =
    if not(IntMap.mem domid !watches)
    then debug "Ignoring frontend device watch on unmanaged domain: %d" domid
    else begin
      let devices = IntMap.find domid !watches in
      let devices' = Device_common.list_frontends ~xs domid in
      let old_devices = set_difference devices devices' in
      let new_devices = set_difference devices' devices in
      List.iter (add_device_watch xs) new_devices;
      List.iter (remove_device_watch xs) old_devices;
    end in

  let fire_event_on_vm domid =
    let d = int_of_string domid in
    if not(IntMap.mem d !domains)
    then debug "Ignoring watch on shutdown domain %d" d
    else
      let di = IntMap.find d !domains in
      let id = di.Xenlight.Dominfo.uuid |> Xenctrl_uuid.uuid_of_handle |> Uuidm.to_string in
      Updates.add (Dynamic.Vm id) internal_updates in

  let fire_event_on_device domid kind devid =
    let d = int_of_string domid in
    if not(IntMap.mem d !domains)
    then debug "Ignoring watch on shutdown domain %d" d
    else
      let di = IntMap.find d !domains in
      let id = di.Xenlight.Dominfo.uuid |> Xenctrl_uuid.uuid_of_handle |> Uuidm.to_string in
      let update = match kind with
        | "vbd" ->
          let devid' = devid |> int_of_string |> Device_number.of_xenstore_key |> Device_number.to_linux_device in
          Some (Dynamic.Vbd (id, devid'))
        | "vif" -> Some (Dynamic.Vif (id, devid))
        | x ->
          debug "Unknown device kind: '%s'" x;
          None in
      Opt.iter (fun x -> Updates.add x internal_updates) update in

  if path = _introduceDomain || path = _releaseDomain
  then look_for_different_domains xc xs
  else match List.filter (fun x -> x <> "") (Xstringext.String.split '/' path) with
    | "local" :: "domain" :: domid :: "backend" :: kind :: frontend :: devid :: key ->
      debug "Watch on backend domid: %s kind: %s -> frontend domid: %s devid: %s" domid kind frontend devid;
      fire_event_on_device frontend kind devid;
      (* If this event was a state change then this might be the first time we see evidence of PV drivers *)
      if key = ["state"] then maybe_update_pv_drivers_detected ~xc ~xs (int_of_string frontend) path
    | "local" :: "domain" :: frontend :: "device" :: _ ->
      look_for_different_devices (int_of_string frontend)
    | "local" :: "domain" :: domid :: _ ->
      fire_event_on_vm domid
    | "vm" :: uuid :: "rtc" :: "timeoffset" :: [] ->
      let timeoffset = try Some (xs.Xs.read path) with _ -> None in
      Opt.iter
        (fun timeoffset ->
           (* Store the rtc/timeoffset for migrate *)
           store_rtc_timeoffset uuid timeoffset;
           (* Tell the higher-level toolstack about this too *)
           Updates.add (Dynamic.Vm uuid) internal_updates
        ) timeoffset
    | _  -> debug "Ignoring unexpected watch: %s" path

(* Here we analyse common startup errors in more detail and
   suggest the most likely fixes (e.g. switch to root, start missing
   service) *)

let look_for_forkexec () =
  try
    let _ = run "/bin/ls" [] in
    debug "fork/exec service is responding"
  with e ->
    error "The fork/exec service is not working properly. The raw error was: %s" (Printexc.to_string e);
    error "This is a fatal error because I will not be able to start any VMs.";
    error "Please start (or restart) the fork/exec service and try again.";
    exit 1

let look_for_xen () = match detect_hypervisor () with
  | Some (Xen (major, minor)) -> major, minor
  | Some (Other x) ->
    error "You are running a different hypervisor (%s)" x;
    error "Please check your bootloader configuration, reboot to xen and try again.";
    exit 1
  | None ->
    error "The file %s does not exist: you are not running xen." _sys_hypervisor_type;
    error "Please check your bootloader configuration, reboot to xen and try again.";
    exit 1

let register_for_watches xc =
  let client = Xenstore.Client.make () in
  Xenstore.Client.immediate client
    (fun h ->
       let xs = Xenstore.Xs.ops h in
       Xenstore.Client.set_watch_callback client enqueue_watches;

       (* NB these two watches will be immediately fired so we will automatically
          			   check for new/missing domains. *)
       xs.Xs.watch _introduceDomain "";
       xs.Xs.watch _releaseDomain "";
       debug "watching for @introduceDomain and @releaseDomain";

       dequeue_watches (process_one_watch xc xs);
    )

let init () =
  look_for_forkexec ();

  let major, minor = look_for_xen () in

  if major < "4" || (major = "4" && minor < "2") && !Xenopsd.run_hotplug_scripts then begin
    error "This is xen version %s.%s. On all versions < 4.1 we must use hotplug/udev scripts" major minor;
    error "To fix this error either upgrade xen or set run_hotplug_scripts=false in xenopsd.conf";
    error "Setting run_hotplug_scripts to false so we can continue: this may cause device timeouts.";
    Xenopsd.run_hotplug_scripts := false
  end;

  if !Xenopsd.run_hotplug_scripts then begin
    with_xs
      (fun xs ->
         xs.Xs.write disable_udev_path "1";
         info "Written %s to disable the hotplug/udev scripts" disable_udev_path;
      )
  end;
  (* XXX: is this completely redundant now? The Citrix PV drivers don't need this any more *)
  (* Special XS entry looked for by the XenSource PV drivers (see xenagentd.hg:src/xad.c) *)
  let xe_key = "/mh/XenSource-TM_XenEnterprise-TM" in
  let xe_val = "XenSource(TM) and XenEnterprise(TM) are registered trademarks of XenSource Inc." in

  with_xs
    (fun xs ->
       xs.Xs.write xe_key xe_val;
       xs.Xs.setperms xe_key { Xs_protocol.ACL.owner = 0; other = Xs_protocol.ACL.READ; acl = [] }
    );

  (* Setup a libxl context *)
  Xenlight.register_exceptions ();
  let logger' = create_logger ~level:Xentoollog.Debug () in
  ctx := Some (Xenlight.ctx_alloc logger');
  logger := Some logger';

  with_ctx (fun ctx ->
      ignore (Xenlight_events.event_loop_init ctx);
      (*	Xenlight_events.E.evenable_domain_death ctx 47 666 *)
    );

  debug "xenstore is responding to requests";
  let (_: Thread.t) = Thread.create
      (fun () ->
         while true do
           finally
             (fun () ->
                debug "(re)starting xenstore watch thread";
                (* TODO: libxl *)
                with_xc register_for_watches)
             (fun () ->
                Thread.delay 5.)
         done
      ) () in
  ()

module DEBUG = struct
  open Xenlight.Dominfo

  let trigger cmd args = match cmd, args with
    | "reboot", [ k ] ->
      let uuid = uuid_of_string k in
      with_xs
        (fun xs ->
           match di_of_uuid Newest uuid with
           | None, _ -> raise (Does_not_exist("domain", k))
           | Some di, _ ->
             with_ctx (fun ctx -> Xenlight.Domain.reboot ctx di.domid)
        )
    | "halt", [ k ] ->
      let uuid = uuid_of_string k in
      with_xs
        (fun xs ->
           match di_of_uuid Newest uuid with
           | None, _ -> raise (Does_not_exist("domain", k))
           | Some di, _ ->
             with_ctx (fun ctx -> Xenlight.Domain.shutdown ctx di.domid)
        )
    | _ ->
      debug "DEBUG.trigger cmd=%s Unimplemented" cmd;
      raise (Unimplemented(cmd))
end
