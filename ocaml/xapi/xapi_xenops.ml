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

module D=Debug.Make(struct let name="xenops" end)
open D

open Network

open Stdext
open Xstringext
open Threadext
open Pervasiveext
open Fun
module XenAPI = Client.Client
module Rrdd = Rrd_client.Client
open Xenops_interface
open Xapi_xenops_queue

let rpc_of t x = Rpcmarshal.marshal t.Rpc.Types.ty x 

let check_power_state_is ~__context ~self ~expected =
  if expected <> `Running then
    Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self ~expected
  else
    (* CA-233915: only warn about unexpected power state - the check
     * is too naive to make it an assertion
    *)
    let actual = Db.VM.get_power_state ~__context ~self in
    if actual <> expected then
      warn "Potential problem: VM %s in power state '%s' when expecting '%s'"
        (Db.VM.get_uuid ~__context ~self)
        (Record_util.power_to_string expected)
        (Record_util.power_to_string actual)

let event_wait queue_name dbg ?from p =
  let finished = ref false in
  let event_id = ref from in
  let module Client = (val make_client queue_name : XENOPS) in
  while not !finished do
    let _, deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    event_id := Some next_id;
    List.iter (fun d -> if p d then finished := true) deltas;
  done

let task_ended queue_name dbg id =
  let module Client = (val make_client queue_name : XENOPS) in
  match (Client.TASK.stat dbg id).Task.state with
  | Task.Completed _
  | Task.Failed _ -> true
  | Task.Pending _ -> false

let wait_for_task queue_name dbg id =
  let module Client = (val make_client queue_name : XENOPS) in
  let finished = function
    | Dynamic.Task id' ->
      id = id' && (task_ended queue_name dbg id)
    | _ ->
      false in
  let from = Client.UPDATES.last_id dbg in
  if not(task_ended queue_name dbg id) then event_wait queue_name dbg ~from finished;
  id

let xenapi_of_xenops_power_state = function
  | Some Running -> `Running
  | Some Halted -> `Halted
  | Some Suspended -> `Suspended
  | Some Paused -> `Paused
  | None -> `Halted

let xenops_of_xenapi_power_state = function
  | `Running -> Running
  | `Halted -> Halted
  | `Suspended -> Suspended
  | `Paused -> Paused

let xenops_vdi_locator_of sr vdi =
  Printf.sprintf "%s/%s" (Storage_interface.Sr.string_of sr) (Storage_interface.Vdi.string_of vdi)

let xenops_vdi_locator ~__context ~self =
  let sr = Db.VDI.get_SR ~__context ~self in
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
  let vdi_location = Db.VDI.get_location ~__context ~self in
  xenops_vdi_locator_of (Storage_interface.Sr.of_string sr_uuid) (Storage_interface.Vdi.of_string vdi_location)

let disk_of_vdi ~__context ~self =
  try Some (VDI (xenops_vdi_locator ~__context ~self)) with _ -> None

let vdi_of_disk ~__context x = match String.split ~limit:2 '/' x with
  | [ sr_uuid; location ] ->
    let open Db_filter_types in
    let sr = Db.SR.get_by_uuid ~__context ~uuid:sr_uuid in
    begin match Db.VDI.get_records_where ~__context ~expr:(And((Eq (Field "location", Literal location)),Eq (Field "SR", Literal (Ref.string_of sr)))) with
      | x :: _ -> Some x
      | _ ->
        error "Failed to find VDI: %s" x;
        None
    end
  | _ ->
    error "Failed to parse VDI name: %s" x;
    None

let backend_of_network net =
  try
    let backend_vm = List.assoc "backend_vm" net.API.network_other_config in
    debug "Using VM %s as backend for VIF on network %s" backend_vm net.API.network_uuid;
    Network.Remote (backend_vm, net.API.network_bridge)
  with Not_found ->
    Network.Local net.API.network_bridge (* PR-1255 *)

let backend_of_vif ~__context ~vif =
  let vif_record = Db.VIF.get_record_internal ~__context ~self:vif in
  let net = Db.Network.get_record ~__context ~self:vif_record.Db_actions.vIF_network in
  let host = Helpers.get_localhost ~__context in
  let pifs = Xapi_network_attach_helpers.get_local_pifs ~__context
      ~network:vif_record.Db_actions.vIF_network ~host
  in
  match pifs with
  | [] -> backend_of_network net
  | pif :: _ ->
    let pif_rec = Db.PIF.get_record ~__context ~self:pif in
    let l = Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec in
    if List.exists (function Xapi_pif_helpers.Network_sriov_logical _ -> true | _ -> false) l then
      begin
        if vif_record.Db_actions.vIF_reserved_pci <> Ref.null then
          let (domain, bus, dev, fn) =
            Pciops.pcidev_of_pci ~__context vif_record.Db_actions.vIF_reserved_pci in
          Network.Sriov {domain; bus; dev; fn}
        else raise (Api_errors.(Server_error (internal_error,
                                              [Printf.sprintf "No reserved_pci for network SR-IOV vif %s" (Ref.string_of vif)])))
      end
    else backend_of_network net

let find f map default feature =
  try
    let v = List.assoc feature map in
    try f v
    with e ->
      warn "Failed to parse %s as value for %s: %s; Using default value."
        v feature (Printexc.to_string e);
      default
  with Not_found -> default
let string = find (fun x -> x)
let int = find int_of_string
let bool = find (function "1" -> true | "0" -> false | x -> bool_of_string x)

let nvram_uefi_of_vm vm =
  let open Xenops_types.Nvram_uefi_variables in
  let on_field name f t =
    match List.assoc name vm.API.vM_NVRAM with
    | v -> f v t
    | exception Not_found -> t
  in
  let add_on_boot =
    on_field "EFI-variables-on-boot" (fun str t -> match str with
        | "persist" -> { t with on_boot = Persist }
        | "reset" -> { t with on_boot = Reset }
        | bad ->
          raise Api_errors.(Server_error(invalid_value, [
              "NVRAM['EFI-variables-on-boot']";
              bad])))
  in
  let add_backend =
    on_field "EFI-variables-backend" (fun backend t ->
        { t with backend })
  in
  default_t
  |> add_on_boot
  |> add_backend

let firmware_of_vm vm =
  let open Xenops_types.Vm in
  match List.assoc "firmware" vm.API.vM_HVM_boot_params with
  | "bios" -> Bios
  | "uefi" -> Uefi (nvram_uefi_of_vm vm)
  | bad ->
    raise Api_errors.(Server_error(invalid_value, [
        "HVM-boot-params['firmware']";
        bad]))
  | exception Not_found -> default_firmware

let nvram_post_clone ~__context ~self ~uuid =
  match Db.VM.get_NVRAM ~__context ~self with
  | [] -> ()
  | original ->
    let uuid = Uuid.to_string uuid in
    info "VM %s was cloned: clearing certain UEFI variables" uuid;
    let (_: string*string) =
      Forkhelpers.execute_command_get_output
        !Xapi_globs.varstore_rm ["-c"; uuid] in
    if Db.VM.get_NVRAM ~__context ~self <> original then
      debug "VM %s: NVRAM changed due to clone" uuid

let rtc_timeoffset_of_vm ~__context (vm, vm_t) vbds =
  let timeoffset = string vm_t.API.vM_platform "0" Vm_platform.timeoffset in
  (* If any VDI has on_boot = reset AND has a VDI.other_config:timeoffset
     	   then we override the platform/timeoffset. This is needed because windows
     	   stores the local time in timeoffset (the BIOS clock) but records whether
     	   it has adjusted it for daylight savings in the system disk. If we reset
     	   the system disk to an earlier snapshot then the BIOS clock needs to be
     	   reset too. *)
  let non_empty_vbds = List.filter (fun vbd -> not vbd.API.vBD_empty) vbds in
  let vdis = List.map (fun vbd -> vbd.API.vBD_VDI) non_empty_vbds in
  let vdis_with_timeoffset_to_be_reset_on_boot =
    vdis
    |> List.map (fun self -> (self, Db.VDI.get_record ~__context ~self))
    |> List.filter (fun (_, record) -> record.API.vDI_on_boot = `reset)
    |> Listext.List.filter_map (fun (reference, record) ->
        Opt.of_exception (fun () ->
            reference,
            List.assoc Vm_platform.timeoffset
              record.API.vDI_other_config)) in
  match vdis_with_timeoffset_to_be_reset_on_boot with
  | [] ->
    timeoffset
  | [(reference, timeoffset)] ->
    timeoffset
  | reference_timeoffset_pairs ->
    raise (Api_errors.Server_error (
        (Api_errors.vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot),
        (Ref.string_of vm) ::
        (reference_timeoffset_pairs
         |> List.map fst
         |> List.map Ref.string_of)))

(* /boot/ contains potentially sensitive files like xen-initrd, so we will only*)
(* allow directly booting guests from the subfolder /boot/guest/ *)
let allowed_dom0_directory_for_boot_files = "/boot/guest/"
let is_boot_file_whitelisted filename =
  let safe_str str = not (String.has_substr str "..") in
  (* make sure the script prefix is the allowed dom0 directory *)
  (String.startswith allowed_dom0_directory_for_boot_files filename)
  (* avoid ..-style attacks and other weird things *)
  &&(safe_str filename)

let builder_of_vm ~__context (vmref, vm) timeoffset pci_passthrough vgpu =
  let open Vm in

  let video_mode =
    if vgpu then Vgpu
    else if (Vm_platform.is_true
               ~key:Vm_platform.igd_passthru_key
               ~platformdata:vm.API.vM_platform
               ~default:false)
    then (IGD_passthrough GVT_d)
    else
      match string vm.API.vM_platform "cirrus" Vm_platform.vga with
      | "std" -> Standard_VGA
      | "cirrus" -> Cirrus
      | x ->
        error "Unknown platform/vga option: %s (expected 'std' or 'cirrus')" x;
        Cirrus
  in

  let pci_emulations =
    let s = try Some (List.assoc "mtc_pci_emulations" vm.API.vM_other_config) with _ -> None in
    match s with
    | None -> []
    | Some x ->
      try
        let l = String.split ',' x in
        List.map (String.strip String.isspace) l
      with _ -> []
  in

  let make_hvmloader_boot_record { Helpers.timeoffset = t } =
    {
      hap = true;
      shadow_multiplier = vm.API.vM_HVM_shadow_multiplier;
      timeoffset = timeoffset;
      video_mib = begin
        (* For vGPU, make sure videoram is at least 16MiB. *)
        let requested_videoram = int vm.API.vM_platform 4 "videoram" in
        if video_mode = Vgpu
        then max requested_videoram 16
        else requested_videoram
      end;
      video = video_mode;
      acpi = bool vm.API.vM_platform true "acpi";
      serial = begin
        (* The platform value should override the other_config value. If
           				 * neither are set, use pty. *)
        let key = "hvm_serial" in
        let other_config_value =
          try Some (List.assoc key vm.API.vM_other_config)
          with Not_found -> None
        in
        let platform_value =
          try Some (List.assoc key vm.API.vM_platform)
          with Not_found -> None
        in
        match other_config_value, platform_value with
        | None, None -> Some "pty"
        | _, Some value -> Some value
        | Some value, None -> Some value
      end;
      keymap = begin
        try Some (List.assoc "keymap" vm.API.vM_platform)
        with Not_found -> None
      end;
      vnc_ip = None (*None PR-1255*);
      pci_emulations = pci_emulations;
      pci_passthrough = pci_passthrough;
      boot_order = string vm.API.vM_HVM_boot_params "cd" "order";
      qemu_disk_cmdline = bool vm.API.vM_platform false "qemu_disk_cmdline";
      qemu_stubdom = bool vm.API.vM_platform false "qemu_stubdom";
      firmware = firmware_of_vm vm;
    }
  in

  let make_direct_boot_record { Helpers.kernel = k; kernel_args = ka; ramdisk = initrd } =
    let k = if is_boot_file_whitelisted k then k else begin
        debug "kernel %s is not in the whitelist: ignoring" k;
        ""
      end in
    let initrd = Opt.map (fun x ->
        if is_boot_file_whitelisted x then x else begin
          debug "initrd %s is not in the whitelist: ignoring" k;
          ""
        end
      ) initrd in
    {
      boot = Direct { kernel = k; cmdline = ka; ramdisk = initrd };
      framebuffer = bool vm.API.vM_platform false "pvfb";
      framebuffer_ip = None; (* None PR-1255 *)
      vncterm = begin match List.mem_assoc "disable_pv_vnc" vm.API.vM_other_config with
        |true -> false
        |false -> true
      end;
      vncterm_ip = None (*None PR-1255*);
    }
  in

  let make_indirect_boot_record { Helpers.bootloader = b; extra_args = e; legacy_args = l; pv_bootloader_args = p; vdis = vdis } =
    {
      boot = Indirect { bootloader = b; extra_args = e; legacy_args = l; bootloader_args = p; devices = Listext.List.filter_map (fun x -> disk_of_vdi ~__context ~self:x) vdis };
      framebuffer = bool vm.API.vM_platform false "pvfb";
      framebuffer_ip = None; (* None PR-1255 *)
      vncterm = begin match List.mem_assoc "disable_pv_vnc" vm.API.vM_other_config with
        |true -> false
        |false -> true
      end;
      vncterm_ip = None (*None PR-1255*);
    }
  in
  match Helpers.(check_domain_type vm.API.vM_domain_type, boot_method_of_vm ~__context ~vm) with
  | `hvm,       Helpers.Hvmloader options -> HVM (make_hvmloader_boot_record options)
  | `pv,        Helpers.Direct options ->    PV (make_direct_boot_record options)
  | `pv,        Helpers.Indirect options ->  PV (make_indirect_boot_record options)
  | `pv_in_pvh, Helpers.Direct options ->    PVinPVH (make_direct_boot_record options)
  | `pv_in_pvh, Helpers.Indirect options ->  PVinPVH (make_indirect_boot_record options)
  | _ -> raise Api_errors.(Server_error (internal_error, ["invalid boot configuration"]))

let list_net_sriov_vf_pcis ~__context ~vm =
  vm.API.vM_VIFs
  |> List.filter (fun self -> Db.VIF.get_currently_attached ~__context ~self)
  |> Listext.List.filter_map (fun vif ->
      match backend_of_vif ~__context ~vif with
      | Network.Sriov {domain; bus; dev; fn} -> Some (domain, bus, dev, fn)
      | _ -> None
    )

module MD = struct
  (** Convert between xapi DB records and xenopsd records *)

  let of_vbd ~__context ~vm ~vbd =
    let hvm = match vm.API.vM_domain_type with
      | `hvm -> true
      | `pv_in_pvh
      | `pv
      | `unspecified -> false
    in
    let device_number = Device_number.of_string hvm vbd.API.vBD_userdevice in
    let open Vbd in
    let ty = vbd.API.vBD_qos_algorithm_type in
    let params = vbd.API.vBD_qos_algorithm_params in

    let qos_class params =
      if List.mem_assoc "class" params then
        match List.assoc "class" params with
        | "highest" -> Highest
        | "high"    -> High
        | "normal"  -> Normal
        | "low"     -> Low
        | "lowest"  -> Lowest
        | s         ->
          try Other (int_of_string s)
          with _ ->
            warn "Unknown VBD QoS scheduler class (try 'high' 'low' 'normal')";
            Normal
      else
        Normal in
    let qos_scheduler params =
      try
        match List.assoc "sched" params with
        | "rt" | "real-time" -> RealTime (qos_class params)
        | "idle"             -> Idle
        | "best-effort"      -> BestEffort (qos_class params)
        | _                  ->
          warn "Unknown VBD QoS scheduler (try 'real-time' 'idle' 'best-effort')";
          BestEffort (qos_class params)
      with Not_found ->
        BestEffort (qos_class params) in
    let qos = function
      | "ionice" -> Some (Ionice (qos_scheduler params))
      | "" -> None
      | x ->
        warn "Unknown VBD QoS type: %s (try 'ionice')" x;
        None in

    let other_config_keys ?(default=None) key =
      let oc = vbd.API.vBD_other_config in
      let k = key in
      try
        let v = List.assoc k oc in
        [(k, v)]
      with Not_found -> match default with None->[] | Some x->[(k, x)]
    in

    let in_range ~min ~max ~fallback values =
      List.map (fun (k,v)-> k,
                            let value = try int_of_string v
                              with _->
                                debug "%s: warning: value %s is not an integer. Using fallback value %d" k v fallback;
                                fallback
                            in
                            string_of_int (
                              if value < min then min
                              else if value > max then max
                              else value
                            )
               )
        values
    in

    let backend_kind_keys = other_config_keys Xapi_globs.vbd_backend_key in
    let poll_duration_keys = in_range ~min:0 ~max:max_int
        ~fallback:0 (* if user provides invalid integer, use 0 = disable polling *)
        (other_config_keys Xapi_globs.vbd_polling_duration_key ~default:(Some (string_of_int !Xapi_globs.default_vbd3_polling_duration)))
    in
    let poll_idle_threshold_keys = in_range ~min:0 ~max:100
        ~fallback:50 (* if user provides invalid float, use 50 = default 50% *)
        (other_config_keys Xapi_globs.vbd_polling_idle_threshold_key ~default:(Some (string_of_int !Xapi_globs.default_vbd3_polling_idle_threshold)))
    in

    let backend_of_vbd vbd =
      let vbd_oc  = vbd.API.vBD_other_config in
      if List.mem_assoc Xapi_globs.vbd_backend_local_key vbd_oc then
        let path = List.assoc Xapi_globs.vbd_backend_local_key vbd_oc in
        warn "Using local override for VBD backend: %s -> %s" vbd.API.vBD_uuid path;
        Some (Local path)
      else disk_of_vdi ~__context ~self:vbd.API.vBD_VDI
    in

    {
      id = (vm.API.vM_uuid, Device_number.to_linux_device device_number);
      position = Some device_number;
      mode = if vbd.API.vBD_mode = `RO then ReadOnly else ReadWrite;
      backend = backend_of_vbd vbd;
      ty = (match vbd.API.vBD_type with
          | `Disk -> Disk
          | `CD -> CDROM
          | `Floppy -> Floppy);
      unpluggable = vbd.API.vBD_unpluggable;
      extra_backend_keys = backend_kind_keys @ poll_duration_keys @ poll_idle_threshold_keys;
      extra_private_keys = [];
      qos = qos ty;
      persistent = (try Db.VDI.get_on_boot ~__context ~self:vbd.API.vBD_VDI = `persist with _ -> true);
    }

  let of_pvs_proxy ~__context vif proxy =
    let site = Db.PVS_proxy.get_site ~__context ~self:proxy in
    let site_uuid = Db.PVS_site.get_uuid ~__context ~self:site in
    let servers = Db.PVS_site.get_servers ~__context ~self:site in
    let servers =
      List.map (fun server ->
          let rc = Db.PVS_server.get_record ~__context ~self:server in
          {
            Vif.PVS_proxy.addresses = rc.API.pVS_server_addresses;
            first_port = Int64.to_int rc.API.pVS_server_first_port;
            last_port = Int64.to_int rc.API.pVS_server_last_port;
          }
        ) servers
    in
    let interface = Pvs_proxy_control.proxy_port_name vif in
    site_uuid, servers, interface

  let of_vif ~__context ~vm ~vif:(vif_ref, vif) =
    let net = Db.Network.get_record ~__context ~self:vif.API.vIF_network in
    let net_mtu = Int64.to_int (net.API.network_MTU) in
    let mtu =
      try
        if List.mem_assoc "mtu" vif.API.vIF_other_config
        then List.assoc "mtu" vif.API.vIF_other_config |> int_of_string
        else net_mtu
      with _ ->
        error "Failed to parse VIF.other_config:mtu; defaulting to network.mtu";
        net_mtu in
    let qos_type = vif.API.vIF_qos_algorithm_type in
    let qos_params = vif.API.vIF_qos_algorithm_params in
    let log_qos_failure reason =
      warn "vif QoS failed: %s (vm=%s,vif=%s)" reason vm.API.vM_uuid vif.API.vIF_uuid in
    let rate = match qos_type with
      | "ratelimit" ->
        let timeslice =
          try Int64.of_string (List.assoc "timeslice_us" qos_params)
          with _ -> 0L in
        begin
          try
            let rate = Int64.of_string (List.assoc "kbps" qos_params) in
            Some (rate, timeslice)
          with
          | Failure _ (* int_of_string *) ->
            log_qos_failure "parameter \"kbps\" not an integer"; None
          | Not_found ->
            log_qos_failure "necessary parameter \"kbps\" not found"; None
          | e ->
            log_qos_failure (Printf.sprintf "unexpected error: %s" (Printexc.to_string e)); None
        end
      | "" -> None
      | _ -> log_qos_failure (Printf.sprintf "unknown type: %s" qos_type); None in
    let locking_mode = match vif.API.vIF_locking_mode, net.API.network_default_locking_mode with
      | `network_default, `disabled -> Vif.Disabled
      | `network_default, `unlocked -> Vif.Unlocked
      | `locked, _ -> Vif.Locked { Vif.ipv4 = vif.API.vIF_ipv4_allowed; ipv6 = vif.API.vIF_ipv6_allowed }
      | `unlocked, _ -> Vif.Unlocked
      | `disabled, _ -> Vif.Disabled in
    let host = Helpers.get_localhost ~__context in
    let pifs = Xapi_network_attach_helpers.get_local_pifs ~__context ~network:vif.API.vIF_network ~host in
    let carrier =
      if !Xapi_globs.pass_through_pif_carrier then
        (* We need to reflect the carrier of the local PIF on the network (if any) *)
        match pifs with
        | [] -> true (* Internal network; consider as "always up" *)
        | pif :: _ ->
          try
            let metrics = Db.PIF.get_metrics ~__context ~self:pif in
            Db.PIF_metrics.get_carrier ~__context ~self:metrics
          with _ -> true
      else
        (* If we don't need to reflect anything, the carrier is set to "true" *)
        true
    in
    let ipv4_configuration =
      match vif.API.vIF_ipv4_configuration_mode with
      | `None -> Vif.Unspecified4
      | `Static ->
        let gateway = if vif.API.vIF_ipv4_gateway = "" then None else Some vif.API.vIF_ipv4_gateway in
        Vif.Static4 (vif.API.vIF_ipv4_addresses, gateway)
    in
    let ipv6_configuration =
      match vif.API.vIF_ipv6_configuration_mode with
      | `None -> Vif.Unspecified6
      | `Static ->
        let gateway = if vif.API.vIF_ipv6_gateway = "" then None else Some vif.API.vIF_ipv6_gateway in
        Vif.Static6 (vif.API.vIF_ipv6_addresses, gateway)
    in
    let extra_private_keys =
      [
        "vif-uuid", vif.API.vIF_uuid;
        "network-uuid", net.API.network_uuid;
      ]
    in
    let pvs_proxy =
      Opt.map
        (of_pvs_proxy ~__context vif)
        (Pvs_proxy_control.find_proxy_for_vif ~__context ~vif:vif_ref)
    in
    let vlan = match pifs with
      | [] -> None
      | pif :: _ ->
        let vlan = Db.PIF.get_VLAN ~__context ~self:pif in
        if vlan < 0L then None else Some vlan
    in
    {
      Vif.id = (vm.API.vM_uuid, vif.API.vIF_device);
      position = int_of_string vif.API.vIF_device;
      mac = vif.API.vIF_MAC;
      carrier = carrier;
      mtu = mtu;
      rate = rate;
      backend = backend_of_vif ~__context ~vif:vif_ref;
      other_config = vif.API.vIF_other_config;
      locking_mode = locking_mode;
      extra_private_keys;
      ipv4_configuration = ipv4_configuration;
      ipv6_configuration = ipv6_configuration;
      pvs_proxy;
      vlan = vlan;
    }

  let pcis_of_vm ~__context (vmref, vm) =
    let vgpu_pcidevs = Vgpuops.list_pcis_for_passthrough ~__context ~vm:vmref in
    let devs = List.flatten (List.map (fun (_, dev) -> dev) (Pciops.sort_pcidevs vgpu_pcidevs)) in

    (* The 'unmanaged' PCI devices are in the other_config key: *)
    let other_pcidevs = Pciops.other_pcidevs_of_vm ~__context vm.API.vM_other_config in

    let unmanaged = List.flatten (List.map (fun (_, dev) -> dev) (Pciops.sort_pcidevs other_pcidevs)) in

    let net_sriov_pcidevs = list_net_sriov_vf_pcis ~__context ~vm in

    let devs = devs @ net_sriov_pcidevs @ unmanaged in

    let open Pci in
    List.map
      (fun (idx, (domain, bus, dev, fn)) -> {
           id = (vm.API.vM_uuid, Printf.sprintf "%04x:%02x:%02x.%01x" domain bus dev fn);
           position = idx;
           address = {domain; bus; dev; fn};
           msitranslate = None;
           power_mgmt = None;
         })
      (List.combine (Range.to_list (Range.make 0 (List.length devs))) devs)

  let get_target_pci_address ~__context vgpu =
    let pgpu =
      if Db.is_valid_ref __context
          vgpu.Db_actions.vGPU_scheduled_to_be_resident_on
      then vgpu.Db_actions.vGPU_scheduled_to_be_resident_on
      else vgpu.Db_actions.vGPU_resident_on
    in
    let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
    let pci_address = Db.PCI.get_pci_id ~__context ~self:pci in
    Xenops_interface.Pci.address_of_string pci_address

  let of_nvidia_vgpu ~__context vm vgpu =
    let open Vgpu in
    (* Get the PCI address. *)
    let physical_pci_address = get_target_pci_address ~__context vgpu in
    (* Get the vGPU config. *)
    let vgpu_type = vgpu.Db_actions.vGPU_type in
    let internal_config =
      Db.VGPU_type.get_internal_config ~__context ~self:vgpu_type in
    let config_file =
      try List.assoc Xapi_globs.vgpu_config_key internal_config
      with Not_found -> failwith "NVIDIA vGPU config file not specified"
    in
    let config_file =
      try
        let extra_args =
          List.assoc Xapi_globs.vgpu_extra_args_key vm.API.vM_platform in
        Printf.sprintf "%s,%s" config_file extra_args
      with Not_found -> config_file
    in
    let implementation =
      Nvidia {
        physical_pci_address = None; (* unused *)
        config_file;
      }
    in {
      id = (vm.API.vM_uuid, vgpu.Db_actions.vGPU_device);
      position = int_of_string vgpu.Db_actions.vGPU_device;
      physical_pci_address;
      implementation;
    }

  let of_gvt_g_vgpu ~__context vm vgpu =
    let open Vgpu in
    (* Get the PCI address. *)
    let physical_pci_address = get_target_pci_address ~__context vgpu in
    (* Get the vGPU config. *)
    let vgpu_type = vgpu.Db_actions.vGPU_type in
    let internal_config =
      Db.VGPU_type.get_internal_config ~__context ~self:vgpu_type in
    try
      let implementation =
        GVT_g {
          physical_pci_address = None; (* unused *)
          low_gm_sz =
            List.assoc Xapi_globs.vgt_low_gm_sz internal_config
            |> Int64.of_string;
          high_gm_sz =
            List.assoc Xapi_globs.vgt_high_gm_sz internal_config
            |> Int64.of_string;
          fence_sz =
            List.assoc Xapi_globs.vgt_fence_sz internal_config
            |> Int64.of_string;
          monitor_config_file =
            if List.mem_assoc Xapi_globs.vgt_monitor_config_file internal_config
            then Some
                (List.assoc Xapi_globs.vgt_monitor_config_file internal_config)
            else None;
        }
      in {
        id = (vm.API.vM_uuid, vgpu.Db_actions.vGPU_device);
        position = int_of_string vgpu.Db_actions.vGPU_device;
        physical_pci_address;
        implementation;
      }
    with
    | Not_found -> failwith "Intel GVT-g settings not specified"
    | Failure _ (* int_of_string *)-> failwith "Intel GVT-g settings invalid"

  let of_mxgpu_vgpu ~__context vm vgpu =
    let open Vgpu in
    (* Get the PCI address. *)
    let physical_pci_address = get_target_pci_address ~__context vgpu in
    let vgpu_type = vgpu.Db_actions.vGPU_type in
    let internal_config =
      Db.VGPU_type.get_internal_config ~__context ~self:vgpu_type in
    let framebufferbytes =
      Db.VGPU_type.get_framebuffer_size ~__context ~self:vgpu_type in
    try
      let implementation =
        MxGPU {
          physical_function = None; (* unused *)
          vgpus_per_pgpu =
            List.assoc Xapi_globs.mxgpu_vgpus_per_pgpu internal_config
            |> Int64.of_string;
          framebufferbytes;
        }
      in {
        id = (vm.API.vM_uuid, vgpu.Db_actions.vGPU_device);
        position = int_of_string vgpu.Db_actions.vGPU_device;
        physical_pci_address;
        implementation;
      }
    with
    | Not_found -> failwith "AMD MxGPU settings not specified"
    | Failure _ (* int_of_string *) -> failwith "AMD MxGPU settings invalid"

  let vgpus_of_vm ~__context (vmref, vm) =
    let open Vgpu in
    if Vgpuops.vgpu_manual_setup_of_vm vm
    && (List.mem_assoc Vm_platform.vgpu_pci_id vm.API.vM_platform)
    && (List.mem_assoc Vm_platform.vgpu_config vm.API.vM_platform)
    then begin
      (* We're using the vGPU manual setup mode, so get the vGPU configuration
         			 * from the VM platform keys. *)
      let implementation =
        Nvidia {
          physical_pci_address = None; (* unused *)
          config_file = List.assoc Vm_platform.vgpu_config vm.API.vM_platform;
        }
      in [{
          id = (vm.API.vM_uuid, "0");
          position = 0;
          physical_pci_address =
            Xenops_interface.Pci.address_of_string
              (List.assoc Vm_platform.vgpu_pci_id vm.API.vM_platform);
          implementation;
        }]
    end else
      List.fold_left
        (fun acc vgpu ->
           let vgpu_record = Db.VGPU.get_record_internal ~__context ~self:vgpu in
           let implementation =
             Db.VGPU_type.get_implementation ~__context
               ~self:vgpu_record.Db_actions.vGPU_type
           in
           match implementation with
           (* Passthrough VGPUs are dealt with in pcis_of_vm. *)
           | `passthrough -> acc
           | `nvidia ->
             (of_nvidia_vgpu ~__context vm vgpu_record) :: acc
           | `gvt_g ->
             (of_gvt_g_vgpu ~__context vm vgpu_record) :: acc
           | `mxgpu ->
             (of_mxgpu_vgpu ~__context vm vgpu_record) :: acc
        )
        [] vm.API.vM_VGPUs

  let of_vusb ~__context ~vm ~pusb =
    let open Vusb in
    try
      let path = pusb.API.pUSB_path in
      let pathList= Xstringext.String.split '-' path in
      let hostbus = List.nth pathList 0 in
      let hostport = List.nth pathList 1 in
      (* Here version can be 1.10/2.00/3.00. *)
      let version = pusb.API.pUSB_version in
      {
        id = (vm.API.vM_uuid, "vusb"^path);
        hostbus = hostbus;
        hostport = hostport;
        version = version;
        path = path;
      }
    with
    | e ->
      error "Caught %s: while getting PUSB path %s" (Printexc.to_string e) pusb.API.pUSB_path;
      raise e

  let vusbs_of_vm ~__context (vmref, vm) =
    vm.API.vM_VUSBs
    |> List.map (fun self -> Db.VUSB.get_record ~__context ~self)
    |> List.filter (fun self -> self.API.vUSB_currently_attached)
    |> List.map (fun self -> self.API.vUSB_USB_group)
    |> List.map (fun usb_group -> Helpers.get_first_pusb ~__context usb_group)
    |> List.map (fun self -> Db.PUSB.get_record ~__context ~self)
    |> List.map (fun pusb -> of_vusb ~__context ~vm ~pusb)

  let of_vm ~__context (vmref, vm) vbds pci_passthrough vgpu =
    let on_crash_behaviour = function
      | `preserve -> [ Vm.Pause ]
      | `coredump_and_restart -> [ Vm.Coredump; Vm.Start ]
      | `coredump_and_destroy -> [ Vm.Coredump; Vm.Shutdown ]
      | `restart
      | `rename_restart -> [ Vm.Start ]
      | `destroy -> [ Vm.Shutdown ] in
    let on_normal_exit_behaviour = function
      | `restart -> [ Vm.Start ]
      | `destroy -> [ Vm.Shutdown ] in
    let open Vm in
    let scheduler_params =
      (* vcpu <-> pcpu affinity settings are stored here.
         			   Format is either:
         			   1,2,3         ::  all vCPUs receive this mask
         			   1,2,3; 4,5,6  ::  vCPU n receives mask n. Unlisted vCPUs
         			                     receive first mask *)
      let affinity =
        try
          List.map
            (fun x -> List.map int_of_string (String.split ',' x))
            (String.split ';' (List.assoc "mask" vm.API.vM_VCPUs_params))
        with _ -> [] in
      let localhost = Helpers.get_localhost ~__context in
      let host_guest_VCPUs_params = Db.Host.get_guest_VCPUs_params ~__context ~self:localhost in
      let host_cpu_mask =
        try
          List.map int_of_string (String.split ',' (List.assoc "mask" host_guest_VCPUs_params))
        with _ -> [] in
      let affinity =
        match affinity,host_cpu_mask with
        | [],[] -> []
        | [],h -> [h]
        | v,[] -> v
        | affinity,mask ->
          List.map
            (fun vcpu_affinity ->
               List.filter (fun x -> List.mem x mask) vcpu_affinity) affinity in
      let priority =
        let weight =
          let default=256 in
          try
            let weight = List.assoc "weight" vm.API.vM_VCPUs_params in
            int_of_string weight
          with
          | Not_found -> default
          | e ->  error "%s" (Printexc.to_string e);
            debug "Could not parse weight value. Setting it to default value %d." default; default in
        let cap =
          let default=0 in
          try
            let cap = List.assoc "cap" vm.API.vM_VCPUs_params in
            int_of_string cap
          with
          | Not_found -> default
          | e ->  error "%s" (Printexc.to_string e);
            debug "Could not parse cap value. Setting it to default value %d." default; default in
        Some ( weight , cap ) in
      { priority = priority; affinity = affinity } in

    let platformdata =
      Vm_platform.sanity_check
        ~platformdata:vm.API.vM_platform
        ~firmware:(firmware_of_vm vm)
        ~vcpu_max:vm.API.vM_VCPUs_max
        ~vcpu_at_startup:vm.API.vM_VCPUs_at_startup
        ~domain_type:(Helpers.check_domain_type vm.API.vM_domain_type)
        ~filter_out_unknowns:
          (not(Pool_features.is_enabled ~__context Features.No_platform_filter))
    in
    (* Replace the timeoffset in the platform data too, to avoid confusion *)
    let timeoffset = rtc_timeoffset_of_vm ~__context (vmref, vm) vbds in
    let platformdata =
      (Vm_platform.timeoffset, timeoffset) ::
      (List.filter (fun (key, _) -> key <> Vm_platform.timeoffset) platformdata) in
    let platformdata =
      let genid = match vm.API.vM_generation_id with
        | "0:0" -> Xapi_vm_helpers.vm_fresh_genid ~__context ~self:vmref
        | _ -> vm.API.vM_generation_id in
      (Vm_platform.generation_id, genid) :: platformdata
    in
    (* Add the CPUID feature set for the VM to the platform data. *)
    let platformdata =
      if not (List.mem_assoc Vm_platform.featureset platformdata) then
        let featureset =
          if List.mem_assoc Xapi_globs.cpu_info_features_key vm.API.vM_last_boot_CPU_flags then
            List.assoc Xapi_globs.cpu_info_features_key vm.API.vM_last_boot_CPU_flags
          else
            failwith "VM's CPU featureset not initialised"
        in
        (Vm_platform.featureset, featureset) :: platformdata
      else
        platformdata
    in

    let pci_msitranslate = true in (* default setting *)
    (* CA-55754: allow VM.other_config:msitranslate to override the bus-wide setting *)
    let pci_msitranslate =
      if List.mem_assoc "msitranslate" vm.API.vM_other_config
      then List.assoc "msitranslate" vm.API.vM_other_config = "1"
      else pci_msitranslate in
    (* CA-55754: temporarily disable msitranslate when GPU is passed through. *)
    let pci_msitranslate =
      if vm.API.vM_VGPUs <> [] then false else pci_msitranslate in

    {
      id = vm.API.vM_uuid;
      name = vm.API.vM_name_label;
      ssidref = 0l;
      xsdata = vm.API.vM_xenstore_data;
      platformdata = platformdata;
      bios_strings = vm.API.vM_bios_strings;
      ty = builder_of_vm ~__context (vmref, vm) timeoffset pci_passthrough vgpu;
      suppress_spurious_page_faults = (try List.assoc "suppress-spurious-page-faults" vm.API.vM_other_config = "true" with _ -> false);
      machine_address_size = (try Some(int_of_string (List.assoc "machine-address-size" vm.API.vM_other_config)) with _ -> None);
      memory_static_max = vm.API.vM_memory_static_max;
      memory_dynamic_max = vm.API.vM_memory_dynamic_max;
      memory_dynamic_min = vm.API.vM_memory_dynamic_min;
      vcpu_max = Int64.to_int vm.API.vM_VCPUs_max;
      vcpus = Int64.to_int vm.API.vM_VCPUs_at_startup;
      scheduler_params = scheduler_params;
      on_crash = on_crash_behaviour vm.API.vM_actions_after_crash;
      on_shutdown = on_normal_exit_behaviour vm.API.vM_actions_after_shutdown;
      on_reboot = on_normal_exit_behaviour vm.API.vM_actions_after_reboot;
      pci_msitranslate = pci_msitranslate;
      pci_power_mgmt = false;
      has_vendor_device = vm.API.vM_has_vendor_device
    }


end

open Xenops_interface
open Fun

module Guest_agent_features = struct
  module Xapi = struct
    let auto_update_enabled = "auto_update_enabled"
    let auto_update_url = "auto_update_url"
  end

  module Xenopsd = struct
    let auto_update_enabled = "enabled"
    let auto_update_url = "update_url"

    let enabled = "1"
    let disabled = "0"
  end

  let auto_update_parameters_of_config config =
    let auto_update_enabled =
      match
        if List.mem_assoc Xapi.auto_update_enabled config
        then Some
            (* bool_of_string should be safe as the setter in xapi_pool.ml only
               					 * allows "true" or "false" to be put into the database. *)
            (bool_of_string (List.assoc Xapi.auto_update_enabled config))
        else None
      with
      | Some true ->  [Xenopsd.auto_update_enabled, Xenopsd.enabled]
      | Some false -> [Xenopsd.auto_update_enabled, Xenopsd.disabled]
      | None -> []
    in
    let auto_update_url =
      if List.mem_assoc Xapi.auto_update_url config
      then [Xenopsd.auto_update_url, List.assoc Xapi.auto_update_url config]
      else []
    in
    auto_update_enabled @ auto_update_url

  let of_config ~__context config =
    let open Features in
    let vss =
      let name = Features.name_of_feature VSS in
      let licensed = Pool_features.is_enabled ~__context VSS in
      let parameters = [] in
      Host.({
          name;
          licensed;
          parameters;
        })
    in
    let guest_agent_auto_update =
      let name = Features.name_of_feature Guest_agent_auto_update in
      let licensed =
        Pool_features.is_enabled ~__context Guest_agent_auto_update in
      let parameters = auto_update_parameters_of_config config in
      Host.({
          name;
          licensed;
          parameters;
        })
    in
    [vss; guest_agent_auto_update]
end

let apply_guest_agent_config ~__context config =
  let dbg = Context.string_of_task __context in
  let features = Guest_agent_features.of_config ~__context config in
  let module Client = (val make_client (default_xenopsd ()): XENOPS) in
  Client.HOST.update_guest_agent_features dbg features

(* Create an instance of Metadata.t, suitable for uploading to the xenops service *)
let create_metadata ~__context ~self =
  let vm = Db.VM.get_record ~__context ~self in
  let vbds = List.filter (fun vbd -> vbd.API.vBD_currently_attached)
      (List.map (fun self -> Db.VBD.get_record ~__context ~self) vm.API.vM_VBDs) in
  let vbds' = List.map (fun vbd -> MD.of_vbd ~__context ~vm ~vbd) vbds in
  let vifs = List.filter (fun (_, vif) -> vif.API.vIF_currently_attached)
      (List.map (fun self -> self, Db.VIF.get_record ~__context ~self) vm.API.vM_VIFs) in
  let vifs' = List.map (fun vif -> MD.of_vif ~__context ~vm ~vif) vifs in
  let pcis = MD.pcis_of_vm ~__context (self, vm) in
  let vgpus = MD.vgpus_of_vm ~__context (self, vm) in
  let vusbs = MD.vusbs_of_vm ~__context (self, vm) in
  let domains =
    (* For suspended VMs, the last_booted_record contains the "live" xenopsd state. *)
    if vm.API.vM_power_state = `Suspended then
      Some vm.API.vM_last_booted_record
    else
      None
  in
  let open Metadata in {
    vm = MD.of_vm ~__context (self, vm) vbds (pcis <> []) (vgpus <> []);
    vbds = vbds';
    vifs = vifs';
    pcis = pcis;
    vgpus = vgpus;
    vusbs = vusbs;
    domains = domains
  }

let id_of_vm ~__context ~self = Db.VM.get_uuid ~__context ~self
let vm_of_id ~__context uuid = Db.VM.get_by_uuid ~__context ~uuid

let vm_exists_in_xenopsd queue_name dbg id =
  let module Client = (val make_client queue_name : XENOPS) in
  Client.VM.exists dbg id

let string_of_exn = function
  | Api_errors.Server_error(code, params) -> Printf.sprintf "%s [ %s ]" code (String.concat "; " params)
  | e -> Printexc.to_string e

(* Serialise updates to the metadata caches *)
let metadata_m = Mutex.create ()

module Xapi_cache = struct
  (** Keep a cache of the "xenops-translation" of XenAPI VM configuration,
      		updated whenever we receive an event from xapi. *)

  let cache = Hashtbl.create 10 (* indexed by Vm.id *)
  let mutex = Mutex.create ()
  let with_lock f =
    Mutex.execute mutex f

  let register id initial_value =
    debug "xapi_cache: creating cache for %s" id;
    with_lock (fun () ->
        match Hashtbl.find_opt cache id with
        | Some (Some _) ->
          (* don't change if we already have a valid cached entry *)
          ()
        | None | Some None ->
          (* we do not have a cache entry for this id,
           * or we have only an empty cache entry *)
          Hashtbl.replace cache id initial_value)

  let unregister id =
    debug "xapi_cache: deleting cache for %s" id;
    with_lock (fun () ->
        Hashtbl.remove cache id)

  let update_if_changed id newvalue =
    let updated = with_lock (fun () ->
        match Hashtbl.find_opt cache id with
        | Some (Some old) when old = newvalue ->
          (* the value did not change: tell the caller it has no action to take *)
          false
        | _ ->
          Hashtbl.replace cache id (Some newvalue);
          (* We either did not have a value before, or we had a different one:
           * tell the caller that it needs to perform an update *)
          true) in
    debug "xapi_cache:%s updating cache for %s" (if updated then "" else " not") id;
    updated

  let list () =
    with_lock (fun () ->
        Hashtbl.fold (fun id _ acc -> id :: acc) cache [])
end

module Xenops_cache = struct
  (** Remember the last events received from xenopsd so we can compute
      		field-level differences. This allows us to minimise the number of
      		database writes we issue upwards. *)

  type t = {
    vm: Vm.state option;
    vbds: (Vbd.id * Vbd.state) list;
    vifs: (Vif.id * Vif.state) list;
    pcis: (Pci.id * Pci.state) list;
    vgpus: (Vgpu.id * Vgpu.state) list;
    vusbs: (Vusb.id * Vusb.state) list;
  }
  let empty = {
    vm = None;
    vbds = [];
    vifs = [];
    pcis = [];
    vgpus = [];
    vusbs = [];
  }

  let cache = Hashtbl.create 10 (* indexed by Vm.id *)
  let mutex = Mutex.create ()
  let with_lock f =
    Mutex.execute mutex f

  let register id =
    debug "xenops_cache: creating empty cache for %s" id;
    with_lock (fun () ->
        Hashtbl.replace cache id empty)

  let unregister id =
    debug "xenops_cache: deleting cache for %s" id;
    with_lock (fun () ->
        Hashtbl.remove cache id)

  let find id : t option =
    with_lock (fun () ->
        Hashtbl.find_opt cache id)

  let find_vm id : Vm.state option =
    match find id with
    | Some { vm = Some vm } -> Some vm
    | _ -> None

  let find_vbd id : Vbd.state option =
    match find (fst id) with
    | Some { vbds = vbds } ->
      List.assoc_opt id vbds
    | _ -> None

  let find_vif id : Vif.state option =
    match find (fst id) with
    | Some { vifs = vifs } ->
      List.assoc_opt id vifs
    | _ -> None

  let find_pci id : Pci.state option =
    match find (fst id) with
    | Some { pcis = pcis } ->
      List.assoc_opt id pcis
    | _ -> None

  let find_vgpu id : Vgpu.state option =
    match find (fst id) with
    | Some { vgpus = vgpus } ->
      List.assoc_opt id vgpus
    | _ -> None

  let find_vusb id : Vusb.state option =
    match find (fst id) with
    | Some { vusbs = vusbs } ->
      List.assoc_opt id vusbs
    | _ -> None

  let update id t =
    with_lock (fun () ->
        if Hashtbl.mem cache id
        then Hashtbl.replace cache id t
        else debug "xenops_cache: Not updating cache for unregistered VM %s" id
      )

  let update_vbd id info =
    let existing = Opt.default empty (find (fst id)) in
    let vbds' = List.filter (fun (vbd_id, _) -> vbd_id <> id) existing.vbds in
    update (fst id) { existing with vbds = Opt.default vbds' (Opt.map (fun info -> (id, info) :: vbds') info) }

  let update_vif id info =
    let existing = Opt.default empty (find (fst id)) in
    let vifs' = List.filter (fun (vif_id, _) -> vif_id <> id) existing.vifs in
    update (fst id) { existing with vifs = Opt.default vifs' (Opt.map (fun info -> (id, info) :: vifs') info) }

  let update_pci id info =
    let existing = Opt.default empty (find (fst id)) in
    let pcis' = List.filter (fun (pci_id, _) -> pci_id <> id) existing.pcis in
    update (fst id) { existing with pcis = Opt.default pcis' (Opt.map (fun info -> (id, info) :: pcis') info) }

  let update_vgpu id info =
    let existing = Opt.default empty (find (fst id)) in
    let vgpus' = List.filter (fun (vgpu_id, _) -> vgpu_id <> id) existing.vgpus in
    update (fst id) { existing with vgpus = Opt.default vgpus' (Opt.map (fun info -> (id, info) :: vgpus') info) }

  let update_vusb id info =
    let existing = Opt.default empty (find (fst id)) in
    let vusbs' = List.filter (fun (vusb_id, _) -> vusb_id <> id) existing.vusbs in
    update (fst id) { existing with vusbs = Opt.default vusbs' (Opt.map (fun info -> (id, info) :: vusbs') info) }

  let update_vm id info =
    let existing = Opt.default empty (find id) in
    update id { existing with vm = info }

  let list () =
    with_lock (fun () ->
        Hashtbl.fold (fun id _ acc -> id :: acc) cache [])
end

module Xenopsd_metadata = struct
  (** Manage the lifetime of VM metadata pushed to xenopsd *)

  (* If the VM has Xapi_globs.persist_xenopsd_md -> filename in its other_config,
     	   we persist the xenopsd metadata to a well-known location in the filesystem *)
  let maybe_persist_md ~__context ~self md =
    let oc = Db.VM.get_other_config ~__context ~self in
    if List.mem_assoc Xapi_globs.persist_xenopsd_md oc then begin
      let file_path =
        Filename.concat Xapi_globs.persist_xenopsd_md_root (List.assoc Xapi_globs.persist_xenopsd_md oc) |>
        Stdext.Unixext.resolve_dot_and_dotdot in

      if not (String.startswith Xapi_globs.persist_xenopsd_md_root file_path) then begin
        warn "Not persisting xenopsd metadata to bad location: '%s'" file_path
      end else begin
        Unixext.mkdir_safe Xapi_globs.persist_xenopsd_md_root 0o755;
        Unixext.write_string_to_file file_path md
      end
    end

  let push ~__context ~self =
    Mutex.execute metadata_m (fun () ->
        let md = create_metadata ~__context ~self in
        let txt = md |> rpc_of Metadata.t |> Jsonrpc.to_string in
        info "xenops: VM.import_metadata %s" txt;
        let dbg = Context.string_of_task __context in
        let module Client = (val make_client (queue_of_vm ~__context ~self) : XENOPS) in
        let id = Client.VM.import_metadata dbg txt in

        maybe_persist_md ~__context ~self txt;

        Xapi_cache.register id (Some txt);
        Xenops_cache.register id;
        id)

  let delete_nolock ~__context id =
    let dbg = Context.string_of_task __context in
    info "xenops: VM.remove %s" id;
    try
      let module Client = (val make_client (queue_of_vm ~__context ~self:(vm_of_id ~__context id)) : XENOPS) in
      Client.VM.remove dbg id;

      (* Once the VM has been successfully removed from xenopsd, remove the caches *)
      Xenops_cache.unregister id;
      Xapi_cache.unregister id

    with
    | Xenopsd_error (Bad_power_state(_, _)) ->
      (* This can fail during a localhost live migrate; but this is safe to ignore *)
      debug "We have not removed metadata from xenopsd because VM %s is still running" id
    | Xenopsd_error (Does_not_exist(_)) ->
      debug "Metadata for VM %s was already removed" id


  (* Unregisters a VM with xenopsd, and cleans up metadata and caches *)
  let pull ~__context id =
    Mutex.execute metadata_m
      (fun () ->
         info "xenops: VM.export_metadata %s" id;
         let dbg = Context.string_of_task __context in
         let module Client = (val make_client (queue_of_vm ~__context ~self:(vm_of_id ~__context id)) : XENOPS) in
         let md =
           match Client.VM.export_metadata dbg id |> Jsonrpc.of_string |> Rpcmarshal.unmarshal Metadata.t.Rpc.Types.ty with
           | Ok x -> x
           | Error (`Msg m) -> raise (Xenopsd_error (Internal_error (Printf.sprintf "Failed to unmarshal metadata: %s" m)))
         in

         delete_nolock ~__context id;

         md)

  let delete ~__context id =
    Mutex.execute metadata_m
      (fun () ->
         delete_nolock ~__context id
      )

  let update ~__context ~self =
    let id = id_of_vm ~__context ~self in
    let queue_name = queue_of_vm ~__context ~self in
    Mutex.execute metadata_m
      (fun () ->
         let dbg = Context.string_of_task __context in
         if vm_exists_in_xenopsd queue_name dbg id
         then
           let txt = create_metadata ~__context ~self |> rpc_of Metadata.t |> Jsonrpc.to_string in
           if Xapi_cache.update_if_changed id txt then begin
             debug "VM %s metadata has changed: updating xenopsd" id;
             info "xenops: VM.import_metadata %s" txt;
             maybe_persist_md ~__context ~self txt;
             let module Client = (val make_client queue_name : XENOPS) in
             let (_: Vm.id) = Client.VM.import_metadata dbg txt in
             ()
           end)
end

let add_caches id =
  Mutex.execute metadata_m
    (fun () ->
       Xapi_cache.register id None;
       Xenops_cache.register id;
    )


let to_xenops_console_protocol = let open Vm in function
    | `rfb -> Rfb
    | `vt100 -> Vt100
    | `rdp -> Rfb (* RDP was never used in the XenAPI so this never happens *)
let to_xenapi_console_protocol = let open Vm in function
    | Rfb -> `rfb
    | Vt100 -> `vt100

(* Event handling:
   When we tell the xenopsd to start a VM, we wait for the task to complete.
   We also wait for an iteration of the xenops event loop to ensure that
   the states of modified objects are properly set. For example: the VM
   power_state is modified by the event thread *only* and must take its
   final value when the XenAPI VM.start returns. It will not be set when
   the xenops VM.start returns since the event is asynchronous. *)

(* If a xapi event thread is blocked, wake it up and cause it to re-register. This should be
   called after updating Host.resident_VMs *)
let trigger_xenapi_reregister =
  ref (fun () ->
      debug "No xapi event thread to wake up"
    )


module Events_from_xenopsd = struct
  type t = {
    mutable finished: bool;
    m: Mutex.t;
    c: Condition.t;
  }
  let make () = {
    finished = false;
    m = Mutex.create ();
    c = Condition.create ();
  }
  let active = Hashtbl.create 10
  let active_m = Mutex.create ()
  let register =
    let counter = ref 0 in
    fun t ->
      Mutex.execute active_m
        (fun () ->
           let id = !counter in
           incr counter;
           Hashtbl.replace active id t;
           id
        )
  let wait queue_name dbg vm_id () =
    let module Client = (val make_client queue_name : XENOPS) in
    let t = make () in
    let id = register t in
    debug "Client.UPDATES.inject_barrier %d" id;
    Client.UPDATES.inject_barrier dbg vm_id id;
    Mutex.execute t.m
      (fun () ->
         while not t.finished do Condition.wait t.c t.m done
      )
  let wakeup queue_name dbg id =
    let module Client = (val make_client queue_name : XENOPS) in
    Client.UPDATES.remove_barrier dbg id;
    let t = Mutex.execute active_m
        (fun () ->
           if not(Hashtbl.mem active id)
           then (warn "Events_from_xenopsd.wakeup: unknown id %d" id; None)
           else
             let t = Hashtbl.find active id in
             Hashtbl.remove active id;
             Some t
        ) in
    Opt.iter
      (fun t ->
         Mutex.execute t.m
           (fun () ->
              t.finished <- true;
              Condition.signal t.c
           )
      ) t

  let events_suppressed_on = Hashtbl.create 10
  let events_suppressed_on_m = Mutex.create ()
  let events_suppressed_on_c = Condition.create ()
  let are_suppressed vm =
    Hashtbl.mem events_suppressed_on vm

  let with_suppressed queue_name dbg vm_id f =
    debug "suppressing xenops events on VM: %s" vm_id;
    let module Client = (val make_client queue_name : XENOPS) in
    Mutex.execute events_suppressed_on_m (fun () ->
        Hashtbl.add events_suppressed_on vm_id ();
      );
    finally f (fun () ->
        Mutex.execute events_suppressed_on_m (fun () ->
            Hashtbl.remove events_suppressed_on vm_id;
            if not (Hashtbl.mem events_suppressed_on vm_id) then begin
              debug "re-enabled xenops events on VM: %s; refreshing VM" vm_id;
              Client.UPDATES.refresh_vm dbg vm_id;
              wait queue_name dbg vm_id ();
              Condition.broadcast events_suppressed_on_c;
            end else while are_suppressed vm_id do
                debug "waiting for events to become re-enabled";
                Condition.wait events_suppressed_on_c events_suppressed_on_m
              done;
          );
      )
end

let update_vm ~__context id =
  try
    if Events_from_xenopsd.are_suppressed id
    then debug "xenopsd event: ignoring event for VM (VM %s migrating away)" id
    else
      let self = Db.VM.get_by_uuid ~__context ~uuid:id in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self <> localhost
      then debug "xenopsd event: ignoring event for VM (VM %s not resident)" id
      else
        let previous = Xenops_cache.find_vm id in
        let dbg = Context.string_of_task __context in
        let module Client = (val make_client (queue_of_vm ~__context ~self) : XENOPS) in
        let info = try Some (Client.VM.stat dbg id) with _ -> None in
        if Opt.map snd info = previous
        then debug "xenopsd event: ignoring event for VM %s: metadata has not changed" id
        else begin
          debug "xenopsd event: processing event for VM %s" id;
          if info = None then debug "xenopsd event: VM state missing: assuming VM has shut down";
          let should_update_allowed_operations = ref false in
          let different f =
            let a = Opt.map (fun x -> f (snd x)) info in
            let b = Opt.map f previous in
            a <> b in
          (* Helpers to create and update guest metrics when needed *)
          let lookup state key =
            if List.mem_assoc key state.Vm.guest_agent then Some (List.assoc key state.Vm.guest_agent) else None in
          let list state dir =
            let dir = if dir.[0] = '/' then String.sub dir 1 (String.length dir - 1) else dir in
            let results = Listext.List.filter_map (fun (path, value) ->
                if String.startswith dir path then begin
                  let rest = String.sub path (String.length dir) (String.length path - (String.length dir)) in
                  match List.filter (fun x -> x <> "") (String.split '/' rest) with
                  | x :: _ -> Some x
                  | _ -> None
                end else None
              ) state.Vm.guest_agent |> Listext.List.setify in
            results in
          let create_guest_metrics_if_needed () =
            let gm = Db.VM.get_guest_metrics ~__context ~self in
            if gm = Ref.null then
              Opt.iter
                (fun (_, state) ->
                   List.iter
                     (fun domid ->
                        try
                          let new_gm_ref =
                            Xapi_guest_agent.create_and_set_guest_metrics
                              (lookup state)
                              (list state)
                              ~__context
                              ~domid
                              ~uuid:id
                              ~pV_drivers_detected:state.pv_drivers_detected
                          in
                          debug "xenopsd event: created guest metrics %s for VM %s" (Ref.string_of new_gm_ref) id
                        with e ->
                          error "Caught %s: while creating VM %s guest metrics" (Printexc.to_string e) id
                     ) state.domids
                ) info in
          let check_guest_agent () =
            Opt.iter
              (fun (_, state) ->
                 Opt.iter (fun oldstate ->
                     let old_ga = oldstate.Vm.guest_agent in
                     let new_ga = state.Vm.guest_agent in

                     (* Remove memory keys *)
                     let ignored_keys = [ "data/meminfo_free"; "data/updated"; "data/update_cnt" ] in
                     let remove_ignored ga =
                       List.fold_left (fun acc k -> List.filter (fun x -> fst x <> k) acc) ga ignored_keys in
                     let old_ga = remove_ignored old_ga in
                     let new_ga = remove_ignored new_ga in
                     if new_ga <> old_ga then begin
                       debug "Will update VM.allowed_operations because guest_agent has changed.";
                       should_update_allowed_operations := true
                     end else begin
                       debug "Supressing VM.allowed_operations update because guest_agent data is largely the same"
                     end
                   ) previous;
                 List.iter
                   (fun domid ->
                      try
                        debug "xenopsd event: Updating VM %s domid %d guest_agent" id domid;
                        Xapi_guest_agent.all (lookup state) (list state) ~__context ~domid ~uuid:id ~pV_drivers_detected:state.pv_drivers_detected
                      with e ->
                        error "Caught %s: while updating VM %s guest_agent" (Printexc.to_string e) id
                   ) state.domids
              ) info in
          (* Notes on error handling: if something fails we log and continue, to
             maximise the amount of state which is correctly synced. If something
             does fail then we may end up permanently out-of-sync until either a
             process restart or an event is generated. We may wish to periodically
             inject artificial events IF there has been an event sync failure? *)
          let power_state = xenapi_of_xenops_power_state (Opt.map (fun x -> (snd x).Vm.power_state) info) in

          (* We preserve the current_domain_type of suspended VMs like we preserve
             the currently_attached fields for VBDs/VIFs etc - it's important to know
             whether suspended VMs are going to resume into PV or PVinPVH for example.
             We do this before updating the power_state to maintain the invariant that
             any VM that's not `Halted cannot have an unspecified current_domain_type *)
          if different (fun x -> x.domain_type) && (power_state <> `Suspended) then begin
            Opt.iter
              (fun (_, state) ->
                 let metrics = Db.VM.get_metrics ~__context ~self in
                 let domain_type = match state.Vm.domain_type with
                   | Domain_HVM       -> `hvm
                   | Domain_PV        -> `pv
                   | Domain_PVinPVH   -> `pv_in_pvh
                   | Domain_undefined -> `unspecified
                 in
                 debug "xenopsd event: Updating VM %s current_domain_type <- %s"
                   id (Record_util.domain_type_to_string domain_type);
                 Db.VM_metrics.set_current_domain_type ~__context ~self:metrics
                   ~value:domain_type;
              )
              info
          end;
          if different (fun x -> x.power_state) then begin
            try
              debug "Will update VM.allowed_operations because power_state has changed.";
              should_update_allowed_operations := true;
              debug "xenopsd event: Updating VM %s power_state <- %s" id (Record_util.power_state_to_string power_state);
              (* This will mark VBDs, VIFs as detached and clear resident_on
                 if the VM has permanently shutdown.  current-operations
                 should not be reset as there maybe a checkpoint is ongoing*)
              Xapi_vm_lifecycle.force_state_reset_keep_current_operations ~__context ~self ~value:power_state;

              if power_state = `Running then create_guest_metrics_if_needed ();
              if power_state = `Suspended || power_state = `Halted then begin
                Xapi_network.detach_for_vm ~__context ~host:localhost ~vm:self;
                Storage_access.reset ~__context ~vm:self;
              end;
              if power_state = `Halted
              then Xenopsd_metadata.delete ~__context id;
              if power_state = `Suspended then begin
                let md = Xenopsd_metadata.pull ~__context id in
                match md.Metadata.domains with
                | None ->
                  error "Suspended VM has no domain-specific metadata"
                | Some x ->
                  Db.VM.set_last_booted_record ~__context ~self ~value:x;
                  debug "VM %s last_booted_record set to %s" (Ref.string_of self) x;
                  Xenopsd_metadata.delete ~__context id
              end;
              if power_state = `Halted then (
                !trigger_xenapi_reregister ()
              );
            with e ->
              error "Caught %s: while updating VM %s power_state" (Printexc.to_string e) id
          end;
          if different (fun x -> x.domids) then begin
            try
              debug "Will update VM.allowed_operations because domid has changed.";
              should_update_allowed_operations := true;
              debug "xenopsd event: Updating VM %s domid" id;
              Opt.iter
                (fun (_, state) ->
                   match state.Vm.domids with
                   | value :: _ ->
                     Db.VM.set_domid ~__context ~self ~value:(Int64.of_int value)
                   | [] -> () (* happens when the VM is shutdown *)
                ) info;
              (* If this is a storage domain, attempt to plug the PBD *)
              Opt.iter (fun pbd ->
                  let (_: Thread.t) = Thread.create (fun () ->
                      (* Don't block the database update thread *)
                      Xapi_pbd.plug ~__context ~self:pbd
                    ) () in
                  ()
                ) (System_domains.pbd_of_vm ~__context ~vm:self)
            with e ->
              error "Caught %s: while updating VM %s domids" (Printexc.to_string e) id
          end;
          (* consoles *)
          if different (fun x -> x.consoles) then begin
            try
              debug "xenopsd event: Updating VM %s consoles" id;
              Opt.iter
                (fun (_, state) ->
                   let localhost = Helpers.get_localhost ~__context in
                   let address = Db.Host.get_address ~__context ~self:localhost in
                   let uri = Printf.sprintf "https://%s%s" address Constants.console_uri in
                   let get_uri_from_location loc =
                     try
                       let n = String.index loc '?' in
                       String.sub loc 0 n
                     with Not_found -> loc
                   in
                   let current_protocols = List.map
                       (fun self ->
                          (Db.Console.get_protocol ~__context ~self |> to_xenops_console_protocol,
                           Db.Console.get_location ~__context ~self |> get_uri_from_location),
                          self)
                       (Db.VM.get_consoles ~__context ~self) in
                   let new_protocols = List.map (fun c -> (c.Vm.protocol, uri), c) state.Vm.consoles in
                   (* Destroy consoles that have gone away *)
                   List.iter
                     (fun protocol ->
                        let self = List.assoc protocol current_protocols in
                        Db.Console.destroy ~__context ~self
                     ) (Listext.List.set_difference (List.map fst current_protocols) (List.map fst new_protocols));
                   (* Create consoles that have appeared *)
                   List.iter
                     (fun (protocol, _) ->
                        let ref = Ref.make () in
                        let uuid = Uuid.to_string (Uuid.make_uuid ()) in
                        let location = Printf.sprintf "%s?uuid=%s" uri uuid in
                        let port =
                          try Int64.of_int ((List.find (fun c -> c.Vm.protocol = protocol) state.Vm.consoles).port)
                          with Not_found -> -1L
                        in
                        Db.Console.create ~__context ~ref ~uuid
                          ~protocol:(to_xenapi_console_protocol protocol) ~location ~vM:self
                          ~other_config:[] ~port
                     ) (Listext.List.set_difference (List.map fst new_protocols) (List.map fst current_protocols));
                ) info;
            with e ->
              error "Caught %s: while updating VM %s consoles" (Printexc.to_string e) id
          end;
          if different (fun x -> x.memory_target) then begin
            try
              Opt.iter
                (fun (_, state) ->
                   debug "xenopsd event: Updating VM %s memory_target <- %Ld" id state.Vm.memory_target;
                   Db.VM.set_memory_target ~__context ~self ~value:state.memory_target
                ) info
            with e ->
              error "Caught %s: while updating VM %s consoles" (Printexc.to_string e) id
          end;
          if different (fun x -> x.rtc_timeoffset) then begin
            try
              Opt.iter
                (fun (_, state) ->
                   if state.Vm.rtc_timeoffset <> "" then begin
                     debug "xenopsd event: Updating VM %s platform:timeoffset <- %s" id state.rtc_timeoffset;
                     (try Db.VM.remove_from_platform ~__context ~self ~key:Vm_platform.timeoffset with _ -> ());
                     Db.VM.add_to_platform ~__context ~self ~key:Vm_platform.timeoffset ~value:state.rtc_timeoffset;
                   end
                ) info
            with e ->
              error "Caught %s: while updating VM %s rtc/timeoffset" (Printexc.to_string e) id
          end;
          if different (fun x -> x.hvm) then begin
            Opt.iter
              (fun (_, state) ->
                 let metrics = Db.VM.get_metrics ~__context ~self in
                 debug "xenopsd event: Updating VM %s hvm <- %s"
                   id (string_of_bool state.Vm.hvm);
                 Db.VM_metrics.set_hvm ~__context ~self:metrics
                   ~value:state.Vm.hvm;
              )
              info
          end;
          if different (fun x -> x.nomigrate) then begin
            Opt.iter
              (fun (_, state) ->
                 let metrics = Db.VM.get_metrics ~__context ~self in
                 debug "xenopsd event: Updating VM %s nomigrate <- %s"
                   id (string_of_bool state.Vm.nomigrate);
                 Db.VM_metrics.set_nomigrate ~__context ~self:metrics
                   ~value:state.Vm.nomigrate;
              )
              info
          end;
          if different (fun x -> x.nested_virt) then begin
            Opt.iter
              (fun (_, state) ->
                 let metrics = Db.VM.get_metrics ~__context ~self in
                 debug "xenopsd event: Updating VM %s nested_virt <- %s"
                   id (string_of_bool state.Vm.nested_virt);
                 Db.VM_metrics.set_nested_virt ~__context ~self:metrics
                   ~value:state.Vm.nested_virt;
              )
              info
          end;
          let update_pv_drivers_detected () =
            Opt.iter
              (fun (_, state) ->
                 try
                   let gm = Db.VM.get_guest_metrics ~__context ~self in
                   debug "xenopsd event: Updating VM %s PV drivers detected %b" id state.Vm.pv_drivers_detected;
                   Db.VM_guest_metrics.set_PV_drivers_detected ~__context ~self:gm ~value:state.Vm.pv_drivers_detected;
                   Db.VM_guest_metrics.set_PV_drivers_up_to_date ~__context ~self:gm ~value:state.Vm.pv_drivers_detected
                 with e ->
                   debug "Caught %s: while updating VM %s PV drivers" (Printexc.to_string e) id
              ) info in
          (* Chack last_start_time before updating anything in the guest metrics *)
          if different (fun x -> x.last_start_time) then begin
            try
              Opt.iter
                (fun (_, state) ->
                   debug "xenopsd event: Updating VM %s last_start_time <- %s" id (Date.to_string (Date.of_float state.Vm.last_start_time));
                   let metrics = Db.VM.get_metrics ~__context ~self in
                   let start_time = Date.of_float state.Vm.last_start_time in
                   Db.VM_metrics.set_start_time ~__context ~self:metrics ~value:start_time;

                   create_guest_metrics_if_needed ();
                   let gm = Db.VM.get_guest_metrics ~__context ~self in
                   let update_time = Db.VM_guest_metrics.get_last_updated ~__context ~self:gm in
                   if update_time < start_time then begin
                     debug "VM %s guest metrics update time (%s) < VM start time (%s): deleting"
                       id (Date.to_string update_time) (Date.to_string start_time);
                     Xapi_vm_helpers.delete_guest_metrics ~__context ~self;
                     check_guest_agent ();
                   end
                ) info
            with e ->
              error "Caught %s: while updating VM %s last_start_time" (Printexc.to_string e) id
          end;
          Opt.iter
            (fun (_, state) ->
               List.iter
                 (fun domid ->
                    (* Guest metrics could have been destroyed during the last_start_time check
                       by recreating them, we avoid CA-223387 *)
                    create_guest_metrics_if_needed ();
                    if different (fun x -> x.Vm.uncooperative_balloon_driver) then begin
                      debug "xenopsd event: VM %s domid %d uncooperative_balloon_driver = %b" id domid state.Vm.uncooperative_balloon_driver;
                    end;
                    if different (fun x -> x.Vm.guest_agent) then check_guest_agent ();
                    if different (fun x -> x.Vm.pv_drivers_detected) then update_pv_drivers_detected ();

                    if different (fun x -> x.Vm.xsdata_state) then begin
                      try
                        debug "xenopsd event: Updating VM %s domid %d xsdata" id domid;
                        Db.VM.set_xenstore_data ~__context ~self ~value:state.Vm.xsdata_state
                      with e ->
                        error "Caught %s: while updating VM %s xsdata" (Printexc.to_string e) id
                    end;
                    if different (fun x -> x.Vm.memory_target) then begin
                      try
                        debug "xenopsd event: Updating VM %s domid %d memory target" id domid;
                        Rrdd.update_vm_memory_target domid state.Vm.memory_target;
                      with e ->
                        error "Caught %s: while updating VM %s memory_target" (Printexc.to_string e) id
                    end;
                 ) state.Vm.domids;
            ) info;
          if different (fun x -> x.Vm.vcpu_target) then begin
            Opt.iter
              (fun (_, state) ->
                 try
                   debug "xenopsd event: Updating VM %s vcpu_target <- %d" id state.Vm.vcpu_target;
                   let metrics = Db.VM.get_metrics ~__context ~self in
                   Db.VM_metrics.set_VCPUs_number ~__context ~self:metrics ~value:(Int64.of_int state.Vm.vcpu_target);
                 with e ->
                   error "Caught %s: while updating VM %s VCPUs_number" (Printexc.to_string e) id
              ) info
          end;
          if different (fun x -> x.shadow_multiplier_target) then begin
            try
              Opt.iter
                (fun (_, state) ->
                   debug "xenopsd event: Updating VM %s shadow_multiplier <- %.2f" id state.Vm.shadow_multiplier_target;
                   if state.Vm.power_state <> Halted && state.Vm.shadow_multiplier_target >= 0.0 then
                     Db.VM.set_HVM_shadow_multiplier ~__context ~self ~value:state.Vm.shadow_multiplier_target
                ) info
            with e ->
              error "Caught %s: while updating VM %s HVM_shadow_multiplier" (Printexc.to_string e) id
          end;
          Xenops_cache.update_vm id (Opt.map snd info);
          if !should_update_allowed_operations then
            Helpers.call_api_functions ~__context
              (fun rpc session_id -> XenAPI.VM.update_allowed_operations ~rpc ~session_id ~self);
        end
  with e ->
    error "xenopsd event: Caught %s while updating VM: has this VM been removed while this host is offline?" (string_of_exn e)

let update_vbd ~__context (id: (string * string)) =
  try
    if Events_from_xenopsd.are_suppressed (fst id)
    then debug "xenopsd event: ignoring event for VM (VM %s migrating away)" (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm <> localhost
      then debug "xenopsd event: ignoring event for VBD (VM %s not resident)" (fst id)
      else
        let previous = Xenops_cache.find_vbd id in
        let dbg = Context.string_of_task __context in
        let module Client = (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS) in
        let info = try Some(Client.VBD.stat dbg id) with _ -> None in
        if Opt.map snd info = previous
        then debug "xenopsd event: ignoring event for VBD %s.%s: metadata has not changed" (fst id) (snd id)
        else begin
          let vbds = Db.VM.get_VBDs ~__context ~self:vm in
          let vbdrs = List.map (fun self -> self, Db.VBD.get_record ~__context ~self) vbds in
          let linux_device = snd id in
          let device_number = Device_number.of_linux_device linux_device in
          (* only try matching against disk number if the device is not a floppy (as "0" shouldn't match "fda") *)
          let disk_number =
            match Device_number.spec device_number with
            | (Device_number.Ide,_,_)
            | (Device_number.Xen,_,_) -> Some (device_number |> Device_number.to_disk_number |> string_of_int)
            | _ -> None in
          debug "VM %s VBD userdevices = [ %s ]" (fst id) (String.concat "; " (List.map (fun (_,r) -> r.API.vBD_userdevice) vbdrs));
          let vbd, vbd_r = List.find (fun (_, vbdr) -> vbdr.API.vBD_userdevice = linux_device ||
                                                       (Opt.is_some disk_number && vbdr.API.vBD_userdevice = Opt.unbox disk_number)) vbdrs in
          debug "VBD %s.%s matched device %s" (fst id) (snd id) vbd_r.API.vBD_userdevice;
          Opt.iter
            (fun (vb, state) ->
               let currently_attached = state.Vbd.plugged || state.Vbd.active in
               debug "xenopsd event: Updating VBD %s.%s device <- %s; currently_attached <- %b" (fst id) (snd id) linux_device currently_attached;
               Db.VBD.set_device ~__context ~self:vbd ~value:linux_device;
               Db.VBD.set_currently_attached ~__context ~self:vbd ~value:currently_attached;
               if state.Vbd.plugged then begin
                 match state.Vbd.backend_present with
                 | Some (VDI x) ->
                   Opt.iter
                     (fun (vdi, _) ->
                        debug "VBD %s.%s backend_present = %s" (fst id) (snd id) x;
                        Db.VBD.set_VDI ~__context ~self:vbd ~value:vdi;
                        Db.VBD.set_empty ~__context ~self:vbd ~value:false;
                        Xapi_vdi.update_allowed_operations ~__context ~self:vdi;
                     ) (vdi_of_disk ~__context x)
                 | Some d ->
                   error "VBD %s.%s backend_present has unknown disk = %s" (fst id) (snd id) (d |> rpc_of disk |> Jsonrpc.to_string)
                 | None ->
                   if vbd_r.API.vBD_type = `CD then begin
                     debug "VBD %s.%s backend_present = None (empty)" (fst id) (snd id);
                     Db.VBD.set_empty ~__context ~self:vbd ~value:true;
                     Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null
                   end else error "VBD %s.%s is empty but is not a CD" (fst id) (snd id)
               end;
               if not(state.Vbd.plugged || state.Vbd.active) then begin
                 debug "VBD.remove %s.%s" (fst id) (snd id);
                 (try Client.VBD.remove dbg id with e -> debug "VBD.remove failed: %s" (Printexc.to_string e))
               end
            ) info;
          Xenops_cache.update_vbd id (Opt.map snd info);
          Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd;
          if not (Db.VBD.get_empty ~__context ~self:vbd) then
            let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
            Xapi_vdi.update_allowed_operations ~__context ~self:vdi
        end
  with e ->
    error "xenopsd event: Caught %s while updating VBD" (string_of_exn e)

let update_vif ~__context id =
  try
    if Events_from_xenopsd.are_suppressed (fst id)
    then debug "xenopsd event: ignoring event for VIF (VM %s migrating away)" (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm <> localhost
      then debug "xenopsd event: ignoring event for VIF (VM %s not resident)" (fst id)
      else
        let previous = Xenops_cache.find_vif id in
        let dbg = Context.string_of_task __context in
        let module Client = (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS) in
        let info = try Some (Client.VIF.stat dbg id) with _ -> None in
        if Opt.map snd info = previous
        then debug "xenopsd event: ignoring event for VIF %s.%s: metadata has not changed" (fst id) (snd id)
        else begin
          let vifs = Db.VM.get_VIFs ~__context ~self:vm in
          let vifrs = List.map (fun self -> self, Db.VIF.get_record ~__context ~self) vifs in
          let vif, vifr = List.find (fun (_, vifr) -> vifr.API.vIF_device = (snd id)) vifrs in
          Opt.iter
            (fun (vf, state) ->
               if not (state.Vif.plugged || state.Vif.active) then begin
                 (try
                    Xapi_network.deregister_vif ~__context vif
                  with e ->
                    error "Failed to deregister vif: %s" (Printexc.to_string e));
                 debug "VIF.remove %s.%s" (fst id) (snd id);
                 (try Client.VIF.remove dbg id with e -> debug "VIF.remove failed: %s" (Printexc.to_string e))
               end;

               begin match backend_of_vif ~__context ~vif with
                 | Network.Sriov _ -> ()
                 | Network.Local _ | Network.Remote _ ->
                   if state.plugged then begin
                     (* sync MTU *)
                     (try
                        match state.device with
                        | None -> failwith (Printf.sprintf "could not determine device id for VIF %s.%s" (fst id) (snd id))
                        | Some device ->
                          let dbg = Context.string_of_task __context in
                          let mtu = Net.Interface.get_mtu dbg device in
                          Db.VIF.set_MTU ~__context ~self:vif ~value:(Int64.of_int mtu)
                      with _ ->
                        debug "could not update MTU field on VIF %s.%s" (fst id) (snd id));

                     (* Clear monitor cache for associated PIF if pass_through_pif_carrier is set *)
                     if !Xapi_globs.pass_through_pif_carrier then
                       let host = Helpers.get_localhost ~__context in
                       let pifs = Xapi_network_attach_helpers.get_local_pifs ~__context ~network:vifr.API.vIF_network ~host in
                       List.iter (fun pif ->
                           let pif_name = Db.PIF.get_device ~__context ~self:pif in
                           Monitor_dbcalls_cache.clear_cache_for_pif ~pif_name
                         ) pifs
                   end
               end;
               (match Pvs_proxy_control.find_proxy_for_vif ~__context ~vif with
                | None -> ()
                | Some proxy ->
                  debug "xenopsd event: Updating PVS_proxy for VIF %s.%s currently_attached <- %b" (fst id) (snd id) state.pvs_rules_active;
                  if state.pvs_rules_active then begin
                    Db.PVS_proxy.set_currently_attached ~__context ~self:proxy ~value:true;
                    (* force status to be read again by invalidating cache *)
                    Monitor_dbcalls_cache.clear_pvs_status_cache (fst id)
                  end else
                    Pvs_proxy_control.clear_proxy_state ~__context vif proxy
               );
               debug "xenopsd event: Updating VIF %s.%s currently_attached <- %b" (fst id) (snd id) (state.plugged || state.active);
               Db.VIF.set_currently_attached ~__context ~self:vif ~value:(state.plugged || state.active)
            ) info;
          Xenops_cache.update_vif id (Opt.map snd info);
          Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif
        end
  with e ->
    error "xenopsd event: Caught %s while updating VIF" (string_of_exn e)

let update_pci ~__context id =
  try
    if Events_from_xenopsd.are_suppressed (fst id)
    then debug "xenopsd event: ignoring event for PCI (VM %s migrating away)" (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm <> localhost
      then debug "xenopsd event: ignoring event for PCI (VM %s not resident)" (fst id)
      else
        let previous = Xenops_cache.find_pci id in
        let dbg = Context.string_of_task __context in
        let module Client = (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS) in
        let info = try Some (Client.PCI.stat dbg id) with _ -> None in
        if Opt.map snd info = previous
        then debug "xenopsd event: ignoring event for PCI %s.%s: metadata has not changed" (fst id) (snd id)
        else begin
          let pcis = Db.Host.get_PCIs ~__context ~self:localhost in
          let pcirs = List.map (fun self -> self, Db.PCI.get_record ~__context ~self) pcis in

          let pci, _ = List.find (fun (_, pcir) -> pcir.API.pCI_pci_id = (snd id)) pcirs in

          (* Assumption: a VM can have only one vGPU *)
          let vgpu_opt =
            let pci_class = Db.PCI.get_class_id ~__context ~self:pci in
            if Xapi_pci.(is_class_of_kind Display_controller @@ int_of_id pci_class)
            then
              match Db.VM.get_VGPUs ~__context ~self:vm with
              | vgpu :: _ -> Some vgpu
              | _ -> None
            else None in
          let attached_in_db = List.mem vm (Db.PCI.get_attached_VMs ~__context ~self:pci) in
          Opt.iter
            (fun (_, state) ->
               debug "xenopsd event: Updating PCI %s.%s currently_attached <- %b" (fst id) (snd id) state.Pci.plugged;
               if attached_in_db && (not state.Pci.plugged) then
                 Db.PCI.remove_attached_VMs ~__context ~self:pci ~value:vm
               else if (not attached_in_db) && state.plugged then begin
                 Db.PCI.add_attached_VMs ~__context ~self:pci ~value:vm;
                 Db.PCI.set_scheduled_to_be_attached_to ~__context ~self:pci ~value:Ref.null
               end;

               Opt.iter
                 (fun vgpu ->
                    let scheduled =
                      Db.VGPU.get_scheduled_to_be_resident_on ~__context ~self:vgpu
                    in
                    if Db.is_valid_ref __context scheduled && state.Pci.plugged
                    then
                      Helpers.call_api_functions ~__context
                        (fun rpc session_id ->
                           XenAPI.VGPU.atomic_set_resident_on ~rpc ~session_id
                             ~self:vgpu ~value:scheduled);
                    debug "xenopsd event: Update VGPU %s.%s currently_attached <- %b" (fst id) (snd id) state.plugged;
                    Db.VGPU.set_currently_attached ~__context ~self:vgpu ~value:state.Pci.plugged
                 ) vgpu_opt
            ) info;
          Xenops_cache.update_pci id (Opt.map snd info);
        end
  with e ->
    error "xenopsd event: Caught %s while updating PCI" (string_of_exn e)

let update_vgpu ~__context id =
  try
    if Events_from_xenopsd.are_suppressed (fst id)
    then debug "xenopsd event: ignoring event for VGPU (VM %s migrating away)" (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm <> localhost
      then debug "xenopsd event: ignoring event for VGPU (VM %s not resident)" (fst id)
      else
        let previous = Xenops_cache.find_vgpu id in
        let dbg = Context.string_of_task __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let info = try Some (Client.VGPU.stat dbg id) with _ -> None in
        if Opt.map snd info = previous
        then debug "xenopsd event: ignoring event for VGPU %s.%s: metadata has not changed" (fst id) (snd id)
        else begin
          let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
          let vgpu_records =
            List.map
              (fun self -> self, Db.VGPU.get_record ~__context ~self)
              vgpus
          in
          let vgpu, vgpu_record =
            List.find
              (fun (_, vgpu_record) -> vgpu_record.API.vGPU_device = (snd id))
              vgpu_records
          in
          (* We only proceed if the VGPU is not a passthrough VGPU. In the
           * passthrough case, the VM will have a PCI device, and update_pci
           * will set VGPU.{resident_on;currently_attached}. *)
          if Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_record.API.vGPU_type = None then
            Opt.iter
              (fun (xenopsd_vgpu, state) ->
                 if state.Vgpu.plugged then begin
                   let scheduled =
                     Db.VGPU.get_scheduled_to_be_resident_on ~__context ~self:vgpu
                   in
                   if Db.is_valid_ref __context scheduled
                   then begin
                     Helpers.call_api_functions ~__context
                       (fun rpc session_id ->
                          XenAPI.VGPU.atomic_set_resident_on ~rpc ~session_id
                            ~self:vgpu ~value:scheduled)
                   end;
                   if not vgpu_record.API.vGPU_currently_attached
                   then Db.VGPU.set_currently_attached ~__context
                       ~self:vgpu ~value:true
                 end else begin
                   if vgpu_record.API.vGPU_currently_attached
                   then Db.VGPU.set_currently_attached ~__context
                       ~self:vgpu ~value:false;
                   try Client.VGPU.remove dbg id
                   with e -> debug "VGPU.remove failed: %s" (Printexc.to_string e)
                 end) info;
          Xenops_cache.update_vgpu id (Opt.map snd info)
        end
  with e ->
    error "xenopsd event: Caught %s while updating VGPU" (string_of_exn e)

let update_vusb ~__context (id: (string * string)) =
  try
    if Events_from_xenopsd.are_suppressed (fst id)
    then debug "xenopsd event: ignoring event for VM (VM %s migrating away)" (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm <> localhost
      then debug "xenopsd event: ignoring event for VUSB (VM %s not resident)" (fst id)
      else
        let previous = Xenops_cache.find_vusb id in
        let dbg = Context.string_of_task __context in
        let module Client = (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS) in
        let info = try Some(Client.VUSB.stat dbg id) with _ -> None in
        if Opt.map snd info = previous
        then debug "xenopsd event: ignoring event for VUSB %s.%s: metadata has not changed" (fst id) (snd id)
        else begin
          let pusb, pusb_r =
            Db.VM.get_VUSBs ~__context ~self:vm
            |> List.map (fun self -> Db.VUSB.get_USB_group ~__context ~self)
            |> List.map (fun usb_group -> Helpers.get_first_pusb ~__context usb_group)
            |> List.map (fun self -> self, Db.PUSB.get_record ~__context ~self)
            |> List.find (fun (_, pusbr) -> "vusb" ^ pusbr.API.pUSB_path= (snd id))
          in
          let usb_group = Db.PUSB.get_USB_group ~__context ~self:pusb in
          let vusb = Helpers.get_first_vusb ~__context usb_group in

          Opt.iter
            (fun (ub, state) ->
               debug "xenopsd event: Updating USB %s.%s; plugged <- %b" (fst id) (snd id)  state.Vusb.plugged;
               let currently_attached = state.Vusb.plugged in
               Db.VUSB.set_currently_attached ~__context ~self:vusb ~value:currently_attached;
            ) info;
          Xenops_cache.update_vusb id (Opt.map snd info);
          Xapi_vusb_helpers.update_allowed_operations ~__context ~self:vusb
        end
  with e ->
    error "xenopsd event: Caught %s while updating VUSB" (string_of_exn e)

exception Not_a_xenops_task
let wrap queue_name id = TaskHelper.Xenops (queue_name, id)
let unwrap x = match x with | TaskHelper.Xenops (queue_name, id) -> queue_name, id | _ -> raise Not_a_xenops_task
let register_task __context ?cancellable queue_name id = TaskHelper.register_task __context ?cancellable (wrap queue_name id); id
let unregister_task __context queue_name id = TaskHelper.unregister_task __context (wrap queue_name id); id

let update_task ~__context queue_name id =
  try
    let self = TaskHelper.id_to_task_exn (TaskHelper.Xenops (queue_name, id)) in (* throws Not_found *)
    let dbg = Context.string_of_task __context in
    let module Client = (val make_client queue_name : XENOPS) in
    let task_t = Client.TASK.stat dbg id in
    match task_t.Task.state with
    | Task.Pending x ->
      Db.Task.set_progress ~__context ~self ~value:x;
      if not task_t.Task.cancellable then begin
        let allowed_operations = Db.Task.get_allowed_operations ~__context ~self in
        if List.mem `cancel allowed_operations then begin
          let allowed_operations' = List.filter (fun x -> x <> `cancel) allowed_operations in
          debug "Set task %s to not cancellable." (Ref.really_pretty_and_small self);
          Db.Task.set_allowed_operations ~__context ~self ~value:allowed_operations'
        end
      end
    | _ -> ()
  with Not_found ->
    (* Since this is called on all tasks, possibly after the task has been
       		   destroyed, it's safe to ignore a Not_found exception here. *)
    ()
     | e ->
       error "xenopsd event: Caught %s while updating task" (string_of_exn e)

let rec events_watch ~__context cancel queue_name from =
  let dbg = Context.string_of_task __context in
  if Xapi_fist.delay_xenopsd_event_threads () then Thread.delay 30.0;
  let module Client = (val make_client queue_name : XENOPS) in
  let barriers, events, next = Client.UPDATES.get dbg from None in
  if !cancel then raise (Api_errors.Server_error(Api_errors.task_cancelled, []));
  let done_events = ref [] in
  let already_done x = List.mem x !done_events in
  let add_event x = done_events := (x :: !done_events) in
  let do_updates l =
    let open Dynamic in
    List.iter
      (fun ev ->
         debug "Processing event: %s" (ev |> Dynamic.rpc_of_id |> Jsonrpc.to_string);
         if (already_done ev) then
           debug "Skipping (already processed this round)"
         else begin
           add_event ev;
           match ev with
           | Vm id ->
             debug "xenops event on VM %s" id;
             update_vm ~__context id
           | Vbd id ->
             debug "xenops event on VBD %s.%s" (fst id) (snd id);
             update_vbd ~__context id
           | Vif id ->
             debug "xenops event on VIF %s.%s" (fst id) (snd id);
             update_vif ~__context id
           | Pci id ->
             debug "xenops event on PCI %s.%s" (fst id) (snd id);
             update_pci ~__context id
           | Vgpu id ->
             debug "xenops event on VGPU %s.%s" (fst id) (snd id);
             update_vgpu ~__context id
           | Vusb id ->
             debug "xenops event on VUSB %s.%s" (fst id) (snd id);
             update_vusb ~__context id
           | Task id ->
             debug "xenops event on Task %s" id;
             update_task ~__context queue_name id
         end) l
  in
  List.iter (fun (id,b_events) ->
      debug "Processing barrier %d" id;
      do_updates b_events;
      Events_from_xenopsd.wakeup queue_name dbg id) barriers;
  do_updates events;
  events_watch ~__context cancel queue_name (Some next)

let events_from_xenopsd queue_name =
  Server_helpers.exec_with_new_task (Printf.sprintf "%s events" queue_name)
    (fun __context ->
       while true do
         try
           events_watch ~__context (ref false) queue_name None;
         with e ->
           error "%s event thread caught: %s" queue_name (string_of_exn e);
           Thread.delay 10.
       done
    )

let refresh_vm ~__context ~self =
  let id = id_of_vm ~__context ~self in
  info "xenops: UPDATES.refresh_vm %s" id;
  let dbg = Context.string_of_task __context in
  let queue_name = queue_of_vm ~__context ~self in
  let module Client = (val make_client queue_name : XENOPS) in
  Client.UPDATES.refresh_vm dbg id;
  Events_from_xenopsd.wait queue_name dbg id ()

let resync_resident_on ~__context =
  let dbg = Context.string_of_task __context in
  let localhost = Helpers.get_localhost ~__context in
  let domain0 = Helpers.get_domain_zero ~__context in

  (* Get a list of all the ids of VMs that Xapi thinks are resident here
     (apart from domain0, which is not managed by xenopsd and is therefore
     irrelevant here) *)
  let resident_vms_in_db =
    Db.Host.get_resident_VMs ~__context ~self:localhost
    |> List.filter (fun self -> self <> domain0 )
    |> List.map (fun self -> (id_of_vm ~__context ~self, self)) in

  (* Get a list of VMs that the xenopsds know about with their xenopsd client *)
  let vms_in_xenopsds =
    List.map (fun queue_name ->
        let module Client = (val make_client queue_name : XENOPS) in
        let vms = Client.VM.list dbg () in
        List.map (fun (vm, state) -> ((vm.Vm.id, state), queue_name)) vms
      ) (all_known_xenopsds ())
    |> List.flatten in

  (* The list of VMs xenopsd knows about that (xapi knows about at all,
     xapi has no idea about at all) *)
  let xenopsd_vms_in_xapi, xenopsd_vms_not_in_xapi =
    List.partition (fun ((id, _), _) ->
        try vm_of_id ~__context id |> ignore; true with _ -> false
      ) vms_in_xenopsds in

  (* Of the VMs xapi knows about, partition that set into VMs xapi believes
     should be running here, and those that it didn't *)
  let xapi_thinks_are_here, xapi_thinks_are_not_here =
    List.partition (fun ((id, _), _) ->
        List.exists (fun (id', _) -> id=id') resident_vms_in_db)
      xenopsd_vms_in_xapi in

  (* Of those xapi thinks aren't here, are any running on another host? If
     so, kill the VM here. If they aren't running on another host (to the
     best of our knowledge), set the resident_on to be here. *)
  let xapi_thinks_are_elsewhere, xapi_thinks_are_nowhere =
    List.partition (fun ((id, _), _) ->
        let vm_ref = vm_of_id ~__context id in
        Db.is_valid_ref __context (Db.VM.get_resident_on ~__context ~self:vm_ref)
      ) xapi_thinks_are_not_here in

  (* This is the list of VMs xapi thought were running here, but actually
     aren't *)
  let xapi_vms_not_in_xenopsd =
    List.filter (fun (id, _) ->
        not (List.exists (fun ((id', _), _) -> id' = id) vms_in_xenopsds)
      ) resident_vms_in_db in

  (* Log the state before we do anything *)
  let maybe_log_em msg prefix l =
    if List.length l > 0 then begin
      debug "%s" msg;
      List.iter (fun ((id,_),queue) -> debug "%s %s (%s)" prefix id queue) l
    end
  in


  maybe_log_em
    "The following VMs are known to xenopsd that xapi does not know about"
    "In xenopsd but unknown to xapi: "
    xenopsd_vms_not_in_xapi;

  maybe_log_em
    "The following VMs are known to xenopsd but xapi thinks are running elsewhere."
    "In xenopsd but resident elsewhere: "
    xapi_thinks_are_elsewhere; (* This is bad if they're running! *)

  maybe_log_em
    "The following VMs are known to xenopsd but xapi thinks are running nowhere."
    "In xenopsd but resident nowhere: "
    xapi_thinks_are_nowhere; (* This is pretty bad! *)

  if List.length xapi_vms_not_in_xenopsd > 0 then begin
    debug "The following VMs are not known to xenopsd, but xapi thought they should have been";
    List.iter (fun (id,_) -> debug "Should have been known to xenopsd: %s" id) xapi_vms_not_in_xenopsd
  end;

  (* Destroy any VMs running that aren't in Xapi's database, or that xapi
     thinks are running on another host *)
  List.iter (fun ((id, state), queue_name) ->
      let module Client = (val make_client queue_name : XENOPS) in
      info "VM %s is known to xenopsd but isn't supposed to be: terminating" id;
      if state.Vm.power_state <> Halted then begin
        info "VM %s was actually running. This can cause data corruption, therefore terminating" id;
        Client.VM.shutdown dbg id None |> wait_for_task queue_name dbg |> ignore
      end;
      Client.VM.remove dbg id
    ) (xenopsd_vms_not_in_xapi @ xapi_thinks_are_elsewhere);

  (* Sync resident_on state in Xapi for VMs running by local Xenopsds that
     xapi didn't think were anywhere. We set resident_on to be this host so that
     the events thread will be aware of it. If it's not running, the events thread will
     remove the metadata from xenopsd and reset resident_on. *)
  List.iter (fun ((id, state), queue_name) ->
      let vm = vm_of_id ~__context id in
      info "Setting resident_on for VM %s to be this host as xenopsd is aware of it" id;
      Db.VM.set_resident_on ~__context ~self:vm ~value:localhost)
    xapi_thinks_are_nowhere;

  List.iter (fun ((id, state), _queue_name) ->
      match xenapi_of_xenops_power_state (Some state.Vm.power_state) with
      | `Running | `Paused -> add_caches id;
      | _ -> ()
    ) xenopsd_vms_in_xapi;

  (* Sync VM state in Xapi for VMs not running on this host *)
  List.iter (fun (id, vm) ->
      info "VM %s was marked as resident here in the DB but isn't known to xenopsd. Resetting in DB" id;
      Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted;
      Db.VM.set_resident_on ~__context ~self:vm ~value:Ref.null;
    ) xapi_vms_not_in_xenopsd

let resync_all_vms ~__context =
  (* This should now be correct *)
  let localhost = Helpers.get_localhost ~__context in
  let domain0 = Helpers.get_domain_zero ~__context in
  let resident_vms_in_db =
    Db.Host.get_resident_VMs ~__context ~self:localhost |>
    List.filter (fun self -> self <> domain0)
  in
  List.iter (fun vm -> refresh_vm ~__context ~self:vm) resident_vms_in_db

let on_xapi_restart ~__context =
  resync_resident_on ~__context;
  (* For all available xenopsds, start the event thread. This will cause
     events on everything xenopsd knows about, hence a refresh of all VMs. *)
  List.iter (fun queue_name ->
      let (_: Thread.t) = Thread.create events_from_xenopsd queue_name in
      ()
    ) (all_known_xenopsds ());

  resync_all_vms ~__context

let assert_resident_on ~__context ~self =
  let localhost = Helpers.get_localhost ~__context in
  if not (Db.VM.get_resident_on ~__context ~self = localhost) then
    raise Api_errors.(Server_error(internal_error,
                                   [Printf.sprintf "the VM %s is not resident on this host" (Ref.string_of self)]))

module Events_from_xapi = struct
  let greatest_token = ref ""
  let c = Condition.create ()
  let m = Mutex.create ()

  let wait ~__context ~self =
    assert_resident_on ~__context ~self;
    let t = Helpers.call_api_functions ~__context
        (fun rpc session_id ->
           XenAPI.Event.inject ~rpc ~session_id ~_class:"VM" ~_ref:(Ref.string_of self)
        ) in
    debug "Waiting for token greater than: %s" t;
    Mutex.execute m
      (fun () ->
         while !greatest_token < t do Condition.wait c m done
      )

  let broadcast new_token =
    Mutex.execute m
      (fun () ->
         greatest_token := new_token;
         Condition.broadcast c
      )
end

(* XXX: PR-1255: this will be receiving too many events and we may wish to synchronise
   updates to the VM metadata and resident_on fields *)
(* XXX: PR-1255: we also want to only listen for events on VMs and fields we care about *)
let events_from_xapi () =
  let open Event_types in
  Server_helpers.exec_with_new_task "xapi events"
    (fun __context ->
       let localhost = Helpers.get_localhost ~__context in
       let token = ref "" in
       while true do
         try
           Helpers.call_api_functions ~__context
             (fun rpc session_id ->
                trigger_xenapi_reregister :=
                  (fun () ->
                     try
                       (* This causes Event.next () and Event.from () to return SESSION_INVALID *)
                       debug "triggering xapi event thread to re-register via session.logout";
                       XenAPI.Session.logout ~rpc ~session_id
                     with
                     | Api_errors.Server_error(code, _) when code = Api_errors.session_invalid ->
                       debug "Event thread has already woken up"
                     | e ->
                       error "Waking up the xapi event thread: %s" (string_of_exn e)
                  );
                (* We register for events on resident_VMs only *)
                let resident_VMs = Db.Host.get_resident_VMs ~__context ~self:localhost in

                let uuids = List.map (fun self -> Db.VM.get_uuid ~__context ~self) resident_VMs in
                let cached = Xenops_cache.list () in
                let missing_in_cache = Listext.List.set_difference uuids cached in
                let extra_in_cache = Listext.List.set_difference cached uuids in
                if missing_in_cache <> []
                then error "events_from_xapi: missing from the cache: [ %s ]" (String.concat "; " missing_in_cache);
                if extra_in_cache <> []
                then error "events_from_xapi: extra items in the cache: [ %s ]" (String.concat "; " extra_in_cache);

                let classes = List.map (fun x -> Printf.sprintf "VM/%s" (Ref.string_of x)) resident_VMs in
                (* NB we re-use the old token so we don't get events we've already
                   							   received BUT we will not necessarily receive events for the new VMs *)

                while true do
                  let api_timeout = 60. in
                  let timeout = 30. +. api_timeout +. !Db_globs.master_connection_reset_timeout in
                  let timebox_rpc = Helpers.make_timeboxed_rpc ~__context timeout in
                  let from =
                    try
                      XenAPI.Event.from
                        ~rpc:timebox_rpc
                        ~session_id ~classes
                        ~token:!token
                        ~timeout:api_timeout
                      |> event_from_of_rpc
                    with e ->
                      Debug.log_backtrace e (Backtrace.get e);
                      raise e
                  in
                  if List.length from.events > 200 then warn "Warning: received more than 200 events!";
                  List.iter
                    (function
                      | { ty = "vm"; reference = vm' } ->
                        let vm = Ref.of_string vm' in
                        begin
                          try
                            let id = id_of_vm ~__context ~self:vm in
                            let resident_here = Db.VM.get_resident_on ~__context ~self:vm = localhost in
                            debug "Event on VM %s; resident_here = %b" id resident_here;
                            if resident_here
                            then Xenopsd_metadata.update ~__context ~self:vm |> ignore
                          with e ->
                            if not(Db.is_valid_ref __context vm)
                            then debug "VM %s has been removed: event on it will be ignored" (Ref.string_of vm)
                            else begin
                              error "Caught %s while processing XenAPI event for VM %s" (Printexc.to_string e) (Ref.string_of vm);
                              raise e
                            end
                        end
                      | _ -> warn "Received event for something we didn't register for!"
                    ) from.events;
                  token := from.token;
                  Events_from_xapi.broadcast !token;
                done
             )
         with
         | Api_errors.Server_error(code, _) when code = Api_errors.session_invalid ->
           debug "Woken event thread: updating list of event subscriptions"
         | e ->
           debug "Caught %s listening to events from xapi" (string_of_exn e);
           (* Start from scratch *)
           token := "";
           Thread.delay 15.
       done
    )

let success_task queue_name f dbg id =
  let module Client = (val make_client queue_name : XENOPS) in
  finally
    (fun () ->
       let t = Client.TASK.stat dbg id in
       match t.Task.state with
       | Task.Completed r -> f t;r.Task.result
       | Task.Failed x ->
         let exn =
           match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty x with
           | Ok e -> Xenopsd_error e
           | Error (`Msg m) -> failwith (Printf.sprintf "Internal error unmarshalling error from xenopsd: %s" m)
         in
         let bt = Backtrace.t_of_sexp (Sexplib.Sexp.of_string t.Task.backtrace) in
         Backtrace.add exn bt;
         raise exn
       | Task.Pending _ -> failwith "task pending"
    ) (fun () -> Client.TASK.destroy dbg id)

(* Catch any uncaught xenops exceptions and transform into the most relevant XenAPI error.
   We do not want a XenAPI client to see a raw xenopsd error. *)
let transform_xenops_exn ~__context ~vm queue_name f =
  try
    f ()
  with e ->
    Backtrace.is_important e;
    let reraise code params =
      error "Re-raising as %s [ %s ]" code (String.concat "; " params);
      let e' = Api_errors.Server_error(code, params) in
      Backtrace.reraise e e' in
    let internal fmt = Printf.kprintf
        (fun x ->
           reraise Api_errors.internal_error [ x ]
        ) fmt in
    begin match e with
      | Xenopsd_error e' -> begin
          match e' with 
          | Internal_error msg -> internal "xenopsd internal error: %s" msg
          | Already_exists(thing, id) -> internal "Object with type %s and id %s already exists in xenopsd" thing id
          | Does_not_exist(thing, id) -> internal "Object with type %s and id %s does not exist in xenopsd" thing id
          | Unimplemented(fn) -> reraise Api_errors.not_implemented [ fn ]
          | Domain_not_built -> internal "domain has not been built"
          | Invalid_vcpus n -> internal "the maximum number of vcpus configured for this VM is currently: %d" n
          | Bad_power_state(found, expected) ->
            let f x = xenapi_of_xenops_power_state (Some x) |> Record_util.power_state_to_string in
            let found = f found and expected = f expected in
            reraise Api_errors.vm_bad_power_state [ Ref.string_of vm; expected; found ]
          | Failed_to_acknowledge_shutdown_request ->
            reraise Api_errors.vm_failed_shutdown_ack [ Ref.string_of vm ]
          | Failed_to_shutdown(id, timeout) ->
            reraise Api_errors.vm_shutdown_timeout [ vm_of_id ~__context id |> Ref.string_of; string_of_float timeout ]
          | Device_is_connected ->
            internal "Cannot remove device because it is connected to a VM"
          | Device_not_connected ->
            internal "Device is not connected"
          | Device_detach_rejected(cls, id, msg) ->
            reraise Api_errors.device_detach_rejected [ cls; id; msg ]
          | Media_not_ejectable -> internal "the media in this drive cannot be ejected"
          | Media_present -> internal "there is already media in this drive"
          | Media_not_present -> internal "there is no media in this drive"
          | No_bootable_device -> internal "there is no bootable device"
          | Bootloader_error (uuid, msg) ->
            let vm = Db.VM.get_by_uuid ~__context ~uuid in
            reraise Api_errors.bootloader_failed [Ref.string_of vm; msg]
          | Cannot_free_this_much_memory(needed, free) ->
            reraise Api_errors.host_not_enough_free_memory [ Int64.to_string needed; Int64.to_string free ]
          | Vms_failed_to_cooperate vms ->
            let vms' = List.map (fun uuid -> Db.VM.get_by_uuid ~__context ~uuid |> Ref.string_of) vms in
            reraise Api_errors.vms_failed_to_cooperate vms'
          | IO_error -> reraise Api_errors.vdi_io_error ["I/O error saving VM suspend image"]
          | Failed_to_contact_remote_service x -> reraise Api_errors.vm_migrate_contact_remote_service_failed []
          | Hook_failed(script, reason, stdout, i) -> reraise Api_errors.xapi_hook_failed [ script; reason; stdout; i ]
          | Not_enough_memory needed -> internal "there was not enough memory (needed %Ld bytes)" needed
          | Cancelled id ->
            let task =
              try
                TaskHelper.id_to_task_exn (TaskHelper.Xenops (queue_name, id))
              with _ ->
                debug "xenopsd task id %s is not associated with a XenAPI task" id;
                Ref.null in
            reraise Api_errors.task_cancelled [ Ref.string_of task ]
          | Storage_backend_error(code, params) -> reraise code params
          | PCIBack_not_loaded -> internal "pciback has not loaded"
          | Failed_to_start_emulator (uuid, name, msg) ->
            let vm = Db.VM.get_by_uuid ~__context ~uuid in
            reraise Api_errors.failed_to_start_emulator [Ref.string_of vm; name; msg]
          | Ballooning_timeout_before_migration ->
            reraise Api_errors.ballooning_timeout_before_migration [Ref.string_of vm]
          | Unknown_error ->
            internal "Unknown error returned from xenopsd"
          | Failed_to_run_script reason ->
            reraise Api_errors.xenapi_plugin_failure [ reason ]
        end
      | e -> raise e
    end

(* After this function is called, locally-generated events will be reflected
   in the xapi pool metadata. When this function returns we believe that the
   VM state is in 'sync' with xenopsd and the pool master where by 'sync'
   we mean that all changes will eventually be propagated or 'no events lost'.
   This function assumes there is no event suppression going on. This is
   not true for the localhost migration case, but this is safe as the sender
   will synchronise state when it's finished anyway. In the other cases where
   this function is called, the VM is only just starting here, so there
   should not be any other suppression going on. *)

let set_resident_on ~__context ~self =
  let id = id_of_vm ~__context ~self in
  debug "VM %s set_resident_on" id;
  let localhost = Helpers.get_localhost ~__context in
  Helpers.call_api_functions ~__context

    (fun rpc session_id -> XenAPI.VM.atomic_set_resident_on rpc session_id self localhost);
  debug "Signalling xenapi event thread to re-register, and xenopsd events to sync";
  refresh_vm ~__context ~self;
  !trigger_xenapi_reregister ();
  (* Any future XenAPI updates will trigger events, but we might have missed one so: *)
  Xenopsd_metadata.update ~__context ~self

let update_debug_info __context t =
  let task = Context.get_task_id __context in
  let debug_info = List.map (fun (k, v) -> "debug_info:" ^ k, v) t.Task.debug_info in
  List.iter
    (fun (k, v) ->
       try
         Db.Task.add_to_other_config ~__context ~self:task ~key:k ~value:v
       with e ->
         debug "Failed to add %s = %s to task %s: %s" k v (Ref.string_of task) (Printexc.to_string e)
    ) debug_info

let sync_with_task_result __context ?cancellable queue_name x =
  let dbg = Context.string_of_task __context in
  x |> register_task __context ?cancellable queue_name |> wait_for_task queue_name dbg |> unregister_task __context queue_name |> success_task queue_name (update_debug_info __context) dbg

let sync_with_task __context ?cancellable queue_name x = sync_with_task_result __context ?cancellable queue_name x |> ignore

let sync __context queue_name x =
  let dbg = Context.string_of_task __context in
  x |> wait_for_task queue_name dbg |> success_task queue_name (update_debug_info __context) dbg |> ignore

let pause ~__context ~self =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.pause %s" id;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VM.pause dbg id |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ();
       Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self ~expected:`Paused
    )

let unpause ~__context ~self =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.unpause %s" id;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VM.unpause dbg id |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ();
       check_power_state_is ~__context ~self ~expected:`Running
    )

let request_rdp ~__context ~self enabled =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.request_rdp %s %b" id enabled;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VM.request_rdp dbg id enabled |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ()
    )

let run_script ~__context ~self script =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.run_script %s %s" id script;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       let r = Client.VM.run_script dbg id script |> sync_with_task_result __context queue_name in
       let r = match r with None -> "" | Some rpc -> Jsonrpc.to_string rpc in
       Events_from_xenopsd.wait queue_name dbg id ();
       r
    )

let set_xenstore_data ~__context ~self xsdata =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.set_xenstore_data %s" id;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VM.set_xsdata dbg id xsdata |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ();
    )

let set_vcpus ~__context ~self n =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.set_vcpus %s" id;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       try
         Client.VM.set_vcpus dbg id (Int64.to_int n) |> sync_with_task __context queue_name;
         Db.VM.set_VCPUs_at_startup ~__context ~self ~value:n;
         let metrics = Db.VM.get_metrics ~__context ~self in
         if metrics <> Ref.null then
           Db.VM_metrics.set_VCPUs_number ~__context ~self:metrics ~value:n;
         Events_from_xenopsd.wait queue_name dbg id ();
       with
       | Xenopsd_error (Invalid_vcpus n) ->
         raise (Api_errors.Server_error(Api_errors.invalid_value, [
             "VCPU values must satisfy: 0 < VCPUs ≤ VCPUs_max";
             string_of_int n
           ]))
    )

let set_shadow_multiplier ~__context ~self target =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.set_shadow_multiplier %s" id;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       try
         Client.VM.set_shadow_multiplier dbg id target |> sync_with_task __context queue_name;
         Events_from_xenopsd.wait queue_name dbg id ();
       with
       | Xenopsd_error (Not_enough_memory needed) ->
         let host = Db.VM.get_resident_on ~__context ~self in
         let free_mem_b = Memory_check.host_compute_free_memory_with_maximum_compression ~__context ~host None in
         raise (Api_errors.Server_error(Api_errors.host_not_enough_free_memory, [ Int64.to_string needed; Int64.to_string free_mem_b ]))
       | Xenopsd_error (Unimplemented _) ->
         (* The existing behaviour is to ignore this failure *)
         error "VM.set_shadow_multiplier: not supported for PV domains"
    )

let set_memory_dynamic_range ~__context ~self min max =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       debug "xenops: VM.set_memory_dynamic_range %s" id;
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VM.set_memory_dynamic_range dbg id min max |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ()
    )

let maybe_cleanup_vm ~__context ~self =
  let dbg = Context.string_of_task __context in
  let queue_name = queue_of_vm ~__context ~self in
  let id = id_of_vm ~__context ~self in
  if vm_exists_in_xenopsd queue_name dbg id then begin
    warn "Stale VM detected in Xenopsd, flushing outstanding events";
    (* By calling with_events_suppressed we can guarentee that an refresh_vm
       		 * will be called with events enabled and therefore we get Xenopsd into a
       		 * consistent state with Xapi *)
    Events_from_xenopsd.with_suppressed queue_name dbg id (fun _ -> ());
    Xenopsd_metadata.delete ~__context id;
  end

let start ~__context ~self paused force =
  let dbg = Context.string_of_task __context in
  let queue_name = queue_of_vm ~__context ~self in
  let vm_id = id_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      maybe_cleanup_vm ~__context ~self;
      if vm_exists_in_xenopsd queue_name dbg vm_id then
        raise (Xenopsd_error (Bad_power_state (Running, Halted)));
      (* For all devices which we want xenopsd to manage, set currently_attached = true
         		   so the metadata is pushed. *)
      let empty_vbds_allowed = Helpers.will_have_qemu ~__context ~self in
      let vbds =
        (* xenopsd only manages empty VBDs for HVM guests *)
        let vbds = Db.VM.get_VBDs ~__context ~self in
        if empty_vbds_allowed then vbds else (List.filter (fun self -> not(Db.VBD.get_empty ~__context ~self)) vbds) in
      List.iter (fun self -> Db.VBD.set_currently_attached ~__context ~self ~value:true) vbds;
      List.iter (fun self -> Db.VIF.set_currently_attached ~__context ~self ~value:true) (Db.VM.get_VIFs ~__context ~self);
      List.iter (fun self -> Db.VUSB.set_currently_attached ~__context ~self ~value:true) (Db.VM.get_VUSBs ~__context ~self);

      let module Client = (val make_client queue_name : XENOPS) in
      debug "Sending VM %s configuration to xenopsd" (Ref.string_of self);
      try
        let id = Xenopsd_metadata.push ~__context ~self in
        Xapi_network.with_networks_attached_for_vm ~__context ~vm:self (fun () ->
            info "xenops: VM.start %s" id;
            if not paused then begin
              let vm_start = Client.VM.start dbg id force in
              info "xenops: Queueing VM.unpause %s" id;
              let vm_unpause = Client.VM.unpause dbg id in
              begin
                try
                  sync_with_task __context queue_name vm_start;
                with e ->
                  (* If the VM.start throws an error, clean up the unpause
                     					     which will fail in an irrelevant manor, then reraise
                     					     the original error *)
                  begin
                    try sync __context queue_name vm_unpause with _ -> ()
                  end;
                  raise e
              end;

              (* At this point, the start paused has succeeded. Now
                 					   we _do_ care about any error from unpause *)

              sync_with_task __context queue_name vm_unpause
            end else
              Client.VM.start dbg id force |> sync_with_task __context queue_name);

        set_resident_on ~__context ~self;
        (* set_resident_on syncs both xenopsd and with the xapi event mechanism *)
        check_power_state_is ~__context ~self ~expected:(if paused then `Paused else `Running)
      with e ->
        error "Caught exception starting VM: %s" (string_of_exn e);
        set_resident_on ~__context ~self;
        raise e
    )

let start ~__context ~self paused force =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       try
         start ~__context ~self paused force
       with Xenopsd_error (Bad_power_state(found, expected)) as e ->
         Backtrace.is_important e;
         let power_state = function
           | Running -> "Running"
           | Halted -> "Halted"
           | Suspended -> "Suspended"
           | Paused -> "Paused" in
         let exn = Api_errors.Server_error(Api_errors.vm_bad_power_state,
                                           [ Ref.string_of self; power_state found; power_state expected ]) in
         Backtrace.reraise e exn
    )

let reboot ~__context ~self timeout =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       assert_resident_on ~__context ~self;
       let id = id_of_vm ~__context ~self in
       let dbg = Context.string_of_task __context in
       maybe_cleanup_vm ~__context ~self;
       (* If Xenopsd no longer knows about the VM after cleanup it was shutdown.
          			   This also means our caches have been removed. *)
       if not (vm_exists_in_xenopsd queue_name dbg id) then
         raise (Xenopsd_error (Bad_power_state (Halted, Running)));
       (* Ensure we have the latest version of the VM metadata before the reboot *)
       Events_from_xapi.wait ~__context ~self;
       info "xenops: VM.reboot %s" id;
       let module Client = (val make_client queue_name : XENOPS ) in
       let () = Pervasiveext.finally
           (fun () ->
              Client.VM.reboot dbg id timeout |> sync_with_task __context queue_name)
           (fun () ->
              Events_from_xenopsd.wait queue_name dbg id ())
       in
       check_power_state_is ~__context ~self ~expected:`Running
    )

let shutdown ~__context ~self timeout =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       assert_resident_on ~__context ~self;
       let id = id_of_vm ~__context ~self in
       let dbg = Context.string_of_task __context in
       info "xenops: VM.shutdown %s" id;
       let module Client = (val make_client queue_name : XENOPS ) in
       let () = Pervasiveext.finally
           (fun () ->
              Client.VM.shutdown dbg id timeout |> sync_with_task __context queue_name)
           (fun () ->
              Events_from_xenopsd.wait queue_name dbg id ())
       in
       Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self ~expected:`Halted;
       (* force_state_reset called from the xenopsd event loop above *)
       if not(Db.VM.get_resident_on ~__context ~self = Ref.null) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "shutdown: The VM %s is still resident on the host" (Ref.string_of self)]));

       List.iter
         (fun vbd ->
            if (Db.VBD.get_currently_attached ~__context ~self:vbd) then
              raise Api_errors.(Server_error(internal_error, [
                  Printf.sprintf "shutdown: The VBD %s is still attached to VM %s"
                    (Ref.string_of vbd) (Ref.string_of self)]))
         ) (Db.VM.get_VBDs ~__context ~self)
    )

let suspend ~__context ~self =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       assert_resident_on ~__context ~self;
       let id = id_of_vm ~__context ~self in
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       let vm_t, state = Client.VM.stat dbg id in
       (* XXX: this needs to be at boot time *)
       let space_needed =
         let ram = Int64.(of_float (to_float vm_t.Vm.memory_static_max *. 1.2 *. 1.05)) in
         let vgpu =
           Db.VM.get_VGPUs ~__context ~self
           |> List.map (fun self -> Db.VGPU.get_type ~__context ~self)
           |> List.map (fun self -> Db.VGPU_type.get_framebuffer_size ~__context ~self)
           |> List.fold_left Int64.add 0L
         in
         Int64.(ram |> add vgpu |> add 104857600L)
       in
       let suspend_SR = Helpers.choose_suspend_sr ~__context ~vm:self in
       let sm_config = [
         Xapi_globs._sm_vm_hint, id;
         (* Fully inflate the VDI if the SR supports thin provisioning *)
         Xapi_globs._sm_initial_allocation, (Int64.to_string space_needed);
       ] in
       Helpers.call_api_functions ~__context
         (fun rpc session_id ->
            let vdi =
              XenAPI.VDI.create ~rpc ~session_id
                ~name_label:"Suspend image"
                ~name_description:"Suspend image"
                ~sR:suspend_SR ~virtual_size:space_needed
                ~sharable:false ~read_only:false ~_type:`suspend
                ~other_config:[] ~xenstore_data:[] ~sm_config ~tags:[] in
            let d = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
            Db.VM.set_suspend_VDI ~__context ~self ~value:vdi;
            try
              let dbg = Context.string_of_task __context in
              info "xenops: VM.suspend %s to %s" id (d |> rpc_of disk |> Jsonrpc.to_string);
              Client.VM.suspend dbg id d |> sync_with_task __context ~cancellable:false queue_name;
              Events_from_xenopsd.wait queue_name dbg id ();
              Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self ~expected:`Suspended;
              if not(Db.VM.get_resident_on ~__context ~self = Ref.null) then
                raise Api_errors.(Server_error(internal_error, [
                    Printf.sprintf "suspend: The VM %s is still resident on the host" (Ref.string_of self)]));
            with e ->
              error "Caught exception suspending VM: %s" (string_of_exn e);
              (* If the domain has suspended, we have to shut it down *)
              Events_from_xenopsd.wait queue_name dbg id ();
              if Db.VM.get_power_state ~__context ~self = `Suspended then begin
                info "VM has already suspended; we must perform a hard_shutdown";
                Xapi_vm_lifecycle.force_state_reset ~__context ~self ~value:`Halted;
                !trigger_xenapi_reregister ();
              end else info "VM is still running after failed suspend";
              XenAPI.VDI.destroy ~rpc ~session_id ~self:vdi;
              Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null;
              raise e
         )
    )

let resume ~__context ~self ~start_paused ~force =
  let dbg = Context.string_of_task __context in
  let queue_name = queue_of_vm ~__context ~self in
  let vm_id = id_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       maybe_cleanup_vm ~__context ~self;
       if vm_exists_in_xenopsd queue_name dbg vm_id then
         raise (Xenopsd_error (Bad_power_state (Running, Suspended)));
       let vdi = Db.VM.get_suspend_VDI ~__context ~self in
       if vdi = Ref.null then begin
         info "VM suspend VDI not found; Performing VM hard_shutdown";
         Xapi_vm_lifecycle.force_state_reset ~__context ~self ~value:`Halted;
         raise Api_errors.(Server_error(vm_has_no_suspend_vdi, ["VM"; Ref.string_of self]))
       end;
       let d = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
       let module Client = (val make_client queue_name : XENOPS) in
       (* NB we don't set resident_on because we don't want to
          			   modify the VM.power_state, {VBD,VIF}.currently_attached in the
          			   failures cases. This means we must remove the metadata from
          			   xenopsd on failure. *)
       begin try
           Events_from_xenopsd.with_suppressed queue_name dbg vm_id
             (fun () ->
                debug "Sending VM %s configuration to xenopsd" (Ref.string_of self);
                let id = Xenopsd_metadata.push ~__context ~self in
                Xapi_network.with_networks_attached_for_vm ~__context ~vm:self
                  (fun () ->
                     info "xenops: VM.resume %s from %s" id (d |> rpc_of disk |> Jsonrpc.to_string);
                     Client.VM.resume dbg id d |> sync_with_task __context ~cancellable:false queue_name;
                     if not start_paused then begin
                       info "xenops: VM.unpause %s" id;
                       Client.VM.unpause dbg id |> sync_with_task __context ~cancellable:false queue_name;
                     end;
                  )
             )
         with e ->
           error "Caught exception resuming VM: %s" (string_of_exn e);
           let id = id_of_vm ~__context ~self in
           Xenopsd_metadata.delete ~__context id;
           raise e
       end;
       set_resident_on ~__context ~self;
       Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null;
       (* Clearing vGPU metadata should happen as late as possible
        * to make sure we only do it on a successful resume
       *)
       Xapi_gpumon.clear_vgpu_metadata ~__context ~vm:self;
       Helpers.call_api_functions ~__context
         (fun rpc session_id ->
            XenAPI.VDI.destroy rpc session_id vdi
         );
       check_power_state_is ~__context ~self ~expected:(if start_paused then `Paused else `Running)
    )

let s3suspend ~__context ~self =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       debug "xenops: VM.s3suspend %s" id;
       Client.VM.s3suspend dbg id |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ()
    )

let s3resume ~__context ~self =
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name
    (fun () ->
       let id = id_of_vm ~__context ~self in
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       debug "xenops: VM.s3resume %s" id;
       Client.VM.s3resume dbg id |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg id ()
    )

let md_of_vbd ~__context ~self =
  let vm = Db.VBD.get_VM ~__context ~self in
  MD.of_vbd ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vbd:(Db.VBD.get_record ~__context ~self)

let vbd_plug ~__context ~self =
  let vm = Db.VBD.get_VM ~__context ~self in
  let vm_id = id_of_vm ~__context ~self:vm in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       Events_from_xapi.wait ~__context ~self:vm;
       let vbd = md_of_vbd ~__context ~self in
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Events_from_xenopsd.with_suppressed queue_name dbg vm_id (fun () ->
           info "xenops: VBD.add %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
           let id = Client.VBD.add dbg vbd in
           info "xenops: VBD.plug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
           Client.VBD.plug dbg id |> sync_with_task __context queue_name;
         );
       if not (Db.VBD.get_currently_attached ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vbd_plug: Unable to plug VBD %s" (Ref.string_of self)]))
    )

let vbd_unplug ~__context ~self force =
  let vm = Db.VBD.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vbd = md_of_vbd ~__context ~self in
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       begin
         try
           info "xenops: VBD.unplug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
           Client.VBD.unplug dbg vbd.Vbd.id force |> sync_with_task __context queue_name;
         with Xenopsd_error (Device_detach_rejected(_, _, _)) ->
           raise Api_errors.(Server_error(device_detach_rejected, ["VBD"; Ref.string_of self; ""]))
       end;
       Events_from_xenopsd.wait queue_name dbg (fst vbd.Vbd.id) ();
       if (Db.VBD.get_currently_attached ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vbd_unplug: Unable to unplug VBD %s" (Ref.string_of self)]))
    )

let vbd_eject_hvm ~__context ~self =
  let vm = Db.VBD.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vbd = md_of_vbd ~__context ~self in
       info "xenops: VBD.eject %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VBD.eject dbg vbd.Vbd.id |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vbd.Vbd.id) ();
       Events_from_xapi.wait ~__context ~self:vm;
       if not (Db.VBD.get_empty ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vbd_eject_hvm: The VBD %s has not been emptied" (Ref.string_of self)]));
       let vdi = Db.VBD.get_VDI ~__context ~self in
       if not (vdi = Ref.null) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vbd_eject_hvm: The VBD %s is still connected to VDI %s"
               (Ref.string_of self) (Ref.string_of vdi)]))
    )

let vbd_insert_hvm ~__context ~self ~vdi =
  let vm = Db.VBD.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vbd = md_of_vbd ~__context ~self in
       let d = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
       info "xenops: VBD.insert %s.%s %s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) (d |> rpc_of disk |> Jsonrpc.to_string);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VBD.insert dbg vbd.Vbd.id d |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vbd.Vbd.id) ();
       Events_from_xapi.wait ~__context ~self:vm;
       if (Db.VBD.get_empty ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vbd_insert_hvm: The VBD %s is empty" (Ref.string_of self)]));
       let vdi' = Db.VBD.get_VDI ~__context ~self in
       if not (vdi' = vdi) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vbd_insert_hvm: The VBD %s has been connected to the wrong VDI (expected %s, got %s)"
               (Ref.string_of self) (Ref.string_of vdi) (Ref.string_of vdi)]))
    )

let has_qemu ~__context ~vm =
  let dbg = Context.string_of_task __context in
  let id = Db.VM.get_uuid ~__context ~self:vm in
  let queue_name = queue_of_vm ~__context ~self:vm in
  let module Client = (val make_client queue_name : XENOPS) in
  let _, state = Client.VM.stat dbg id in
  state.Vm.domain_type = Domain_HVM

let ejectable ~__context ~self =
  let vm = Db.VBD.get_VM ~__context ~self in
  has_qemu ~__context ~vm

let vbd_eject ~__context ~self =
  if ejectable ~__context ~self
  then vbd_eject_hvm ~__context ~self
  else begin
    vbd_unplug ~__context ~self false;
    Db.VBD.set_empty ~__context ~self ~value:true;
    Db.VBD.set_VDI ~__context ~self ~value:Ref.null;
  end

let vbd_insert ~__context ~self ~vdi =
  if ejectable ~__context ~self
  then vbd_insert_hvm ~__context ~self ~vdi
  else begin
    Db.VBD.set_VDI ~__context ~self ~value:vdi;
    Db.VBD.set_empty ~__context ~self ~value:false;
    vbd_plug ~__context ~self
  end

let md_of_vif ~__context ~self =
  let vm = Db.VIF.get_VM ~__context ~self in
  MD.of_vif ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vif:(self, Db.VIF.get_record ~__context ~self)

let vif_plug ~__context ~self =
  let vm = Db.VIF.get_VM ~__context ~self in
  let vm_id = id_of_vm ~__context ~self:vm in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       Events_from_xapi.wait ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Xapi_network.with_networks_attached_for_vm ~__context ~vm (fun () ->
           Events_from_xenopsd.with_suppressed queue_name dbg vm_id (fun () ->
               info "xenops: VIF.add %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
               let id = Client.VIF.add dbg vif in
               info "xenops: VIF.plug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
               Client.VIF.plug dbg id |> sync_with_task __context queue_name;
             );
         );
       if not (Db.VIF.get_currently_attached ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vif_plug: Unable to plug VIF %s" (Ref.string_of self)]))
    )

let vif_set_locking_mode ~__context ~self =
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       info "xenops: VIF.set_locking_mode %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VIF.set_locking_mode dbg vif.Vif.id vif.Vif.locking_mode |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ();
    )

let vif_set_pvs_proxy ~__context ~self creating =
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       let proxy = if creating then vif.Vif.pvs_proxy else None in
       info "xenops: VIF.set_pvs_proxy %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VIF.set_pvs_proxy dbg vif.Vif.id proxy |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ();
    )

let vif_unplug ~__context ~self force =
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       info "xenops: VIF.unplug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VIF.unplug dbg vif.Vif.id force |> sync_with_task __context queue_name;
       (* We need to make sure VIF.stat still works so: wait before calling VIF.remove *)
       Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ();
       if (Db.VIF.get_currently_attached ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vif_unplug: Unable to unplug VIF %s" (Ref.string_of self)]))
    )

let vif_move ~__context ~self network =
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       info "xenops: VIF.move %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
       let backend = backend_of_vif ~__context ~vif:self in
       match backend with
       | Network.Sriov _ -> raise Api_errors.(Server_error(internal_error, [
           Printf.sprintf "vif_move: Unable to move a network SR-IOV backed VIF %s"
             (Ref.string_of self)]))
       | _ ->
         let dbg = Context.string_of_task __context in
         let module Client = (val make_client queue_name : XENOPS) in
         (* Nb., at this point, the database shows the vif on the new network *)
         Xapi_network.attach_for_vif ~__context ~vif:self ();
         Client.VIF.move dbg vif.Vif.id backend |> sync_with_task __context queue_name;
         Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ();
         if not (Db.VIF.get_currently_attached ~__context ~self) then
           raise Api_errors.(Server_error(internal_error, [
               Printf.sprintf "vif_move: Unable to plug moved VIF %s" (Ref.string_of self)]))
    )

let vif_set_ipv4_configuration ~__context ~self =
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       info "xenops: VIF.set_ipv4_configuration %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VIF.set_ipv4_configuration dbg vif.Vif.id vif.Vif.ipv4_configuration |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ();
    )

let vif_set_ipv6_configuration ~__context ~self =
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vif = md_of_vif ~__context ~self in
       info "xenops: VIF.set_ipv6_configuration %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VIF.set_ipv6_configuration dbg vif.Vif.id vif.Vif.ipv6_configuration |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ();
    )

let task_cancel ~__context ~self =
  try
    let queue_name, id = TaskHelper.task_to_id_exn self |> unwrap in
    let module Client = (val make_client queue_name : XENOPS) in
    let dbg = Context.string_of_task __context in
    info "xenops: TASK.cancel %s" id;
    Client.TASK.cancel dbg id |> ignore; (* it might actually have completed, we don't care *)
    true
  with
  | Not_found -> false
  | Not_a_xenops_task -> false

let md_of_vusb ~__context ~self =
  let vm = Db.VUSB.get_VM ~__context ~self in
  let usb_group = Db.VUSB.get_USB_group ~__context ~self in
  let pusb = Helpers.get_first_pusb ~__context usb_group in
  let pusbr =Db.PUSB.get_record ~__context ~self:pusb in
  MD.of_vusb ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~pusb:pusbr

let vusb_unplug_hvm ~__context ~self =
  let vm = Db.VUSB.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name
    (fun () ->
       assert_resident_on ~__context ~self:vm;
       let vusb = md_of_vusb ~__context ~self in
       info "xenops: VUSB.unplug %s.%s" (fst vusb.Vusb.id) (snd vusb.Vusb.id);
       let dbg = Context.string_of_task __context in
       let module Client = (val make_client queue_name : XENOPS) in
       Client.VUSB.unplug dbg vusb.Vusb.id |> sync_with_task __context queue_name;
       Events_from_xenopsd.wait queue_name dbg (fst vusb.Vusb.id) ();
       if (Db.VUSB.get_currently_attached ~__context ~self) then
         raise Api_errors.(Server_error(internal_error, [
             Printf.sprintf "vusb_unplug: Unable to unplug VUSB %s" (Ref.string_of self)]))
    )

let vusb_plugable ~__context ~self =
  let vm = Db.VUSB.get_VM ~__context ~self in
  has_qemu ~__context ~vm

let vusb_unplug ~__context ~self =
  if vusb_plugable ~__context ~self then
    vusb_unplug_hvm ~__context ~self
  else
    raise Api_errors.(Server_error(internal_error, [
        Printf.sprintf "vusb_unplug: Unable to unplug vusb %s" (Ref.string_of self)]))
