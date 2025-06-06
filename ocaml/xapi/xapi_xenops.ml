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

module D = Debug.Make (struct let name = "xenops" end)

open D
module StringSet = Set.Make (String)
open Network
open Xapi_stdext_std.Xstringext
module Date = Clock.Date
module Listext = Xapi_stdext_std.Listext.List

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Unixext = Xapi_stdext_unix.Unixext
module XenAPI = Client.Client
module Rrdd = Rrd_client.Client
open Xenops_interface
open Xapi_xenops_queue

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let rpc_of t x = Rpcmarshal.marshal t.Rpc.Types.ty x

let ( let@ ) f x = f x

let check_power_state_is ~__context ~self ~expected =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
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
        (Record_util.vm_power_state_to_lowercase_string actual)
        (Record_util.vm_power_state_to_lowercase_string expected)

let event_wait queue_name dbg ?from p =
  Debug_info.with_dbg ~name:__FUNCTION__ ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let finished = ref false in
  let event_id = ref from in
  let module Client = (val make_client queue_name : XENOPS) in
  while not !finished do
    let _, deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    event_id := Some next_id ;
    List.iter (fun d -> if p d then finished := true) deltas
  done

let task_ended queue_name dbg id =
  Debug_info.with_dbg ~name:__FUNCTION__ ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let module Client = (val make_client queue_name : XENOPS) in
  match (Client.TASK.stat dbg id).Task.state with
  | Task.Completed _ | Task.Failed _ ->
      true
  | Task.Pending _ ->
      false

let wait_for_task queue_name dbg id =
  Debug_info.with_dbg ~name:__FUNCTION__ ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let module Client = (val make_client queue_name : XENOPS) in
  let finished = function
    | Dynamic.Task id' ->
        id = id' && task_ended queue_name dbg id
    | _ ->
        false
  in
  let from = Client.UPDATES.last_id dbg in
  if not (task_ended queue_name dbg id) then
    event_wait queue_name dbg ~from finished ;
  id

let xenapi_of_xenops_power_state = function
  | Some Running ->
      `Running
  | Some Halted ->
      `Halted
  | Some Suspended ->
      `Suspended
  | Some Paused ->
      `Paused
  | None ->
      `Halted

let xenops_of_xenapi_power_state = function
  | `Running ->
      Running
  | `Halted ->
      Halted
  | `Suspended ->
      Suspended
  | `Paused ->
      Paused

let xenops_vdi_locator_of sr vdi =
  Printf.sprintf "%s/%s"
    (Storage_interface.Sr.string_of sr)
    (Storage_interface.Vdi.string_of vdi)

let xenops_vdi_locator ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let sr = Db.VDI.get_SR ~__context ~self in
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
  let vdi_location = Db.VDI.get_location ~__context ~self in
  xenops_vdi_locator_of
    (Storage_interface.Sr.of_string sr_uuid)
    (Storage_interface.Vdi.of_string vdi_location)

let disk_of_vdi ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try Some (VDI (xenops_vdi_locator ~__context ~self)) with _ -> None

let vdi_of_disk ~__context x =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  match String.split ~limit:2 '/' x with
  | [sr_uuid; location] -> (
      let open Xapi_database.Db_filter_types in
      let sr = Db.SR.get_by_uuid ~__context ~uuid:sr_uuid in
      match
        Db.VDI.get_records_where ~__context
          ~expr:
            (And
               ( Eq (Field "location", Literal location)
               , Eq (Field "SR", Literal (Ref.string_of sr))
               )
            )
      with
      | x :: _ ->
          Some x
      | _ ->
          error "Failed to find VDI: %s" x ;
          None
    )
  | _ ->
      error "Failed to parse VDI name: %s" x ;
      None

let backend_of_network net =
  try
    let backend_vm = List.assoc "backend_vm" net.API.network_other_config in
    debug "Using VM %s as backend for VIF on network %s" backend_vm
      net.API.network_uuid ;
    Network.Remote (backend_vm, net.API.network_bridge)
  with Not_found -> Network.Local net.API.network_bridge

(* PR-1255 *)

let backend_of_vif ~__context ~vif =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vif_record = Db.VIF.get_record_internal ~__context ~self:vif in
  let net =
    Db.Network.get_record ~__context ~self:vif_record.Db_actions.vIF_network
  in
  let host = Helpers.get_localhost ~__context in
  let pifs =
    Xapi_network_attach_helpers.get_local_pifs ~__context
      ~network:vif_record.Db_actions.vIF_network ~host
  in
  match pifs with
  | [] ->
      backend_of_network net
  | pif :: _ ->
      let pif_rec = Db.PIF.get_record ~__context ~self:pif in
      let l = Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec in
      if
        List.exists
          (function
            | Xapi_pif_helpers.Network_sriov_logical _ -> true | _ -> false
            )
          l
      then
        if vif_record.Db_actions.vIF_reserved_pci <> Ref.null then
          let domain, bus, dev, fn =
            Pciops.pcidev_of_pci ~__context
              vif_record.Db_actions.vIF_reserved_pci
          in
          Network.Sriov {domain; bus; dev; fn}
        else
          Helpers.internal_error "No reserved_pci for network SR-IOV vif %s"
            (Ref.string_of vif)
      else
        backend_of_network net

let find f map default feature =
  try
    let v = List.assoc feature map in
    try f v
    with e ->
      warn "Failed to parse %s as value for %s: %s; Using default value." v
        feature (Printexc.to_string e) ;
      default
  with Not_found -> default

let string = find (fun x -> x)

let assume_default_if_null_empty map default feature =
  match List.assoc_opt feature map with
  | None | Some "" ->
      D.info "assuming default setting %s=%s" feature default ;
      default
  | Some x ->
      x

let int = find int_of_string

let bool platformdata default key =
  Vm_platform.is_true ~key ~platformdata ~default

let nvram_uefi_of_vm vm =
  let open Xenops_types.Nvram_uefi_variables in
  let on_field name f t =
    match List.assoc name vm.API.vM_NVRAM with
    | v ->
        f v t
    | exception Not_found ->
        t
  in
  let add_on_boot =
    on_field "EFI-variables-on-boot" (fun str t ->
        match str with
        | "persist" ->
            {t with on_boot= Persist}
        | "reset" ->
            {t with on_boot= Reset}
        | bad ->
            raise
              Api_errors.(
                Server_error
                  (invalid_value, ["NVRAM['EFI-variables-on-boot']"; bad])
              )
    )
  in
  let add_backend =
    on_field "EFI-variables-backend" (fun backend t -> {t with backend})
  in
  default_t |> add_on_boot |> add_backend

let firmware_of_vm vm =
  let open Xenops_types.Vm in
  match List.assoc "firmware" vm.API.vM_HVM_boot_params with
  | "bios" ->
      Bios
  | "uefi" ->
      Uefi (nvram_uefi_of_vm vm)
  | bad ->
      raise
        Api_errors.(
          Server_error (invalid_value, ["HVM-boot-params['firmware']"; bad])
        )
  | exception Not_found ->
      default_firmware

let varstore_rm_with_sandbox ~__context ~vm_uuid f =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let domid = 0 in
  let chroot, socket_path =
    Xenops_sandbox.Varstore_guard.start dbg ~domid ~vm_uuid ~paths:[]
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f chroot socket_path)
    (fun () -> Xenops_sandbox.Varstore_guard.stop dbg ~domid ~vm_uuid)

let nvram_post_clone ~__context ~self ~uuid =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  match Db.VM.get_NVRAM ~__context ~self with
  | [] ->
      ()
  | original ->
      let uuid = Uuidx.to_string uuid in
      info "VM %s was cloned: clearing certain UEFI variables" uuid ;
      varstore_rm_with_sandbox ~__context ~vm_uuid:uuid
        (fun chroot socket_path ->
          Forkhelpers.execute_command_get_output !Xapi_globs.varstore_rm
            [
              "-c"
            ; uuid
            ; "-r"
            ; chroot.root
            ; "-u"
            ; string_of_int chroot.uid
            ; "-g"
            ; string_of_int chroot.gid
            ; "-s"
            ; socket_path
            ]
          |> ignore
      ) ;
      if Db.VM.get_NVRAM ~__context ~self <> original then
        debug "VM %s: NVRAM changed due to clone" uuid

let rtc_timeoffset_of_vm ~__context (vm, vm_t) vbds =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
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
    |> List.filter_map (fun (reference, record) ->
           Option.map
             (fun offset -> (reference, offset))
             (List.assoc_opt Vm_platform.timeoffset record.API.vDI_other_config)
       )
  in
  match vdis_with_timeoffset_to_be_reset_on_boot with
  | [] ->
      timeoffset
  | [(_, timeoffset)] ->
      timeoffset
  | reference_timeoffset_pairs ->
      raise
        (Api_errors.Server_error
           ( Api_errors
             .vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot
           , Ref.string_of vm
             :: (reference_timeoffset_pairs
                |> List.map fst
                |> List.map Ref.string_of
                )
           )
        )

let allowed_dom0_directories_for_boot_files = ["/var/lib/xcp/guest/"]

let kernel_path filename =
  let ( let* ) = Result.bind in
  let* real_path =
    try Ok (Unix.realpath filename) with
    | Unix.(Unix_error (ENOENT, _, _)) ->
        let reason = "File does not exist" in
        Error (filename, reason)
    | exn ->
        let reason = Printexc.to_string exn in
        Error (filename, reason)
  in
  let* () =
    match Unix.stat real_path with
    | {st_kind= Unix.S_REG; _} ->
        Ok ()
    | _ ->
        let reason = "Is not a regular file" in
        Error (filename, reason)
  in
  let allowed =
    List.exists
      (fun allowed -> String.starts_with ~prefix:allowed real_path)
      allowed_dom0_directories_for_boot_files
  in
  if not allowed then
    let reason =
      Printf.sprintf "Is not in any of the allowed kernel directories: [%s]"
        (String.concat "; " allowed_dom0_directories_for_boot_files)
    in
    Error (filename, reason)
  else
    Ok real_path

let builder_of_vm ~__context (vmref, vm) timeoffset pci_passthrough vgpu =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let open Vm in
  let video_mode =
    if vgpu then
      Vgpu
    else if
      Vm_platform.is_true ~key:Vm_platform.igd_passthru_key
        ~platformdata:vm.API.vM_platform ~default:false
    then
      IGD_passthrough GVT_d
    else
      match string vm.API.vM_platform "cirrus" Vm_platform.vga with
      | "std" ->
          Standard_VGA
      | "cirrus" ->
          Cirrus
      | x ->
          error "Unknown platform/vga option: %s (expected 'std' or 'cirrus')" x ;
          Cirrus
  in
  let pci_emulations =
    let s = List.assoc_opt "mtc_pci_emulations" vm.API.vM_other_config in
    match s with
    | None ->
        []
    | Some x ->
        String.split_on_char ',' x |> List.map String.trim
  in
  let make_hvmloader_boot_record () =
    if bool vm.API.vM_platform false "qemu_stubdom" then
      warn "QEMU stub domains are no longer implemented" ;

    let tpm_of_vm () =
      let ( let* ) = Option.bind in
      let* vtpm =
        match vm.API.vM_VTPMs with
        | [] ->
            (* The vtpm parameter in platform data only has influence when the
               VM does not have a VTPM associated, otherwise the associated
               VTPM gets always attached. *)
            if bool vm.API.vM_platform false "vtpm" then
              Some (Xapi_vtpm.create ~__context ~vM:vmref ~is_unique:false)
            else
              None
        | [vtpm] ->
            Some vtpm
        | _ :: _ :: _ ->
            failwith "Multiple vTPMs are not supported"
      in
      let uuid = Db.VTPM.get_uuid ~__context ~self:vtpm in
      Some (Xenops_interface.Vm.Vtpm (Uuidm.of_string uuid |> Option.get))
    in

    {
      hap= true
    ; shadow_multiplier= vm.API.vM_HVM_shadow_multiplier
    ; timeoffset
    ; video_mib=
        ((* For vGPU, make sure videoram is at least 16MiB. *)
         let requested_videoram = int vm.API.vM_platform 4 "videoram" in
         if video_mode = Vgpu then
           max requested_videoram 16
         else
           requested_videoram
        )
    ; video= video_mode
    ; acpi= bool vm.API.vM_platform true "acpi"
    ; serial=
        ((* The platform value should override the other_config value. If
            neither are set, use pty. *)
         let key = "hvm_serial" in
         let other_config_value = List.assoc_opt key vm.API.vM_other_config in
         let platform_value = List.assoc_opt key vm.API.vM_platform in
         match (other_config_value, platform_value) with
         | None, None ->
             Some "pty"
         | _, Some value ->
             Some value
         | Some value, None ->
             Some value
        )
    ; keymap= List.assoc_opt "keymap" vm.API.vM_platform
    ; vnc_ip= None (*None PR-1255*)
    ; pci_emulations
    ; pci_passthrough
    ; boot_order=
        (* XSI-804 avoid boot orders which are the empty string, as qemu
         * will silently fail to start the VM *)
        (let open Constants in
         assume_default_if_null_empty vm.API.vM_HVM_boot_params
           hvm_default_boot_order hvm_boot_params_order
        )
    ; qemu_disk_cmdline= bool vm.API.vM_platform false "qemu_disk_cmdline"
    ; qemu_stubdom= false (* Obsolete: implementation removed *)
    ; firmware= firmware_of_vm vm
    ; tpm= tpm_of_vm ()
    }
  in
  let make_direct_boot_record {Helpers.kernel; kernel_args= ka; ramdisk} =
    let resolve name ~path =
      match kernel_path path with
      | Ok k ->
          k
      | Error (file, msg) ->
          info {|%s: refusing to load %s "%s": %s|} __FUNCTION__ name file msg ;
          raise Api_errors.(Server_error (invalid_value, [name; file; msg]))
    in
    let kernel = resolve "kernel" ~path:kernel in
    let ramdisk = Option.map (fun k -> resolve "ramdisk" ~path:k) ramdisk in
    {
      boot= Direct {kernel; cmdline= ka; ramdisk}
    ; framebuffer= bool vm.API.vM_platform false "pvfb"
    ; framebuffer_ip= None (* None PR-1255 *)
    ; vncterm= not (List.mem_assoc "disable_pv_vnc" vm.API.vM_other_config)
    ; vncterm_ip= None (*None PR-1255*)
    ; pci_passthrough= List.mem_assoc "pci" vm.API.vM_other_config
    }
  in
  let make_indirect_boot_record
      {Helpers.bootloader; extra_args; legacy_args; pv_bootloader_args= p; vdis}
      =
    {
      boot=
        Indirect
          {
            bootloader
          ; extra_args
          ; legacy_args
          ; bootloader_args= p
          ; devices=
              List.filter_map (fun x -> disk_of_vdi ~__context ~self:x) vdis
          }
    ; framebuffer= bool vm.API.vM_platform false "pvfb"
    ; framebuffer_ip= None (* None PR-1255 *)
    ; vncterm= not (List.mem_assoc "disable_pv_vnc" vm.API.vM_other_config)
    ; vncterm_ip= None (*None PR-1255*)
    ; pci_passthrough= List.mem_assoc "pci" vm.API.vM_other_config
    }
  in
  match
    Helpers.
      (check_domain_type vm.API.vM_domain_type, boot_method_of_vm ~__context ~vm)
  with
  | `hvm, Helpers.Hvmloader _ ->
      HVM (make_hvmloader_boot_record ())
  | `pv, Helpers.Direct options ->
      PV (make_direct_boot_record options)
  | `pv, Helpers.Indirect options ->
      PV (make_indirect_boot_record options)
  | `pv_in_pvh, Helpers.Direct options ->
      PVinPVH (make_direct_boot_record options)
  | `pv_in_pvh, Helpers.Indirect options ->
      PVinPVH (make_indirect_boot_record options)
  | `pvh, Helpers.Direct options ->
      PVH (make_direct_boot_record options)
  | `pvh, Helpers.Indirect options ->
      PVH (make_indirect_boot_record options)
  | _ ->
      Helpers.internal_error "invalid boot configuration"

let list_net_sriov_vf_pcis ~__context ~vm =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  vm.API.vM_VIFs
  |> List.filter (fun self -> Db.VIF.get_currently_attached ~__context ~self)
  |> List.filter_map (fun vif ->
         match backend_of_vif ~__context ~vif with
         | Network.Sriov {domain; bus; dev; fn} ->
             Some (domain, bus, dev, fn)
         | _ ->
             None
     )

module MD = struct
  (** Convert between xapi DB records and xenopsd records *)

  let of_vbd ~__context ~vm ~vbd =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let hvm =
      match vm.API.vM_domain_type with
      | `hvm ->
          true
      | `pv_in_pvh | `pv | `pvh | `unspecified ->
          false
    in
    let device_number =
      match Device_number.of_string ~hvm vbd.API.vBD_userdevice with
      | Some dev ->
          dev
      | None ->
          raise
            Api_errors.(Server_error (invalid_device, [vbd.API.vBD_userdevice]))
    in
    let open Vbd in
    let ty = vbd.API.vBD_qos_algorithm_type in
    let params = vbd.API.vBD_qos_algorithm_params in
    let qos_class params =
      match List.assoc_opt "class" params with
      | Some "highest" ->
          Highest
      | Some "high" ->
          High
      | Some "normal" ->
          Normal
      | Some "low" ->
          Low
      | Some "lowest" ->
          Lowest
      | Some s -> (
        try Other (int_of_string s)
        with _ ->
          warn "Unknown VBD QoS scheduler class (try 'high' 'low' 'normal')" ;
          Normal
      )
      | None ->
          Normal
    in
    let qos_scheduler params =
      try
        match List.assoc "sched" params with
        | "rt" | "real-time" ->
            RealTime (qos_class params)
        | "idle" ->
            Idle
        | "best-effort" ->
            BestEffort (qos_class params)
        | _ ->
            warn
              "Unknown VBD QoS scheduler (try 'real-time' 'idle' 'best-effort')" ;
            BestEffort (qos_class params)
      with Not_found -> BestEffort (qos_class params)
    in
    let qos = function
      | "ionice" ->
          Some (Ionice (qos_scheduler params))
      | "" ->
          None
      | x ->
          warn "Unknown VBD QoS type: %s (try 'ionice')" x ;
          None
    in
    let other_config_keys ?(default = None) key =
      let oc = vbd.API.vBD_other_config in
      let k = key in
      try
        let v = List.assoc k oc in
        [(k, v)]
      with Not_found -> ( match default with None -> [] | Some x -> [(k, x)]
      )
    in
    let in_range ~min ~max ~fallback values =
      List.map
        (fun (k, v) ->
          ( k
          , let value =
              try int_of_string v
              with _ ->
                debug
                  "%s: warning: value %s is not an integer. Using fallback \
                   value %d"
                  k v fallback ;
                fallback
            in
            string_of_int
              ( if value < min then
                  min
                else if value > max then
                  max
                else
                  value
              )
          )
        )
        values
    in
    let backend_kind_keys = other_config_keys Xapi_globs.vbd_backend_key in
    let poll_duration_keys =
      in_range ~min:0 ~max:max_int ~fallback:0
        (* if user provides invalid integer, use 0 = disable polling *)
        (other_config_keys Xapi_globs.vbd_polling_duration_key
           ~default:
             (Some (string_of_int !Xapi_globs.default_vbd3_polling_duration))
        )
    in
    let poll_idle_threshold_keys =
      in_range ~min:0 ~max:100
        ~fallback:50 (* if user provides invalid float, use 50 = default 50% *)
        (other_config_keys Xapi_globs.vbd_polling_idle_threshold_key
           ~default:
             (Some
                (string_of_int !Xapi_globs.default_vbd3_polling_idle_threshold)
             )
        )
    in
    let backend_of_vbd vbd =
      let vbd_oc = vbd.API.vBD_other_config in
      if List.mem_assoc Xapi_globs.vbd_backend_local_key vbd_oc then (
        let path = List.assoc Xapi_globs.vbd_backend_local_key vbd_oc in
        warn "Using local override for VBD backend: %s -> %s" vbd.API.vBD_uuid
          path ;
        Some (Local path)
      ) else
        disk_of_vdi ~__context ~self:vbd.API.vBD_VDI
    in
    {
      id= (vm.API.vM_uuid, Device_number.to_linux_device device_number)
    ; position= Some device_number
    ; mode= (if vbd.API.vBD_mode = `RO then ReadOnly else ReadWrite)
    ; backend= backend_of_vbd vbd
    ; ty=
        ( match vbd.API.vBD_type with
        | `Disk ->
            Disk
        | `CD ->
            CDROM
        | `Floppy ->
            Floppy
        )
    ; unpluggable= vbd.API.vBD_unpluggable
    ; extra_backend_keys=
        backend_kind_keys @ poll_duration_keys @ poll_idle_threshold_keys
    ; extra_private_keys= []
    ; qos= qos ty
    ; persistent=
        ( try Db.VDI.get_on_boot ~__context ~self:vbd.API.vBD_VDI = `persist
          with _ -> true
        )
    }

  let of_pvs_proxy ~__context vif proxy =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let site = Db.PVS_proxy.get_site ~__context ~self:proxy in
    let site_uuid = Db.PVS_site.get_uuid ~__context ~self:site in
    let servers = Db.PVS_site.get_servers ~__context ~self:site in
    let servers =
      List.map
        (fun server ->
          let rc = Db.PVS_server.get_record ~__context ~self:server in
          {
            Vif.PVS_proxy.addresses= rc.API.pVS_server_addresses
          ; first_port= Int64.to_int rc.API.pVS_server_first_port
          ; last_port= Int64.to_int rc.API.pVS_server_last_port
          }
        )
        servers
    in
    let interface = Pvs_proxy_control.proxy_port_name vif in
    (site_uuid, servers, interface)

  let of_vif ~__context ~vm ~vif:(vif_ref, vif) =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let net = Db.Network.get_record ~__context ~self:vif.API.vIF_network in
    let net_mtu = Int64.to_int net.API.network_MTU in
    let mtu =
      try
        if List.mem_assoc "mtu" vif.API.vIF_other_config then
          List.assoc "mtu" vif.API.vIF_other_config |> int_of_string
        else
          net_mtu
      with _ ->
        error "Failed to parse VIF.other_config:mtu; defaulting to network.mtu" ;
        net_mtu
    in
    let qos_type = vif.API.vIF_qos_algorithm_type in
    let qos_params = vif.API.vIF_qos_algorithm_params in
    let log_qos_failure reason =
      warn "vif QoS failed: %s (vm=%s,vif=%s)" reason vm.API.vM_uuid
        vif.API.vIF_uuid
    in
    let rate =
      match qos_type with
      | "ratelimit" -> (
          let timeslice =
            try Int64.of_string (List.assoc "timeslice_us" qos_params)
            with _ -> 0L
          in
          try
            let rate = Int64.of_string (List.assoc "kbps" qos_params) in
            Some (rate, timeslice)
          with
          | Failure _ (* int_of_string *) ->
              log_qos_failure "parameter \"kbps\" not an integer" ;
              None
          | Not_found ->
              log_qos_failure "necessary parameter \"kbps\" not found" ;
              None
          | e ->
              log_qos_failure
                (Printf.sprintf "unexpected error: %s" (Printexc.to_string e)) ;
              None
        )
      | "" ->
          None
      | _ ->
          log_qos_failure (Printf.sprintf "unknown type: %s" qos_type) ;
          None
    in
    let locking_mode =
      match
        (vif.API.vIF_locking_mode, net.API.network_default_locking_mode)
      with
      | `network_default, `disabled ->
          Vif.Disabled
      | `network_default, `unlocked ->
          Vif.Unlocked
      | `locked, _ ->
          Vif.Locked
            {Vif.ipv4= vif.API.vIF_ipv4_allowed; ipv6= vif.API.vIF_ipv6_allowed}
      | `unlocked, _ ->
          Vif.Unlocked
      | `disabled, _ ->
          Vif.Disabled
    in
    let host = Helpers.get_localhost ~__context in
    let pifs =
      Xapi_network_attach_helpers.get_local_pifs ~__context
        ~network:vif.API.vIF_network ~host
    in
    let carrier =
      if !Xapi_globs.pass_through_pif_carrier then
        (* We need to reflect the carrier of the local PIF on the network (if any) *)
        match pifs with
        | [] ->
            true (* Internal network; consider as "always up" *)
        | pif :: _ -> (
          try
            let metrics = Db.PIF.get_metrics ~__context ~self:pif in
            Db.PIF_metrics.get_carrier ~__context ~self:metrics
          with _ -> true
        )
      else
        (* If we don't need to reflect anything, the carrier is set to "true" *)
        true
    in
    let ipv4_configuration =
      match vif.API.vIF_ipv4_configuration_mode with
      | `None ->
          Vif.Unspecified4
      | `Static ->
          let gateway =
            if vif.API.vIF_ipv4_gateway = "" then
              None
            else
              Some vif.API.vIF_ipv4_gateway
          in
          Vif.Static4 (vif.API.vIF_ipv4_addresses, gateway)
    in
    let ipv6_configuration =
      match vif.API.vIF_ipv6_configuration_mode with
      | `None ->
          Vif.Unspecified6
      | `Static ->
          let gateway =
            if vif.API.vIF_ipv6_gateway = "" then
              None
            else
              Some vif.API.vIF_ipv6_gateway
          in
          Vif.Static6 (vif.API.vIF_ipv6_addresses, gateway)
    in
    let extra_private_keys =
      [("vif-uuid", vif.API.vIF_uuid); ("network-uuid", net.API.network_uuid)]
    in
    let pvs_proxy =
      Option.map
        (of_pvs_proxy ~__context vif)
        (Pvs_proxy_control.find_proxy_for_vif ~__context ~vif:vif_ref)
    in
    let vlan =
      match pifs with
      | [] ->
          None
      | pif :: _ ->
          let vlan = Db.PIF.get_VLAN ~__context ~self:pif in
          if vlan < 0L then None else Some vlan
    in
    {
      Vif.id= (vm.API.vM_uuid, vif.API.vIF_device)
    ; position= int_of_string vif.API.vIF_device
    ; mac= vif.API.vIF_MAC
    ; carrier
    ; mtu
    ; rate
    ; backend= backend_of_vif ~__context ~vif:vif_ref
    ; other_config= vif.API.vIF_other_config
    ; locking_mode
    ; extra_private_keys
    ; ipv4_configuration
    ; ipv6_configuration
    ; pvs_proxy
    ; vlan
    }

  let pcis_of_vm ~__context (vmref, vm) =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let vgpu_pcidevs = Vgpuops.list_pcis_for_passthrough ~__context ~vm:vmref in
    let devs =
      List.concat_map (fun (_, dev) -> dev) (Pciops.sort_pcidevs vgpu_pcidevs)
    in
    (* The 'unmanaged' PCI devices are in the other_config key: *)
    let other_pcidevs =
      Pciops.other_pcidevs_of_vm ~__context vm.API.vM_other_config
    in
    let unmanaged =
      List.concat_map (fun (_, dev) -> dev) (Pciops.sort_pcidevs other_pcidevs)
    in
    let net_sriov_pcidevs = list_net_sriov_vf_pcis ~__context ~vm in
    let devs = devs @ net_sriov_pcidevs @ unmanaged in
    let open Pci in
    List.mapi
      (fun idx (domain, bus, dev, fn) ->
        {
          id=
            ( vm.API.vM_uuid
            , Printf.sprintf "%04x:%02x:%02x.%01x" domain bus dev fn
            )
        ; position= idx
        ; address= {domain; bus; dev; fn}
        ; msitranslate= None
        ; power_mgmt= None
        }
      )
      devs

  let get_target_pci_address ~__context vgpu =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let pgpu =
      if
        Db.is_valid_ref __context
          vgpu.Db_actions.vGPU_scheduled_to_be_resident_on
      then
        vgpu.Db_actions.vGPU_scheduled_to_be_resident_on
      else
        vgpu.Db_actions.vGPU_resident_on
    in
    let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
    let pci_address = Db.PCI.get_pci_id ~__context ~self:pci in
    Xenops_interface.Pci.address_of_string pci_address

  let get_virtual_pci_address ~__context vgpu =
    let open Pci in
    let device = vgpu.Db_actions.vGPU_device in
    {
      domain= 0000
    ; bus= 0
    ; dev= int_of_string device + Xapi_globs.nvidia_vgpu_first_slot_in_guest
    ; fn= 0
    }

  (** Return the virtual function (VF) for a VGPU operated in SR-IOV
   * mode, or None otherwise. In particular, return None when a VGPU
   * is passed trough completely.
   *)
  let sriov_vf ~__context vgpu =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let is_sriov () =
      let ty = vgpu.Db_actions.vGPU_type in
      match Db.VGPU_type.get_implementation ~__context ~self:ty with
      | `nvidia_sriov ->
          true
      | _ ->
          false
    in
    match vgpu.Db_actions.vGPU_PCI with
    | pci when pci = Ref.null ->
        None
    | pci when not (Db.is_valid_ref __context pci) ->
        None
    | _ when not @@ is_sriov () ->
        None
    | pci ->
        Db.PCI.get_pci_id ~__context ~self:pci |> fun str ->
        Xenops_interface.Pci.address_of_string str |> fun addr -> Some addr

  let of_nvidia_vgpu ~__context vm vgpu =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let open Vgpu in
    (* Get the PCI address. *)
    let physical_pci_address = get_target_pci_address ~__context vgpu in
    let virtual_pci_address = get_virtual_pci_address ~__context vgpu in
    let vgpu_type = vgpu.Db_actions.vGPU_type in
    let type_id, config_file, vclass =
      Db.VGPU_type.get_internal_config ~__context ~self:vgpu_type |> fun kv ->
      ( List.assoc_opt Xapi_globs.vgpu_type_id kv
      , List.assoc_opt Xapi_globs.nvidia_compat_config_file_key kv
      , List.assoc_opt Xapi_globs.vgpu_type_vclass kv
      )
    in
    let uuid = vgpu.Db_actions.vGPU_uuid in
    let extra_args = vgpu.Db_actions.vGPU_extra_args in
    let implementation =
      Nvidia
        {
          physical_pci_address= None
        ; (* unused *)
          config_file
        ; virtual_pci_address
        ; type_id
        ; uuid= Some uuid
        ; extra_args
        ; vclass (* from vgpuType class attribute in vgpuConfig.xml *)
        }
    in
    {
      id= (vm.API.vM_uuid, vgpu.Db_actions.vGPU_device)
    ; position= int_of_string vgpu.Db_actions.vGPU_device
    ; physical_pci_address
    ; virtual_pci_address= sriov_vf ~__context vgpu
    ; implementation
    }

  let of_gvt_g_vgpu ~__context vm vgpu =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let open Vgpu in
    (* Get the PCI address. *)
    let physical_pci_address = get_target_pci_address ~__context vgpu in
    (* Get the vGPU config. *)
    let vgpu_type = vgpu.Db_actions.vGPU_type in
    let internal_config =
      Db.VGPU_type.get_internal_config ~__context ~self:vgpu_type
    in
    try
      let implementation =
        GVT_g
          {
            physical_pci_address= None
          ; (* unused *)
            low_gm_sz=
              List.assoc Xapi_globs.vgt_low_gm_sz internal_config
              |> Int64.of_string
          ; high_gm_sz=
              List.assoc Xapi_globs.vgt_high_gm_sz internal_config
              |> Int64.of_string
          ; fence_sz=
              List.assoc Xapi_globs.vgt_fence_sz internal_config
              |> Int64.of_string
          ; monitor_config_file= None (* unused *)
          }
      in
      {
        id= (vm.API.vM_uuid, vgpu.Db_actions.vGPU_device)
      ; position= int_of_string vgpu.Db_actions.vGPU_device
      ; physical_pci_address
      ; implementation
      ; virtual_pci_address= sriov_vf ~__context vgpu
      }
    with
    | Not_found ->
        failwith "Intel GVT-g settings not specified"
    | Failure _ (* int_of_string *) ->
        failwith "Intel GVT-g settings invalid"

  let of_mxgpu_vgpu ~__context vm vgpu =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let open Vgpu in
    (* Get the PCI address. *)
    let physical_pci_address = get_target_pci_address ~__context vgpu in
    let vgpu_type = vgpu.Db_actions.vGPU_type in
    let internal_config =
      Db.VGPU_type.get_internal_config ~__context ~self:vgpu_type
    in
    let framebufferbytes =
      Db.VGPU_type.get_framebuffer_size ~__context ~self:vgpu_type
    in
    try
      let implementation =
        MxGPU
          {
            physical_function= None
          ; (* unused *)
            vgpus_per_pgpu=
              List.assoc Xapi_globs.mxgpu_vgpus_per_pgpu internal_config
              |> Int64.of_string
          ; framebufferbytes
          }
      in
      {
        id= (vm.API.vM_uuid, vgpu.Db_actions.vGPU_device)
      ; position= int_of_string vgpu.Db_actions.vGPU_device
      ; physical_pci_address
      ; implementation
      ; virtual_pci_address= sriov_vf ~__context vgpu
      }
    with
    | Not_found ->
        failwith "AMD MxGPU settings not specified"
    | Failure _ (* int_of_string *) ->
        failwith "AMD MxGPU settings invalid"

  let vgpus_of_vm ~__context (_, vm) =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    List.fold_left
      (fun acc vgpu ->
        let vgpu_record = Db.VGPU.get_record_internal ~__context ~self:vgpu in
        let implementation =
          Db.VGPU_type.get_implementation ~__context
            ~self:vgpu_record.Db_actions.vGPU_type
        in
        match implementation with
        (* Passthrough VGPUs are dealt with in pcis_of_vm. *)
        | `passthrough ->
            acc
        | `nvidia | `nvidia_sriov ->
            of_nvidia_vgpu ~__context vm vgpu_record :: acc
        | `gvt_g ->
            of_gvt_g_vgpu ~__context vm vgpu_record :: acc
        | `mxgpu ->
            of_mxgpu_vgpu ~__context vm vgpu_record :: acc
      )
      [] vm.API.vM_VGPUs

  let of_vusb ~__context ~vm ~pusb =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let open Vusb in
    try
      let path = pusb.API.pUSB_path in
      let pathList = String.split_on_char '-' path in
      let hostbus = List.nth pathList 0 in
      let hostport = List.nth pathList 1 in
      (* Here version can be 1.10/2.00/3.00. *)
      let version = pusb.API.pUSB_version in
      let speed = pusb.API.pUSB_speed in
      {
        id= (vm.API.vM_uuid, "vusb" ^ path)
      ; hostbus
      ; hostport
      ; version
      ; path
      ; speed
      }
    with e ->
      error "Caught %s: while getting PUSB path %s" (Printexc.to_string e)
        pusb.API.pUSB_path ;
      raise e

  let vusbs_of_vm ~__context (_, vm) =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    vm.API.vM_VUSBs
    |> List.map (fun self -> Db.VUSB.get_record ~__context ~self)
    |> List.filter (fun self -> self.API.vUSB_currently_attached)
    |> List.map (fun self -> self.API.vUSB_USB_group)
    |> List.map (fun usb_group -> Helpers.get_first_pusb ~__context usb_group)
    |> List.map (fun self -> Db.PUSB.get_record ~__context ~self)
    |> List.map (fun pusb -> of_vusb ~__context ~vm ~pusb)

  let of_vm ~__context (vmref, vm) vbds pci_passthrough vgpu =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let on_action_behaviour = function
      | `preserve ->
          [Vm.Pause]
      | `coredump_and_restart ->
          [Vm.Coredump; Vm.Start]
      | `coredump_and_destroy ->
          [Vm.Coredump; Vm.Shutdown]
      | `restart | `rename_restart ->
          [Vm.Start]
      | `destroy ->
          [Vm.Shutdown]
      | `soft_reboot ->
          [Vm.Softreboot]
    in

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
        with _ -> []
      in
      let localhost = Helpers.get_localhost ~__context in
      let host_guest_VCPUs_params =
        Db.Host.get_guest_VCPUs_params ~__context ~self:localhost
      in
      let host_cpu_mask =
        try
          List.map int_of_string
            (String.split ',' (List.assoc "mask" host_guest_VCPUs_params))
        with _ -> []
      in
      let affinity =
        match (affinity, host_cpu_mask) with
        | [], [] ->
            []
        | [], h ->
            [h]
        | v, [] ->
            v
        | affinity, mask ->
            List.map
              (fun vcpu_affinity ->
                List.filter (fun x -> List.mem x mask) vcpu_affinity
              )
              affinity
      in
      let priority =
        let weight =
          let default = 256 in
          try
            let weight = List.assoc "weight" vm.API.vM_VCPUs_params in
            int_of_string weight
          with
          | Not_found ->
              default
          | e ->
              error "%s" (Printexc.to_string e) ;
              debug
                "Could not parse weight value. Setting it to default value %d."
                default ;
              default
        in
        let cap =
          let default = 0 in
          try
            let cap = List.assoc "cap" vm.API.vM_VCPUs_params in
            int_of_string cap
          with
          | Not_found ->
              default
          | e ->
              error "%s" (Printexc.to_string e) ;
              debug "Could not parse cap value. Setting it to default value %d."
                default ;
              default
        in
        Some (weight, cap)
      in
      {priority; affinity}
    in
    let firmware = firmware_of_vm vm in
    let platformdata =
      Vm_platform.sanity_check ~platformdata:vm.API.vM_platform ~firmware
        ~vcpu_max:vm.API.vM_VCPUs_max
        ~vcpu_at_startup:vm.API.vM_VCPUs_at_startup
        ~domain_type:(Helpers.check_domain_type vm.API.vM_domain_type)
        ~filter_out_unknowns:
          (not (Pool_features.is_enabled ~__context Features.No_platform_filter))
    in
    (* Replace the timeoffset in the platform data too, to avoid confusion *)
    let timeoffset = rtc_timeoffset_of_vm ~__context (vmref, vm) vbds in
    let platformdata =
      (Vm_platform.timeoffset, timeoffset)
      :: List.filter
           (fun (key, _) -> key <> Vm_platform.timeoffset)
           platformdata
    in
    let generation_id =
      match vm.API.vM_generation_id with
      | "0:0" ->
          Some (Xapi_vm_helpers.vm_fresh_genid ~__context ~self:vmref)
      | _ ->
          Some vm.API.vM_generation_id
    in
    (* Add the CPUID feature set for the VM's next boot to the platform data. *)
    let platformdata =
      if not (List.mem_assoc Vm_platform.featureset platformdata) then
        let featureset =
          match
            List.assoc_opt Xapi_globs.cpu_info_features_key
              vm.API.vM_last_boot_CPU_flags
          with
          | _ when vm.API.vM_power_state <> `Suspended ->
              Cpuid_helpers.next_boot_cpu_features ~__context ~vm:vmref
          | Some fs ->
              (* The VM's current featureset is now part of xenopsd's
                 persistent metadata, and taken from there on resume
                 and migrate-receive. However, VMs suspended before this
                 change don't have the featureset there yet, and xenopsd
                 falls back to the platformdata. We can't detect this case
                 here, so we'll therefore fall back to the original source
                 (VM.last_boot_CPU_flags) regardless. *)
              fs
          | None ->
              failwith "VM's CPU featureset not initialised"
        in
        (Vm_platform.featureset, featureset) :: platformdata
      else
        platformdata
    in
    (* BIOS guests don't seem to detect the attached VTPM, block them *)
    ( match (firmware, vm.API.vM_VTPMs) with
    | Xenops_types.Vm.Bios, _ :: _ ->
        let message = "Booting BIOS VM with VTPMs attached" in
        Helpers.maybe_raise_vtpm_unimplemented __FUNCTION__ message
    | _ ->
        ()
    ) ;
    (* Add TPM version 2 iff there's a tpm attached to the VM, this allows
       hvmloader to load the TPM 2.0 ACPI table while maintaing the current
       ACPI table for other guests *)
    let platformdata =
      if vm.API.vM_VTPMs <> [] || bool vm.API.vM_platform false "vtpm" then
        (Vm_platform.tpm_version, "2") :: platformdata
      else
        platformdata
    in
    let pci_msitranslate = true in
    (* default setting *)
    (* CA-55754: allow VM.other_config:msitranslate to override the bus-wide setting *)
    let pci_msitranslate =
      if List.mem_assoc "msitranslate" vm.API.vM_other_config then
        List.assoc "msitranslate" vm.API.vM_other_config = "1"
      else
        pci_msitranslate
    in
    (* CA-55754: temporarily disable msitranslate when GPU is passed through. *)
    let pci_msitranslate =
      if vm.API.vM_VGPUs <> [] then false else pci_msitranslate
    in
    if
      List.assoc_opt "suppress-spurious-page-faults" vm.API.vM_other_config
      = Some "true"
    then
      warn
        "The suppress-spurious-page-faults option used by VM %s is no longer \
         implemented"
        vm.API.vM_uuid ;
    if List.mem_assoc "machine-address-size" vm.API.vM_other_config then
      warn
        "The machine-address-size option used by VM %s is no longer implemented"
        vm.API.vM_uuid ;
    {
      id= vm.API.vM_uuid
    ; name= vm.API.vM_name_label
    ; ssidref= 0l
    ; xsdata= vm.API.vM_xenstore_data
    ; platformdata
    ; bios_strings= vm.API.vM_bios_strings
    ; ty= builder_of_vm ~__context (vmref, vm) timeoffset pci_passthrough vgpu
    ; suppress_spurious_page_faults= false
    ; (* Obsolete: no longer implemented *)
      machine_address_size= None
    ; (* Obsolete: no longer implemented *)
      memory_static_max= vm.API.vM_memory_static_max
    ; memory_dynamic_max= vm.API.vM_memory_dynamic_max
    ; memory_dynamic_min= vm.API.vM_memory_dynamic_min
    ; vcpu_max= Int64.to_int vm.API.vM_VCPUs_max
    ; vcpus= Int64.to_int vm.API.vM_VCPUs_at_startup
    ; scheduler_params
    ; on_crash= on_action_behaviour vm.API.vM_actions_after_crash
    ; on_shutdown= on_action_behaviour vm.API.vM_actions_after_shutdown
    ; on_reboot= on_action_behaviour vm.API.vM_actions_after_reboot
    ; on_softreboot= on_action_behaviour vm.API.vM_actions_after_softreboot
    ; pci_msitranslate
    ; pci_power_mgmt= false
    ; has_vendor_device= vm.API.vM_has_vendor_device
    ; generation_id
    }
end

open Xenops_interface

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
        if List.mem_assoc Xapi.auto_update_enabled config then
          Some
            (* bool_of_string should be safe as the setter in xapi_pool.ml only
               					 * allows "true" or "false" to be put into the database. *)
            (bool_of_string (List.assoc Xapi.auto_update_enabled config))
        else
          None
      with
      | Some true ->
          [(Xenopsd.auto_update_enabled, Xenopsd.enabled)]
      | Some false ->
          [(Xenopsd.auto_update_enabled, Xenopsd.disabled)]
      | None ->
          []
    in
    let auto_update_url =
      if List.mem_assoc Xapi.auto_update_url config then
        [(Xenopsd.auto_update_url, List.assoc Xapi.auto_update_url config)]
      else
        []
    in
    auto_update_enabled @ auto_update_url

  let of_config ~__context config =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let open Features in
    let vss =
      let name = Features.name_of_feature VSS in
      let licensed = Pool_features.is_enabled ~__context VSS in
      let parameters = [] in
      Host.{name; licensed; parameters}
    in
    let guest_agent_auto_update =
      let name = Features.name_of_feature Guest_agent_auto_update in
      let licensed =
        Pool_features.is_enabled ~__context Guest_agent_auto_update
      in
      let parameters = auto_update_parameters_of_config config in
      Host.{name; licensed; parameters}
    in
    [vss; guest_agent_auto_update]
end

let apply_guest_agent_config ~__context config =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let features = Guest_agent_features.of_config ~__context config in
  let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
  Client.HOST.update_guest_agent_features dbg features

(* Create an instance of Metadata.t, suitable for uploading to the xenops service *)
let create_metadata ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VM.get_record ~__context ~self in
  let vbds =
    List.filter
      (fun vbd -> vbd.API.vBD_currently_attached)
      (List.map (fun self -> Db.VBD.get_record ~__context ~self) vm.API.vM_VBDs)
  in
  let vbds' = List.map (fun vbd -> MD.of_vbd ~__context ~vm ~vbd) vbds in
  let vifs =
    List.filter
      (fun (_, vif) -> vif.API.vIF_currently_attached)
      (List.map
         (fun self -> (self, Db.VIF.get_record ~__context ~self))
         vm.API.vM_VIFs
      )
  in
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
  let open Metadata in
  {
    vm= MD.of_vm ~__context (self, vm) vbds (pcis <> []) (vgpus <> [])
  ; vbds= vbds'
  ; vifs= vifs'
  ; pcis
  ; vgpus
  ; vusbs
  ; domains
  }

let id_of_vm ~__context ~self = Db.VM.get_uuid ~__context ~self

let vm_of_id ~__context uuid = Db.VM.get_by_uuid ~__context ~uuid

let vm_exists_in_xenopsd queue_name dbg id =
  Debug_info.with_dbg ~name:__FUNCTION__ ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let module Client = (val make_client queue_name : XENOPS) in
  Client.VM.exists dbg id

let string_of_exn = function
  | Api_errors.Server_error (code, params) ->
      Printf.sprintf "%s [ %s ]" code (String.concat "; " params)
  | e ->
      Printexc.to_string e

(* Serialise updates to the metadata caches *)
let metadata_m = Mutex.create ()

module Xapi_cache = struct
  (** Keep a cache of the "xenops-translation" of XenAPI VM configuration,
      		updated whenever we receive an event from xapi. *)

  let cache = Hashtbl.create 10 (* indexed by Vm.id *)

  let mutex = Mutex.create ()

  let with_lock f = with_lock mutex f

  let register id initial_value =
    debug "xapi_cache: creating cache for %s" id ;
    with_lock (fun () ->
        match Hashtbl.find_opt cache id with
        | Some (Some _) ->
            (* don't change if we already have a valid cached entry *)
            ()
        | None | Some None ->
            (* we do not have a cache entry for this id,
             * or we have only an empty cache entry *)
            Hashtbl.replace cache id initial_value
    )

  let unregister id =
    debug "xapi_cache: deleting cache for %s" id ;
    with_lock (fun () -> Hashtbl.remove cache id)

  let update_if_changed id newvalue =
    let updated =
      with_lock (fun () ->
          match Hashtbl.find_opt cache id with
          | Some (Some old) when old = newvalue ->
              (* the value did not change: tell the caller it has no action to take *)
              false
          | _ ->
              Hashtbl.replace cache id (Some newvalue) ;
              (* We either did not have a value before, or we had a different one:
               * tell the caller that it needs to perform an update *)
              true
      )
    in
    debug "xapi_cache:%s updating cache for %s"
      (if updated then "" else " not")
      id ;
    updated

  let list () =
    with_lock (fun () -> Hashtbl.fold (fun id _ acc -> id :: acc) cache [])
end

module Xenops_cache = struct
  (** Remember the last events received from xenopsd so we can compute
      		field-level differences. This allows us to minimise the number of
      		database writes we issue upwards. *)

  type t = {
      vm: Vm.state option
    ; vbds: (Vbd.id * Vbd.state) list
    ; vifs: (Vif.id * Vif.state) list
    ; pcis: (Pci.id * Pci.state) list
    ; vgpus: (Vgpu.id * Vgpu.state) list
    ; vusbs: (Vusb.id * Vusb.state) list
  }

  let empty = {vm= None; vbds= []; vifs= []; pcis= []; vgpus= []; vusbs= []}

  let cache = Hashtbl.create 10 (* indexed by Vm.id *)

  let mutex = Mutex.create ()

  let with_lock f = with_lock mutex f

  let register id =
    debug "xenops_cache: creating empty cache for %s" id ;
    with_lock (fun () -> Hashtbl.replace cache id empty)

  let unregister id =
    debug "xenops_cache: deleting cache for %s" id ;
    with_lock (fun () -> Hashtbl.remove cache id)

  let find id : t option = with_lock (fun () -> Hashtbl.find_opt cache id)

  let find_vm id : Vm.state option =
    match find id with Some {vm= Some vm; _} -> Some vm | _ -> None

  let find_vbd id : Vbd.state option =
    match find (fst id) with
    | Some {vbds; _} ->
        List.assoc_opt id vbds
    | _ ->
        None

  let find_vif id : Vif.state option =
    match find (fst id) with
    | Some {vifs; _} ->
        List.assoc_opt id vifs
    | _ ->
        None

  let find_pci id : Pci.state option =
    match find (fst id) with
    | Some {pcis; _} ->
        List.assoc_opt id pcis
    | _ ->
        None

  let find_vgpu id : Vgpu.state option =
    match find (fst id) with
    | Some {vgpus; _} ->
        List.assoc_opt id vgpus
    | _ ->
        None

  let find_vusb id : Vusb.state option =
    match find (fst id) with
    | Some {vusbs; _} ->
        List.assoc_opt id vusbs
    | _ ->
        None

  let update id t =
    with_lock (fun () ->
        if Hashtbl.mem cache id then
          Hashtbl.replace cache id t
        else
          debug "xenops_cache: Not updating cache for unregistered VM %s" id
    )

  let update_vbd id info =
    let existing = Option.value ~default:empty (find (fst id)) in
    let vbds' = List.filter (fun (vbd_id, _) -> vbd_id <> id) existing.vbds in
    update (fst id)
      {
        existing with
        vbds=
          Option.fold ~none:vbds' ~some:(fun info -> (id, info) :: vbds') info
      }

  let update_vif id info =
    let existing = Option.value ~default:empty (find (fst id)) in
    let vifs' = List.filter (fun (vif_id, _) -> vif_id <> id) existing.vifs in
    update (fst id)
      {
        existing with
        vifs=
          Option.fold ~none:vifs' ~some:(fun info -> (id, info) :: vifs') info
      }

  let update_pci id info =
    let existing = Option.value ~default:empty (find (fst id)) in
    let pcis' = List.filter (fun (pci_id, _) -> pci_id <> id) existing.pcis in
    update (fst id)
      {
        existing with
        pcis=
          Option.fold ~none:pcis' ~some:(fun info -> (id, info) :: pcis') info
      }

  let update_vgpu id info =
    let existing = Option.value ~default:empty (find (fst id)) in
    let vgpus' =
      List.filter (fun (vgpu_id, _) -> vgpu_id <> id) existing.vgpus
    in
    update (fst id)
      {
        existing with
        vgpus=
          Option.fold ~none:vgpus' ~some:(fun info -> (id, info) :: vgpus') info
      }

  let update_vusb id info =
    let existing = Option.value ~default:empty (find (fst id)) in
    let vusbs' =
      List.filter (fun (vusb_id, _) -> vusb_id <> id) existing.vusbs
    in
    update (fst id)
      {
        existing with
        vusbs=
          Option.fold ~none:vusbs' ~some:(fun info -> (id, info) :: vusbs') info
      }

  let update_vm id info =
    let existing = Option.value ~default:empty (find id) in
    update id {existing with vm= info}

  let list () =
    with_lock (fun () -> Hashtbl.fold (fun id _ acc -> id :: acc) cache [])
end

module Xenopsd_metadata = struct
  (** Manage the lifetime of VM metadata pushed to xenopsd *)

  (* If the VM has Xapi_globs.persist_xenopsd_md -> filename in its other_config,
     we persist the xenopsd metadata to a well-known location in the filesystem *)
  let maybe_persist_md ~__context ~self md =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let oc = Db.VM.get_other_config ~__context ~self in
    if List.mem_assoc Xapi_globs.persist_xenopsd_md oc then
      let file_path =
        Filename.concat Xapi_globs.persist_xenopsd_md_root
          (List.assoc Xapi_globs.persist_xenopsd_md oc)
        |> Xapi_stdext_unix.Unixext.resolve_dot_and_dotdot
      in
      if
        not
          (String.starts_with ~prefix:Xapi_globs.persist_xenopsd_md_root
             file_path
          )
      then
        warn "Not persisting xenopsd metadata to bad location: '%s'" file_path
      else (
        Unixext.mkdir_safe Xapi_globs.persist_xenopsd_md_root 0o755 ;
        Unixext.write_string_to_file file_path md
      )

  let push ~__context ~self =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    with_lock metadata_m (fun () ->
        let md = create_metadata ~__context ~self in
        let txt = md |> rpc_of Metadata.t |> Jsonrpc.to_string in
        info "xenops: VM.import_metadata %s" txt ;
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self) : XENOPS)
        in
        let id = Client.VM.import_metadata dbg txt in
        maybe_persist_md ~__context ~self txt ;
        Xapi_cache.register id (Some txt) ;
        Xenops_cache.register id ;
        id
    )

  let delete_nolock ~__context id =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let dbg = Context.string_of_task_and_tracing __context in
    info "xenops: VM.remove %s" id ;
    try
      let module Client =
        ( val make_client (queue_of_vm ~__context ~self:(vm_of_id ~__context id))
            : XENOPS
          )
      in
      Client.VM.remove dbg id ;
      (* Once the VM has been successfully removed from xenopsd, remove the caches *)
      Xenops_cache.unregister id ;
      Xapi_cache.unregister id
    with
    | Xenopsd_error (Bad_power_state (_, _)) ->
        (* This can fail during a localhost live migrate; but this is safe to ignore *)
        debug
          "We have not removed metadata from xenopsd because VM %s is still \
           running"
          id
    | Xenopsd_error (Does_not_exist _) ->
        debug "Metadata for VM %s was already removed" id

  (* Unregisters a VM with xenopsd, and cleans up metadata and caches *)
  let pull ~__context id =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    with_lock metadata_m (fun () ->
        info "xenops: VM.export_metadata %s" id ;
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          ( val make_client
                  (queue_of_vm ~__context ~self:(vm_of_id ~__context id))
              : XENOPS
            )
        in
        let md =
          match
            Client.VM.export_metadata dbg id
            |> Jsonrpc.of_string
            |> Rpcmarshal.unmarshal Metadata.t.Rpc.Types.ty
          with
          | Ok x ->
              x
          | Error (`Msg m) ->
              raise
                (Xenopsd_error
                   (Internal_error
                      (Printf.sprintf "Failed to unmarshal metadata: %s" m)
                   )
                )
        in
        delete_nolock ~__context id ;
        md
    )

  let delete ~__context id =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    with_lock metadata_m (fun () -> delete_nolock ~__context id)

  let update ~__context ~self =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let id = id_of_vm ~__context ~self in
    let queue_name = queue_of_vm ~__context ~self in
    with_lock metadata_m (fun () ->
        let dbg = Context.string_of_task_and_tracing __context in
        if vm_exists_in_xenopsd queue_name dbg id then
          let txt =
            create_metadata ~__context ~self
            |> rpc_of Metadata.t
            |> Jsonrpc.to_string
          in
          if Xapi_cache.update_if_changed id txt then (
            debug "VM %s metadata has changed: updating xenopsd" id ;
            info "xenops: VM.import_metadata %s" txt ;
            maybe_persist_md ~__context ~self txt ;
            let module Client = (val make_client queue_name : XENOPS) in
            Client.VM.import_metadata_async dbg txt
            |> Client.TASK.destroy_on_finish dbg
          )
    )
end

let add_caches id =
  with_lock metadata_m (fun () ->
      Xapi_cache.register id None ;
      Xenops_cache.register id
  )

let to_xenops_console_protocol =
  let open Vm in
  function `rfb -> Rfb | `vt100 -> Vt100 | `rdp -> Rfb

(* RDP was never used in the XenAPI so this never happens *)

let to_xenapi_console_protocol =
  let open Vm in
  function Rfb -> `rfb | Vt100 -> `vt100

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
  ref (fun () -> debug "No xapi event thread to wake up")

module Events_from_xenopsd = struct
  type t = {mutable finished: bool; m: Mutex.t; c: Condition.t}

  let make () = {finished= false; m= Mutex.create (); c= Condition.create ()}

  let active = Hashtbl.create 10

  let active_m = Mutex.create ()

  let register =
    let counter = ref 0 in
    fun t ->
      with_lock active_m (fun () ->
          let id = !counter in
          incr counter ;
          Hashtbl.replace active id t ;
          id
      )

  let wait queue_name dbg vm_id () =
    let module Client = (val make_client queue_name : XENOPS) in
    let t = make () in
    let id = register t in
    Debug_info.with_dbg
      ~attributes:
        [
          ("messaging.operation.name", "subscribe")
        ; ("messaging.system", "event")
        ; ("messaging.destination.subscription.name", vm_id)
        ; ("messaging.message.id", string_of_int id)
        ]
      ~name:("subscribe" ^ " " ^ queue_name)
      ~dbg
    @@ fun di ->
    let dbg = Debug_info.to_string di in
    debug "Client.UPDATES.inject_barrier %d" id ;
    Client.UPDATES.inject_barrier dbg vm_id id ;
    with_lock t.m (fun () ->
        while not t.finished do
          Condition.wait t.c t.m
        done
    )

  let wakeup queue_name dbg id =
    Debug_info.with_dbg
      ~attributes:
        [
          ("messaging.operation.name", "settle")
        ; ("messaging.system", "event")
        ; ("messaging.message.id", string_of_int id)
        ]
      ~name:("settle" ^ " " ^ queue_name)
      ~dbg
    @@ fun di ->
    let dbg = Debug_info.to_string di in
    let module Client = (val make_client queue_name : XENOPS) in
    Client.UPDATES.remove_barrier dbg id ;
    let t =
      with_lock active_m @@ fun () ->
      match Hashtbl.find_opt active id with
      | Some t ->
          Hashtbl.remove active id ; Some t
      | None ->
          warn "Events_from_xenopsd.wakeup: unknown id %d" id ;
          None
    in
    Option.iter
      (fun t ->
        with_lock t.m @@ fun () ->
        t.finished <- true ;
        Condition.signal t.c
      )
      t

  let events_suppressed_on = Hashtbl.create 10

  let events_suppressed_on_m = Mutex.create ()

  let events_suppressed_on_c = Condition.create ()

  let are_suppressed vm = Hashtbl.mem events_suppressed_on vm

  let with_suppressed queue_name dbg vm_id f =
    debug "suppressing xenops events on VM: %s" vm_id ;
    let module Client = (val make_client queue_name : XENOPS) in
    with_lock events_suppressed_on_m (fun () ->
        Hashtbl.add events_suppressed_on vm_id ()
    ) ;
    finally f (fun () ->
        with_lock events_suppressed_on_m (fun () ->
            Hashtbl.remove events_suppressed_on vm_id ;
            if not (Hashtbl.mem events_suppressed_on vm_id) then (
              debug "re-enabled xenops events on VM: %s; refreshing VM" vm_id ;
              Client.UPDATES.refresh_vm dbg vm_id ;
              wait queue_name dbg vm_id () ;
              Condition.broadcast events_suppressed_on_c
            ) else
              while are_suppressed vm_id do
                debug "waiting for events to become re-enabled" ;
                Condition.wait events_suppressed_on_c events_suppressed_on_m
              done
        )
    )
end

let update_vm ~__context id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    if Events_from_xenopsd.are_suppressed id then
      debug "xenopsd event: ignoring event for VM (VM %s migrating away)" id
    else
      let self = Db.VM.get_by_uuid ~__context ~uuid:id in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self = localhost then
        let previous = Xenops_cache.find_vm id in
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self) : XENOPS)
        in
        let info = try Some (Client.VM.stat dbg id) with _ -> None in
        if Option.map snd info <> previous then (
          debug "xenopsd event: processing event for VM %s" id ;
          if info = None then
            debug "xenopsd event: VM state missing: assuming VM has shut down" ;
          let should_update_allowed_operations = ref false in
          let different f =
            let a = Option.map (fun x -> f (snd x)) info in
            let b = Option.map f previous in
            a <> b
          in
          (* Helpers to create and update guest metrics when needed *)
          let lookup state key = List.assoc_opt key state.Vm.guest_agent in
          let list state dir =
            let dir =
              if dir.[0] = '/' then
                String.sub dir 1 (String.length dir - 1)
              else
                dir
            in
            let results =
              List.filter_map
                (fun (path, _) ->
                  if String.starts_with ~prefix:dir path then
                    let rest =
                      String.sub path (String.length dir)
                        (String.length path - String.length dir)
                    in
                    match
                      List.filter (fun x -> x <> "") (String.split '/' rest)
                    with
                    | x :: _ ->
                        Some x
                    | _ ->
                        None
                  else
                    None
                )
                state.Vm.guest_agent
              |> Listext.setify
            in
            results
          in
          let create_guest_metrics_if_needed () =
            let gm = Db.VM.get_guest_metrics ~__context ~self in
            if gm = Ref.null then
              Option.iter
                (fun (_, state) ->
                  List.iter
                    (fun domid ->
                      try
                        let new_gm_ref =
                          Xapi_guest_agent.create_and_set_guest_metrics
                            (lookup state) (list state) ~__context ~domid
                            ~uuid:id
                            ~pV_drivers_detected:state.pv_drivers_detected
                        in
                        debug
                          "xenopsd event: created guest metrics %s for VM %s"
                          (Ref.string_of new_gm_ref) id
                      with e ->
                        error "Caught %s: while creating VM %s guest metrics"
                          (Printexc.to_string e) id
                    )
                    state.domids
                )
                info
          in
          let check_guest_agent () =
            Option.iter
              (fun (_, state) ->
                Option.iter
                  (fun oldstate ->
                    let old_ga = oldstate.Vm.guest_agent in
                    let new_ga = state.Vm.guest_agent in
                    (* Remove memory keys *)
                    let ignored_keys =
                      ["data/meminfo_free"; "data/updated"; "data/update_cnt"]
                    in
                    let remove_ignored ga =
                      List.fold_left
                        (fun acc k -> List.filter (fun x -> fst x <> k) acc)
                        ga ignored_keys
                    in
                    let old_ga = remove_ignored old_ga in
                    let new_ga = remove_ignored new_ga in
                    if new_ga <> old_ga then (
                      debug
                        "Will update VM.allowed_operations because guest_agent \
                         has changed." ;
                      should_update_allowed_operations := true
                    ) else
                      debug
                        "Supressing VM.allowed_operations update because \
                         guest_agent data is largely the same"
                  )
                  previous ;
                List.iter
                  (fun domid ->
                    try
                      debug "xenopsd event: Updating VM %s domid %d guest_agent"
                        id domid ;
                      Xapi_guest_agent.all (lookup state) (list state)
                        ~__context ~domid ~uuid:id
                        ~pV_drivers_detected:state.pv_drivers_detected
                    with e ->
                      error "Caught %s: while updating VM %s guest_agent"
                        (Printexc.to_string e) id
                  )
                  state.domids
              )
              info
          in
          (* Notes on error handling: if something fails we log and continue, to
             maximise the amount of state which is correctly synced. If something
             does fail then we may end up permanently out-of-sync until either a
             process restart or an event is generated. We may wish to periodically
             inject artificial events IF there has been an event sync failure? *)
          let power_state =
            xenapi_of_xenops_power_state
              (Option.map (fun x -> (snd x).Vm.power_state) info)
          in
          let power_state_before_update =
            Db.VM.get_power_state ~__context ~self
          in
          (* We preserve the current_domain_type of suspended VMs like we preserve
             the currently_attached fields for VBDs/VIFs etc - it's important to know
             whether suspended VMs are going to resume into PV or PVinPVH for example.
             We do this before updating the power_state to maintain the invariant that
             any VM that's not `Halted cannot have an unspecified current_domain_type *)
          if different (fun x -> x.domain_type) && power_state <> `Suspended
          then
            Option.iter
              (fun (_, state) ->
                let metrics = Db.VM.get_metrics ~__context ~self in
                let update domain_type =
                  debug
                    "xenopsd event: Updating VM %s current_domain_type <- %s" id
                    (Record_util.domain_type_to_string domain_type) ;
                  Db.VM_metrics.set_current_domain_type ~__context ~self:metrics
                    ~value:domain_type
                in
                match state.Vm.domain_type with
                | Domain_HVM ->
                    update `hvm
                | Domain_PV ->
                    update `pv
                | Domain_PVinPVH ->
                    update `pv_in_pvh
                | Domain_PVH ->
                    update `pvh
                | Domain_undefined ->
                    if power_state <> `Halted then
                      debug
                        "xenopsd returned an undefined domain type for \
                         non-halted VM %s;assuming this is transient, so not \
                         updating current_domain_type"
                        id
                    else
                      update `unspecified
              )
              info ;
          ( if different (fun x -> x.power_state) then
              try
                debug
                  "Will update VM.allowed_operations because power_state has \
                   changed." ;
                should_update_allowed_operations := true ;
                (* Update ha_always_run before the power_state (if needed), to avoid racing
                   with the HA monitor thread. *)
                let pool = Helpers.get_pool ~__context in
                if
                  power_state = `Halted
                  && not
                       (Db.Pool.get_ha_reboot_vm_on_internal_shutdown ~__context
                          ~self:pool
                       )
                then (
                  Db.VM.set_ha_always_run ~__context ~self ~value:false ;
                  debug "Setting ha_always_run on vm=%s as false after shutdown"
                    (Ref.string_of self)
                ) ;
                debug "xenopsd event: Updating VM %s power_state <- %s" id
                  (Record_util.vm_power_state_to_string power_state) ;

                (* NOTE: Pull xenopsd metadata as soon as possible so that
                   nothing comes inbetween the power state change and the
                   Xenopsd_metadata.pull and overwrites it. *)
                ( if power_state = `Suspended then
                    let md = Xenopsd_metadata.pull ~__context id in
                    match md.Metadata.domains with
                    | None ->
                        error "Suspended VM has no domain-specific metadata"
                    | Some x ->
                        Db.VM.set_last_booted_record ~__context ~self ~value:x ;
                        debug "VM %s last_booted_record set to %s"
                          (Ref.string_of self) x
                ) ;

                (* This will mark VBDs, VIFs as detached and clear resident_on
                   if the VM has permanently shutdown.  current-operations
                   should not be reset as there maybe a checkpoint is ongoing*)
                Xapi_vm_lifecycle.force_state_reset_keep_current_operations
                  ~__context ~self ~value:power_state ;
                if power_state = `Running then
                  create_guest_metrics_if_needed () ;
                if power_state = `Suspended || power_state = `Halted then (
                  Xapi_network.detach_for_vm ~__context ~host:localhost ~vm:self ;
                  Storage_access.reset ~__context ~vm:self
                ) ;
                if power_state = `Halted then (
                  Xenopsd_metadata.delete ~__context id ;
                  !trigger_xenapi_reregister ()
                )
              with e ->
                error "Caught %s: while updating VM %s power_state"
                  (Printexc.to_string e) id
          ) ;
          ( if different (fun x -> x.domids) then
              try
                debug
                  "Will update VM.allowed_operations because domid has changed." ;
                should_update_allowed_operations := true ;
                debug "xenopsd event: Updating VM %s domid" id ;
                Option.iter
                  (fun (_, state) ->
                    match state.Vm.domids with
                    | value :: _ ->
                        Db.VM.set_domid ~__context ~self
                          ~value:(Int64.of_int value)
                    | [] ->
                        ()
                    (* happens when the VM is shutdown *)
                  )
                  info ;
                (* If this is a storage domain, attempt to plug the PBD *)
                Option.iter
                  (fun pbd ->
                    let (_ : Thread.t) =
                      Thread.create
                        (fun () ->
                          (* Don't block the database update thread *)
                          Xapi_pbd.plug ~__context ~self:pbd
                        )
                        ()
                    in
                    ()
                  )
                  (System_domains.pbd_of_vm ~__context ~vm:self)
              with e ->
                error "Caught %s: while updating VM %s domids"
                  (Printexc.to_string e) id
          ) ;
          (* consoles *)
          ( if different (fun x -> x.consoles) then
              try
                debug "xenopsd event: Updating VM %s consoles" id ;
                Option.iter
                  (fun (_, state) ->
                    let localhost = Helpers.get_localhost ~__context in
                    let address =
                      Db.Host.get_address ~__context ~self:localhost
                    in
                    let uri =
                      Uri.(
                        make ~scheme:"https" ~host:address
                          ~path:Constants.console_uri ()
                        |> to_string
                      )
                    in
                    let get_uri_from_location loc =
                      try
                        let n = String.index loc '?' in
                        String.sub loc 0 n
                      with Not_found -> loc
                    in
                    let current_protocols =
                      List.map
                        (fun self ->
                          ( ( Db.Console.get_protocol ~__context ~self
                              |> to_xenops_console_protocol
                            , Db.Console.get_location ~__context ~self
                              |> get_uri_from_location
                            )
                          , self
                          )
                        )
                        (Db.VM.get_consoles ~__context ~self)
                    in
                    let new_protocols =
                      List.map
                        (fun c -> ((c.Vm.protocol, uri), c))
                        state.Vm.consoles
                    in
                    (* Destroy consoles that have gone away *)
                    List.iter
                      (fun protocol ->
                        let self = List.assoc protocol current_protocols in
                        Db.Console.destroy ~__context ~self
                      )
                      (Listext.set_difference
                         (List.map fst current_protocols)
                         (List.map fst new_protocols)
                      ) ;
                    (* Create consoles that have appeared *)
                    List.iter
                      (fun (protocol, _) ->
                        let ref = Ref.make () in
                        let uuid = Uuidx.to_string (Uuidx.make ()) in
                        let location = Printf.sprintf "%s?uuid=%s" uri uuid in
                        let port =
                          try
                            Int64.of_int
                              (List.find
                                 (fun c -> c.Vm.protocol = protocol)
                                 state.Vm.consoles
                              )
                                .port
                          with Not_found -> -1L
                        in
                        Db.Console.create ~__context ~ref ~uuid
                          ~protocol:(to_xenapi_console_protocol protocol)
                          ~location ~vM:self ~other_config:[] ~port
                      )
                      (Listext.set_difference
                         (List.map fst new_protocols)
                         (List.map fst current_protocols)
                      )
                  )
                  info
              with e ->
                error "Caught %s: while updating VM %s consoles"
                  (Printexc.to_string e) id
          ) ;
          ( if different (fun x -> x.memory_target) then
              try
                Option.iter
                  (fun (_, state) ->
                    debug "xenopsd event: Updating VM %s memory_target <- %Ld"
                      id state.Vm.memory_target ;
                    Db.VM.set_memory_target ~__context ~self
                      ~value:state.memory_target
                  )
                  info
              with e ->
                error "Caught %s: while updating VM %s consoles"
                  (Printexc.to_string e) id
          ) ;
          ( if different (fun x -> x.rtc_timeoffset) then
              try
                Option.iter
                  (fun (_, state) ->
                    if state.Vm.rtc_timeoffset <> "" then (
                      debug
                        "xenopsd event: Updating VM %s platform:timeoffset <- \
                         %s"
                        id state.rtc_timeoffset ;
                      ( try
                          Db.VM.remove_from_platform ~__context ~self
                            ~key:Vm_platform.timeoffset
                        with _ -> ()
                      ) ;
                      Db.VM.add_to_platform ~__context ~self
                        ~key:Vm_platform.timeoffset ~value:state.rtc_timeoffset
                    )
                  )
                  info
              with e ->
                error "Caught %s: while updating VM %s rtc/timeoffset"
                  (Printexc.to_string e) id
          ) ;
          if different (fun x -> x.hvm) then
            Option.iter
              (fun (_, state) ->
                let metrics = Db.VM.get_metrics ~__context ~self in
                debug "xenopsd event: Updating VM %s hvm <- %s" id
                  (string_of_bool state.Vm.hvm) ;
                Db.VM_metrics.set_hvm ~__context ~self:metrics
                  ~value:state.Vm.hvm
              )
              info ;
          if different (fun x -> x.nomigrate) then
            Option.iter
              (fun (_, state) ->
                let metrics = Db.VM.get_metrics ~__context ~self in
                debug "xenopsd event: Updating VM %s nomigrate <- %s" id
                  (string_of_bool state.Vm.nomigrate) ;
                Db.VM_metrics.set_nomigrate ~__context ~self:metrics
                  ~value:state.Vm.nomigrate
              )
              info ;
          if different (fun x -> x.nested_virt) then
            Option.iter
              (fun (_, state) ->
                let metrics = Db.VM.get_metrics ~__context ~self in
                debug "xenopsd event: Updating VM %s nested_virt <- %s" id
                  (string_of_bool state.Vm.nested_virt) ;
                Db.VM_metrics.set_nested_virt ~__context ~self:metrics
                  ~value:state.Vm.nested_virt
              )
              info ;
          let update_pv_drivers_detected () =
            Option.iter
              (fun (_, state) ->
                try
                  let gm = Db.VM.get_guest_metrics ~__context ~self in
                  debug "xenopsd event: Updating VM %s PV drivers detected %b"
                    id state.Vm.pv_drivers_detected ;
                  Db.VM_guest_metrics.set_PV_drivers_detected ~__context
                    ~self:gm ~value:state.Vm.pv_drivers_detected ;
                  Db.VM_guest_metrics.set_PV_drivers_up_to_date ~__context
                    ~self:gm ~value:state.Vm.pv_drivers_detected
                with e ->
                  debug "Caught %s: while updating VM %s PV drivers"
                    (Printexc.to_string e) id
              )
              info
          in
          (* Chack last_start_time before updating anything in the guest metrics *)
          ( if different (fun x -> x.last_start_time) then
              try
                Option.iter
                  (fun (_, state) ->
                    let metrics = Db.VM.get_metrics ~__context ~self in
                    (* Clamp time to full seconds, stored timestamps do not
                        have decimals *)
                    let start_time =
                      Float.floor state.Vm.last_start_time |> Date.of_unix_time
                    in
                    let expected_time =
                      Db.VM_metrics.get_start_time ~__context ~self:metrics
                    in
                    if Date.is_later ~than:expected_time start_time then (
                      debug
                        "xenopsd event: Updating VM %s last_start_time <- %s" id
                        Date.(to_rfc3339 (of_unix_time state.Vm.last_start_time)) ;
                      Db.VM_metrics.set_start_time ~__context ~self:metrics
                        ~value:start_time ;
                      if
                        (* VM start and VM reboot *)
                        power_state = `Running
                        && power_state_before_update <> `Suspended
                      then (
                        Xapi_vm_lifecycle.remove_pending_guidance ~__context
                          ~self ~value:`restart_device_model ;
                        Xapi_vm_lifecycle.remove_pending_guidance ~__context
                          ~self ~value:`restart_vm
                      )
                    ) ;
                    create_guest_metrics_if_needed () ;
                    let gm = Db.VM.get_guest_metrics ~__context ~self in
                    let update_time =
                      Db.VM_guest_metrics.get_last_updated ~__context ~self:gm
                    in
                    if update_time < start_time then (
                      debug
                        "VM %s guest metrics update time (%s) < VM start time \
                         (%s): deleting"
                        id
                        (Date.to_rfc3339 update_time)
                        (Date.to_rfc3339 start_time) ;
                      Xapi_vm_helpers.delete_guest_metrics ~__context ~self ;
                      check_guest_agent ()
                    )
                  )
                  info
              with e ->
                error "Caught %s: while updating VM %s last_start_time"
                  (Printexc.to_string e) id
          ) ;
          Option.iter
            (fun (_, state) ->
              List.iter
                (fun domid ->
                  (* Guest metrics could have been destroyed during the last_start_time check
                     by recreating them, we avoid CA-223387 *)
                  create_guest_metrics_if_needed () ;
                  if different (fun x -> x.Vm.uncooperative_balloon_driver) then
                    debug
                      "xenopsd event: VM %s domid %d \
                       uncooperative_balloon_driver = %b"
                      id domid state.Vm.uncooperative_balloon_driver ;
                  if different (fun x -> x.Vm.guest_agent) then
                    check_guest_agent () ;
                  if different (fun x -> x.Vm.pv_drivers_detected) then
                    update_pv_drivers_detected () ;
                  ( if different (fun x -> x.Vm.xsdata_state) then
                      try
                        debug "xenopsd event: Updating VM %s domid %d xsdata" id
                          domid ;
                        Db.VM.set_xenstore_data ~__context ~self
                          ~value:state.Vm.xsdata_state
                      with e ->
                        error "Caught %s: while updating VM %s xsdata"
                          (Printexc.to_string e) id
                  ) ;
                  if different (fun x -> x.Vm.memory_target) then
                    try
                      debug
                        "xenopsd event: Updating VM %s domid %d memory target"
                        id domid ;
                      Rrdd.update_vm_memory_target domid state.Vm.memory_target
                    with e ->
                      error "Caught %s: while updating VM %s memory_target"
                        (Printexc.to_string e) id
                )
                state.Vm.domids
            )
            info ;
          if different (fun x -> x.Vm.vcpu_target) then
            Option.iter
              (fun (_, state) ->
                try
                  debug "xenopsd event: Updating VM %s vcpu_target <- %d" id
                    state.Vm.vcpu_target ;
                  let metrics = Db.VM.get_metrics ~__context ~self in
                  Db.VM_metrics.set_VCPUs_number ~__context ~self:metrics
                    ~value:(Int64.of_int state.Vm.vcpu_target)
                with e ->
                  error "Caught %s: while updating VM %s VCPUs_number"
                    (Printexc.to_string e) id
              )
              info ;
          ( if different (fun x -> x.shadow_multiplier_target) then
              try
                Option.iter
                  (fun (_, state) ->
                    debug
                      "xenopsd event: Updating VM %s shadow_multiplier <- %.2f"
                      id state.Vm.shadow_multiplier_target ;
                    if
                      state.Vm.power_state <> Halted
                      && state.Vm.shadow_multiplier_target >= 0.0
                    then
                      Db.VM.set_HVM_shadow_multiplier ~__context ~self
                        ~value:state.Vm.shadow_multiplier_target
                  )
                  info
              with e ->
                error "Caught %s: while updating VM %s HVM_shadow_multiplier"
                  (Printexc.to_string e) id
          ) ;
          (* Preserve last_boot_CPU_flags when suspending (see current_domain_type) *)
          if different (fun x -> x.Vm.featureset) && power_state <> `Suspended
          then
            Option.iter
              (fun (_, state) ->
                try
                  debug
                    "xenopsd event: Updating VM %s last_boot_CPU_flags <- %s" id
                    state.Vm.featureset ;
                  let vendor =
                    Db.Host.get_cpu_info ~__context ~self:localhost
                    |> List.assoc Xapi_globs.cpu_info_vendor_key
                  in
                  let value =
                    [
                      (Xapi_globs.cpu_info_vendor_key, vendor)
                    ; (Xapi_globs.cpu_info_features_key, state.Vm.featureset)
                    ]
                  in
                  Db.VM.set_last_boot_CPU_flags ~__context ~self ~value
                with e ->
                  error "Caught %s: while updating VM %s last_boot_CPU_flags"
                    (Printexc.to_string e) id
              )
              info ;
          Xenops_cache.update_vm id (Option.map snd info) ;
          if !should_update_allowed_operations then
            Helpers.call_api_functions ~__context (fun rpc session_id ->
                XenAPI.VM.update_allowed_operations ~rpc ~session_id ~self
            )
        )
  with e ->
    error
      "xenopsd event: Caught %s while updating VM: has this VM been removed \
       while this host is offline?"
      (string_of_exn e)

let update_vbd ~__context (id : string * string) =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    if Events_from_xenopsd.are_suppressed (fst id) then
      debug "xenopsd event: ignoring event for VBD (VM %s migrating away)"
        (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        let previous = Xenops_cache.find_vbd id in
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let info = try Some (Client.VBD.stat dbg id) with _ -> None in
        if Option.map snd info <> previous then (
          let vbds = Db.VM.get_VBDs ~__context ~self:vm in
          let vbdrs =
            List.map
              (fun self -> (self, Db.VBD.get_record ~__context ~self))
              vbds
          in
          let linux_device = snd id in
          let device_number = Device_number.of_linux_device linux_device in
          let disk_of dev =
            (* only try matching against disk number if the device is not a
               floppy (as "0" shouldn't match "fda") *)
            match Device_number.bus dev with
            | Ide | Xen ->
                Some (string_of_int Device_number.(disk dev))
            | _ ->
                None
          in
          let disk_number = Option.bind device_number disk_of in
          debug "VM %s VBD userdevices = [ %s ]" (fst id)
            (String.concat "; "
               (List.map (fun (_, r) -> r.API.vBD_userdevice) vbdrs)
            ) ;
          let vbd, vbd_r =
            List.find
              (fun (_, vbdr) ->
                vbdr.API.vBD_userdevice = linux_device
                || Option.is_some disk_number
                   && vbdr.API.vBD_userdevice = Option.get disk_number
              )
              vbdrs
          in
          debug "VBD %s.%s matched device %s" (fst id) (snd id)
            vbd_r.API.vBD_userdevice ;
          Option.iter
            (fun (_, state) ->
              let currently_attached = state.Vbd.plugged || state.Vbd.active in
              debug
                "xenopsd event: Updating VBD %s.%s device <- %s; \
                 currently_attached <- %b"
                (fst id) (snd id) linux_device currently_attached ;
              Db.VBD.set_device ~__context ~self:vbd ~value:linux_device ;
              Db.VBD.set_currently_attached ~__context ~self:vbd
                ~value:currently_attached ;
              ( if state.Vbd.plugged then
                  match state.Vbd.backend_present with
                  | Some (VDI x) ->
                      Option.iter
                        (fun (vdi, _) ->
                          debug "VBD %s.%s backend_present = %s" (fst id)
                            (snd id) x ;
                          Db.VBD.set_VDI ~__context ~self:vbd ~value:vdi ;
                          Db.VBD.set_empty ~__context ~self:vbd ~value:false ;
                          Xapi_vdi.update_allowed_operations ~__context
                            ~self:vdi
                        )
                        (vdi_of_disk ~__context x)
                  | Some d ->
                      error "VBD %s.%s backend_present has unknown disk = %s"
                        (fst id) (snd id)
                        (d |> rpc_of disk |> Jsonrpc.to_string)
                  | None ->
                      if vbd_r.API.vBD_type = `CD then (
                        debug "VBD %s.%s backend_present = None (empty)"
                          (fst id) (snd id) ;
                        Db.VBD.set_empty ~__context ~self:vbd ~value:true ;
                        Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null
                      ) else
                        error "VBD %s.%s is empty but is not a CD" (fst id)
                          (snd id)
              ) ;
              if not (state.Vbd.plugged || state.Vbd.active) then (
                debug "VBD.remove %s.%s" (fst id) (snd id) ;
                try Client.VBD.remove dbg id
                with e -> debug "VBD.remove failed: %s" (Printexc.to_string e)
              )
            )
            info ;
          Xenops_cache.update_vbd id (Option.map snd info) ;
          Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd ;
          if not (Db.VBD.get_empty ~__context ~self:vbd) then
            let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
            Xapi_vdi.update_allowed_operations ~__context ~self:vdi
        )
  with e ->
    error "xenopsd event: Caught %s while updating VBD" (string_of_exn e)

let update_vif ~__context id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    if Events_from_xenopsd.are_suppressed (fst id) then
      debug "xenopsd event: ignoring event for VIF (VM %s migrating away)"
        (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        let previous = Xenops_cache.find_vif id in
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let info = try Some (Client.VIF.stat dbg id) with _ -> None in
        if Option.map snd info <> previous then (
          let vifs = Db.VM.get_VIFs ~__context ~self:vm in
          let vifrs =
            List.map
              (fun self -> (self, Db.VIF.get_record ~__context ~self))
              vifs
          in
          let vif, vifr =
            List.find (fun (_, vifr) -> vifr.API.vIF_device = snd id) vifrs
          in
          Option.iter
            (fun (_, state) ->
              if not (state.Vif.plugged || state.Vif.active) then (
                ( try Xapi_network.deregister_vif ~__context vif
                  with e ->
                    error "Failed to deregister vif: %s" (Printexc.to_string e)
                ) ;
                debug "VIF.remove %s.%s" (fst id) (snd id) ;
                try Client.VIF.remove dbg id
                with e -> debug "VIF.remove failed: %s" (Printexc.to_string e)
              ) ;
              ( match backend_of_vif ~__context ~vif with
              | Network.Sriov _ ->
                  ()
              | Network.Local _ | Network.Remote _ ->
                  if state.plugged then (
                    (* sync MTU *)
                    ( try
                        match state.device with
                        | None ->
                            failwith
                              (Printf.sprintf
                                 "could not determine device id for VIF %s.%s"
                                 (fst id) (snd id)
                              )
                        | Some device ->
                            let dbg =
                              Context.string_of_task_and_tracing __context
                            in
                            let mtu = Net.Interface.get_mtu dbg device in
                            Db.VIF.set_MTU ~__context ~self:vif
                              ~value:(Int64.of_int mtu)
                      with _ ->
                        debug "could not update MTU field on VIF %s.%s" (fst id)
                          (snd id)
                    ) ;
                    (* Clear monitor cache for associated PIF if pass_through_pif_carrier is set *)
                    if !Xapi_globs.pass_through_pif_carrier then
                      let host = Helpers.get_localhost ~__context in
                      let pifs =
                        Xapi_network_attach_helpers.get_local_pifs ~__context
                          ~network:vifr.API.vIF_network ~host
                      in
                      List.iter
                        (fun pif ->
                          let pif_name =
                            Db.PIF.get_device ~__context ~self:pif
                          in
                          Monitor_dbcalls_cache.clear_cache_for_pif ~pif_name
                        )
                        pifs
                  )
              ) ;
              ( match Pvs_proxy_control.find_proxy_for_vif ~__context ~vif with
              | None ->
                  ()
              | Some proxy ->
                  debug
                    "xenopsd event: Updating PVS_proxy for VIF %s.%s \
                     currently_attached <- %b"
                    (fst id) (snd id) state.pvs_rules_active ;
                  if state.pvs_rules_active then (
                    Db.PVS_proxy.set_currently_attached ~__context ~self:proxy
                      ~value:true ;
                    (* force status to be read again by invalidating cache *)
                    Monitor_dbcalls_cache.clear_pvs_status_cache
                      ~vm_uuid:(fst id)
                  ) else
                    Pvs_proxy_control.clear_proxy_state ~__context vif proxy
              ) ;
              debug "xenopsd event: Updating VIF %s.%s currently_attached <- %b"
                (fst id) (snd id)
                (state.plugged || state.active) ;
              Db.VIF.set_currently_attached ~__context ~self:vif
                ~value:(state.plugged || state.active)
            )
            info ;
          Xenops_cache.update_vif id (Option.map snd info) ;
          Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif
        )
  with e ->
    error "xenopsd event: Caught %s while updating VIF" (string_of_exn e)

let update_pci ~__context id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    if Events_from_xenopsd.are_suppressed (fst id) then
      debug "xenopsd event: ignoring event for PCI (VM %s migrating away)"
        (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        let previous = Xenops_cache.find_pci id in
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let info = try Some (Client.PCI.stat dbg id) with _ -> None in
        if Option.map snd info <> previous then (
          let pcis = Db.Host.get_PCIs ~__context ~self:localhost in
          let pcirs =
            List.map
              (fun self -> (self, Db.PCI.get_record ~__context ~self))
              pcis
          in
          let pci, _ =
            List.find (fun (_, pcir) -> pcir.API.pCI_pci_id = snd id) pcirs
          in
          let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
          let eq vgpu = Db.VGPU.get_PCI ~__context ~self:vgpu = pci in
          let vgpu_opt = List.find_opt eq vgpus in
          let attached_in_db =
            List.mem vm (Db.PCI.get_attached_VMs ~__context ~self:pci)
          in
          Option.iter
            (fun (_, state) ->
              debug "xenopsd event: Updating PCI %s.%s currently_attached <- %b"
                (fst id) (snd id) state.Pci.plugged ;
              if attached_in_db && not state.Pci.plugged then
                Db.PCI.remove_attached_VMs ~__context ~self:pci ~value:vm
              else if (not attached_in_db) && state.plugged then (
                Db.PCI.add_attached_VMs ~__context ~self:pci ~value:vm ;
                Db.PCI.set_scheduled_to_be_attached_to ~__context ~self:pci
                  ~value:Ref.null
              ) ;
              Option.iter
                (fun vgpu ->
                  let scheduled =
                    Db.VGPU.get_scheduled_to_be_resident_on ~__context
                      ~self:vgpu
                  in
                  if Db.is_valid_ref __context scheduled && state.Pci.plugged
                  then
                    Helpers.call_api_functions ~__context (fun rpc session_id ->
                        XenAPI.VGPU.atomic_set_resident_on ~rpc ~session_id
                          ~self:vgpu ~value:scheduled
                    ) ;
                  debug
                    "xenopsd event: Update VGPU %s.%s currently_attached <- %b"
                    (fst id) (snd id) state.plugged ;
                  Db.VGPU.set_currently_attached ~__context ~self:vgpu
                    ~value:state.Pci.plugged
                )
                vgpu_opt
            )
            info ;
          Xenops_cache.update_pci id (Option.map snd info)
        )
  with e ->
    error "xenopsd event: Caught %s while updating PCI" (string_of_exn e)

let update_vgpu ~__context id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    if Events_from_xenopsd.are_suppressed (fst id) then
      debug "xenopsd event: ignoring event for VGPU (VM %s migrating away)"
        (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        let previous = Xenops_cache.find_vgpu id in
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let info = try Some (Client.VGPU.stat dbg id) with _ -> None in
        if Option.map snd info <> previous then (
          let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
          let vgpu_records =
            List.map
              (fun self -> (self, Db.VGPU.get_record ~__context ~self))
              vgpus
          in
          let vgpu, vgpu_record =
            List.find
              (fun (_, vgpu_record) -> vgpu_record.API.vGPU_device = snd id)
              vgpu_records
          in
          (* We only proceed if the VGPU is not a passthrough VGPU. In the
           * passthrough case, the VM will have a PCI device, and update_pci
           * will set VGPU.{resident_on;currently_attached}. *)
          if
            Xapi_vgpu_type.requires_passthrough ~__context
              ~self:vgpu_record.API.vGPU_type
            = None
          then
            Option.iter
              (fun (_, state) ->
                ( if state.Vgpu.plugged then
                    let scheduled =
                      Db.VGPU.get_scheduled_to_be_resident_on ~__context
                        ~self:vgpu
                    in
                    if Db.is_valid_ref __context scheduled then
                      Helpers.call_api_functions ~__context
                        (fun rpc session_id ->
                          XenAPI.VGPU.atomic_set_resident_on ~rpc ~session_id
                            ~self:vgpu ~value:scheduled
                      )
                ) ;
                Db.VGPU.set_currently_attached ~__context ~self:vgpu
                  ~value:(state.plugged || state.active) ;
                if not (state.plugged || state.active) then (
                  debug "VGPU.remove %s.%s" (fst id) (snd id) ;
                  try Client.VGPU.remove dbg id
                  with e ->
                    debug "VGPU.remove failed: %s" (Printexc.to_string e)
                )
              )
              info ;
          Xenops_cache.update_vgpu id (Option.map snd info)
        )
  with e ->
    error "xenopsd event: Caught %s while updating VGPU" (string_of_exn e)

let update_vusb ~__context (id : string * string) =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    if Events_from_xenopsd.are_suppressed (fst id) then
      debug "xenopsd event: ignoring event for VUSB (VM %s migrating away)"
        (fst id)
    else
      let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
      let localhost = Helpers.get_localhost ~__context in
      if Db.VM.get_resident_on ~__context ~self:vm = localhost then
        let previous = Xenops_cache.find_vusb id in
        let dbg = Context.string_of_task_and_tracing __context in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let info = try Some (Client.VUSB.stat dbg id) with _ -> None in
        if Option.map snd info <> previous then (
          let pusb, _ =
            Db.VM.get_VUSBs ~__context ~self:vm
            |> List.map (fun self -> Db.VUSB.get_USB_group ~__context ~self)
            |> List.map (fun usb_group ->
                   Helpers.get_first_pusb ~__context usb_group
               )
            |> List.map (fun self -> (self, Db.PUSB.get_record ~__context ~self))
            |> List.find (fun (_, pusbr) ->
                   "vusb" ^ pusbr.API.pUSB_path = snd id
               )
          in
          let usb_group = Db.PUSB.get_USB_group ~__context ~self:pusb in
          let vusb = Helpers.get_first_vusb ~__context usb_group in
          Option.iter
            (fun (_, state) ->
              debug "xenopsd event: Updating USB %s.%s; plugged <- %b" (fst id)
                (snd id) state.Vusb.plugged ;
              let currently_attached = state.Vusb.plugged in
              Db.VUSB.set_currently_attached ~__context ~self:vusb
                ~value:currently_attached
            )
            info ;
          Xenops_cache.update_vusb id (Option.map snd info) ;
          Xapi_vusb_helpers.update_allowed_operations ~__context ~self:vusb
        )
  with e ->
    error "xenopsd event: Caught %s while updating VUSB" (string_of_exn e)

exception Not_a_xenops_task

let wrap queue_name id = TaskHelper.Xenops (queue_name, id)

let unwrap x =
  match x with
  | TaskHelper.Xenops (queue_name, id) ->
      (queue_name, id)
  | _ ->
      raise Not_a_xenops_task

let register_task __context ?cancellable queue_name id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  TaskHelper.register_task __context ?cancellable (wrap queue_name id) ;
  id

let unregister_task __context queue_name id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  TaskHelper.unregister_task __context (wrap queue_name id) ;
  id

let update_task ~__context queue_name id =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    let self = TaskHelper.id_to_task_exn (TaskHelper.Xenops (queue_name, id)) in
    (* throws Not_found *)
    let dbg = Context.string_of_task_and_tracing __context in
    let module Client = (val make_client queue_name : XENOPS) in
    let task_t = Client.TASK.stat dbg id in
    match task_t.Task.state with
    | Task.Pending x ->
        Db.Task.set_progress ~__context ~self ~value:x ;
        if not task_t.Task.cancellable then
          let allowed_operations =
            Db.Task.get_allowed_operations ~__context ~self
          in
          if List.mem `cancel allowed_operations then (
            let allowed_operations' =
              List.filter (fun x -> x <> `cancel) allowed_operations
            in
            debug "Set task %s to not cancellable."
              (Ref.really_pretty_and_small self) ;
            Db.Task.set_allowed_operations ~__context ~self
              ~value:allowed_operations'
          )
    | _ ->
        ()
  with
  | Not_found ->
      (* Since this is called on all tasks, possibly after the task has been
         		   destroyed, it's safe to ignore a Not_found exception here. *)
      ()
  | e ->
      error "xenopsd event: Caught %s while updating task" (string_of_exn e)

let rec events_watch ~__context cancel queue_name from =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  if Xapi_fist.delay_xenopsd_event_threads () then Thread.delay 30.0 ;
  let module Client = (val make_client queue_name : XENOPS) in
  let barriers, events, next = Client.UPDATES.get dbg from None in
  if !cancel then
    raise (Api_errors.Server_error (Api_errors.task_cancelled, [])) ;
  let done_events = ref [] in
  let already_done x = List.mem x !done_events in
  let add_event x = done_events := x :: !done_events in
  let do_updates l =
    let open Dynamic in
    List.iter
      (fun ev ->
        debug "Processing event: %s"
          (ev |> Dynamic.rpc_of_id |> Jsonrpc.to_string) ;
        if already_done ev then
          debug "Skipping (already processed this round)"
        else (
          add_event ev ;
          match ev with
          | Vm id ->
              debug "xenops event on VM %s" id ;
              update_vm ~__context id
          | Vbd id ->
              debug "xenops event on VBD %s.%s" (fst id) (snd id) ;
              update_vbd ~__context id
          | Vif id ->
              debug "xenops event on VIF %s.%s" (fst id) (snd id) ;
              update_vif ~__context id
          | Pci id ->
              debug "xenops event on PCI %s.%s" (fst id) (snd id) ;
              update_pci ~__context id
          | Vgpu id ->
              debug "xenops event on VGPU %s.%s" (fst id) (snd id) ;
              update_vgpu ~__context id
          | Vusb id ->
              debug "xenops event on VUSB %s.%s" (fst id) (snd id) ;
              update_vusb ~__context id
          | Task id ->
              debug "xenops event on Task %s" id ;
              update_task ~__context queue_name id
        )
      )
      l
  in
  List.iter
    (fun (id, b_events) ->
      debug "Processing barrier %d" id ;
      do_updates b_events ;
      Events_from_xenopsd.wakeup queue_name dbg id
    )
    barriers ;
  do_updates events ;
  events_watch ~__context cancel queue_name (Some next)

let events_from_xenopsd queue_name =
  Server_helpers.exec_with_new_task (Printf.sprintf "%s events" queue_name)
    (fun __context ->
      while true do
        try events_watch ~__context (ref false) queue_name None
        with e ->
          error "%s event thread caught: %s" queue_name (string_of_exn e) ;
          Thread.delay 10.
      done
  )

let refresh_vm ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let id = id_of_vm ~__context ~self in
  info "xenops: UPDATES.refresh_vm %s" id ;
  let dbg = Context.string_of_task_and_tracing __context in
  let queue_name = queue_of_vm ~__context ~self in
  let module Client = (val make_client queue_name : XENOPS) in
  Client.UPDATES.refresh_vm dbg id ;
  Events_from_xenopsd.wait queue_name dbg id ()

let resync_resident_on ~__context =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let localhost = Helpers.get_localhost ~__context in
  let domain0 = Helpers.get_domain_zero ~__context in
  (* Get a list of all the ids of VMs that Xapi thinks are resident here
     (apart from domain0, which is not managed by xenopsd and is therefore
     irrelevant here) *)
  let resident_vms_in_db =
    Db.Host.get_resident_VMs ~__context ~self:localhost
    |> List.filter (fun self -> self <> domain0)
    |> List.map (fun self -> (id_of_vm ~__context ~self, self))
  in
  (* Get a list of VMs that the xenopsds know about with their xenopsd client *)
  let vms_in_xenopsds =
    List.concat_map
      (fun queue_name ->
        let module Client = (val make_client queue_name : XENOPS) in
        let vms = Client.VM.list dbg () in
        List.map (fun (vm, state) -> ((vm.Vm.id, state), queue_name)) vms
      )
      (all_known_xenopsds ())
  in
  (* The list of VMs xenopsd knows about that (xapi knows about at all,
     xapi has no idea about at all) *)
  let xenopsd_vms_in_xapi, xenopsd_vms_not_in_xapi =
    List.partition
      (fun ((id, _), _) ->
        try
          vm_of_id ~__context id |> ignore ;
          true
        with _ -> false
      )
      vms_in_xenopsds
  in
  (* Of the VMs xapi knows about, partition that set into VMs xapi believes
     should be running here, and those that it didn't *)
  let _, xapi_thinks_are_not_here =
    List.partition
      (fun ((id, _), _) ->
        List.exists (fun (id', _) -> id = id') resident_vms_in_db
      )
      xenopsd_vms_in_xapi
  in
  (* Of those xapi thinks aren't here, are any running on another host? If
     so, kill the VM here. If they aren't running on another host (to the
     best of our knowledge), set the resident_on to be here. *)
  let xapi_thinks_are_elsewhere, xapi_thinks_are_nowhere =
    List.partition
      (fun ((id, _), _) ->
        let vm_ref = vm_of_id ~__context id in
        Db.is_valid_ref __context (Db.VM.get_resident_on ~__context ~self:vm_ref)
      )
      xapi_thinks_are_not_here
  in
  (* This is the list of VMs xapi thought were running here, but actually
     aren't *)
  let xapi_vms_not_in_xenopsd =
    List.filter
      (fun (id, _) ->
        not (List.exists (fun ((id', _), _) -> id' = id) vms_in_xenopsds)
      )
      resident_vms_in_db
  in
  (* Log the state before we do anything *)
  let maybe_log_em msg prefix l =
    if l <> [] then (
      debug "%s" msg ;
      List.iter (fun ((id, _), queue) -> debug "%s %s (%s)" prefix id queue) l
    )
  in
  maybe_log_em
    "The following VMs are known to xenopsd that xapi does not know about"
    "In xenopsd but unknown to xapi: " xenopsd_vms_not_in_xapi ;
  maybe_log_em
    "The following VMs are known to xenopsd but xapi thinks are running \
     elsewhere."
    "In xenopsd but resident elsewhere: " xapi_thinks_are_elsewhere ;
  (* This is bad if they're running! *)
  maybe_log_em
    "The following VMs are known to xenopsd but xapi thinks are running \
     nowhere."
    "In xenopsd but resident nowhere: " xapi_thinks_are_nowhere ;
  (* This is pretty bad! *)
  if xapi_vms_not_in_xenopsd <> [] then (
    debug
      "The following VMs are not known to xenopsd, but xapi thought they \
       should have been" ;
    List.iter
      (fun (id, _) -> debug "Should have been known to xenopsd: %s" id)
      xapi_vms_not_in_xenopsd
  ) ;
  (* Destroy any VMs running that aren't in Xapi's database, or that xapi
     thinks are running on another host *)
  List.iter
    (fun ((id, state), queue_name) ->
      let module Client = (val make_client queue_name : XENOPS) in
      info "VM %s is known to xenopsd but isn't supposed to be: terminating" id ;
      if state.Vm.power_state <> Halted then (
        info
          "VM %s was actually running. This can cause data corruption, \
           therefore terminating"
          id ;
        Client.VM.shutdown dbg id None |> wait_for_task queue_name dbg |> ignore
      ) ;
      Client.VM.remove dbg id
    )
    (xenopsd_vms_not_in_xapi @ xapi_thinks_are_elsewhere) ;
  (* Sync resident_on state in Xapi for VMs running by local Xenopsds that
     xapi didn't think were anywhere. We set resident_on to be this host so that
     the events thread will be aware of it. If it's not running, the events thread will
     remove the metadata from xenopsd and reset resident_on. *)
  List.iter
    (fun ((id, _), _) ->
      let vm = vm_of_id ~__context id in
      info
        "Setting resident_on for VM %s to be this host as xenopsd is aware of \
         it"
        id ;
      Db.VM.set_resident_on ~__context ~self:vm ~value:localhost
    )
    xapi_thinks_are_nowhere ;
  List.iter
    (fun ((id, state), _queue_name) ->
      match xenapi_of_xenops_power_state (Some state.Vm.power_state) with
      | `Running | `Paused ->
          add_caches id
      | _ ->
          ()
    )
    xenopsd_vms_in_xapi ;
  (* Sync VM state in Xapi for VMs not running on this host *)
  List.iter
    (fun (id, vm) ->
      info
        "VM %s was marked as resident here in the DB but isn't known to \
         xenopsd. Resetting in DB"
        id ;
      Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted ;
      Db.VM.set_resident_on ~__context ~self:vm ~value:Ref.null
    )
    xapi_vms_not_in_xenopsd

let resync_all_vms ~__context =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  (* This should now be correct *)
  let localhost = Helpers.get_localhost ~__context in
  let domain0 = Helpers.get_domain_zero ~__context in
  let resident_vms_in_db =
    Db.Host.get_resident_VMs ~__context ~self:localhost
    |> List.filter (fun self -> self <> domain0)
  in
  List.iter (fun vm -> refresh_vm ~__context ~self:vm) resident_vms_in_db

(* experimental feature for hard-pinning vcpus *)
let hard_numa_enabled ~__context =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let pool = Helpers.get_pool ~__context in
  let restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
  List.assoc_opt "restrict_hard_numa" restrictions = Some "false"

let set_numa_affinity_policy ~__context ~value =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task __context in
  let open Xapi_xenops_queue in
  let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
  let value =
    let open Xenops_interface.Host in
    match value with
    | `any ->
        Some Any
    | `best_effort when hard_numa_enabled ~__context ->
        Some Best_effort_hard
    | `best_effort ->
        Some Best_effort
    | `default_policy ->
        None
  in
  Client.HOST.set_numa_affinity_policy dbg value

let on_xapi_restart ~__context =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let host = Helpers.get_localhost ~__context in
  let value = Db.Host.get_numa_affinity_policy ~__context ~self:host in
  info "Setting NUMA affinity policy in xenopsd on startup to %s"
    (Record_util.host_numa_affinity_policy_to_string value) ;
  set_numa_affinity_policy ~__context ~value ;

  info "Resynchronizing VM state with xenopsd" ;
  resync_resident_on ~__context ;
  (* For all available xenopsds, start the event thread. This will cause
     events on everything xenopsd knows about, hence a refresh of all VMs. *)
  List.iter
    (fun queue_name ->
      let (_ : Thread.t) = Thread.create events_from_xenopsd queue_name in
      ()
    )
    (all_known_xenopsds ()) ;
  resync_all_vms ~__context ;
  info "applying guest agent configuration during restart" ;
  let pool = Helpers.get_pool ~__context in
  let config = Db.Pool.get_guest_agent_config ~__context ~self:pool in
  apply_guest_agent_config ~__context config

let assert_resident_on ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let localhost = Helpers.get_localhost ~__context in
  if not (Db.VM.get_resident_on ~__context ~self = localhost) then
    Helpers.internal_error "the VM %s is not resident on this host"
      (Ref.string_of self)

module Events_from_xapi = struct
  let greatest_token = ref ""

  let c = Condition.create ()

  let m = Mutex.create ()

  let wait ~__context ~self =
    assert_resident_on ~__context ~self ;
    let t =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          XenAPI.Event.inject ~rpc ~session_id ~_class:"VM"
            ~_ref:(Ref.string_of self)
      )
    in
    debug "Waiting for token greater than: %s" t ;
    with_lock m (fun () ->
        while !greatest_token < t do
          Condition.wait c m
        done
    )

  let broadcast new_token =
    with_lock m (fun () ->
        greatest_token := new_token ;
        Condition.broadcast c
    )
end

(* XXX: PR-1255: this will be receiving too many events and we may wish to synchronise
   updates to the VM metadata and resident_on fields *)
(* XXX: PR-1255: we also want to only listen for events on VMs and fields we care about *)
let events_from_xapi () =
  let open Event_types in
  Server_helpers.exec_with_new_task "xapi events" (fun __context ->
      let localhost = Helpers.get_localhost ~__context in
      let localhost' = Ref.string_of localhost in
      let token = ref "" in
      let stop = ref false in
      while not !stop do
        try
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              (trigger_xenapi_reregister :=
                 fun () ->
                   debug
                     "triggering xapi event thread to re-register via event \
                      injection" ;
                   try
                     let _ =
                       XenAPI.Event.inject ~rpc ~session_id ~_class:"host"
                         ~_ref:localhost'
                     in
                     ()
                   with e ->
                     error "Waking up the xapi event thread: %s"
                       (string_of_exn e)
              ) ;
              (* We register for events on resident_VMs only *)
              let resident_VMs =
                Db.Host.get_resident_VMs ~__context ~self:localhost
              in
              let uuids =
                StringSet.of_list
                  (List.rev_map
                     (fun self -> Db.VM.get_uuid ~__context ~self)
                     resident_VMs
                  )
              in
              let cached = StringSet.of_list (Xenops_cache.list ()) in
              let missing_in_cache = StringSet.diff uuids cached in
              let extra_in_cache = StringSet.diff cached uuids in
              if not (StringSet.is_empty missing_in_cache) then
                error "events_from_xapi: missing from the cache: [ %s ]"
                  (String.concat "; " (StringSet.elements missing_in_cache)) ;
              if not (StringSet.is_empty extra_in_cache) then
                error "events_from_xapi: extra items in the cache: [ %s ]"
                  (String.concat "; " (StringSet.elements extra_in_cache)) ;
              let classes =
                Printf.sprintf "host/%s" localhost'
                :: List.map
                     (fun x -> Printf.sprintf "VM/%s" (Ref.string_of x))
                     resident_VMs
              in
              (* NB we re-use the old token so we don't get events we've already
                 received BUT we will not necessarily receive events for the new VMs *)
              let reregister = ref false in
              while not !reregister do
                let api_timeout = 60. in
                let timeout =
                  30.
                  +. api_timeout
                  +. !Xapi_database.Db_globs.master_connection_reset_timeout
                in
                let timebox_rpc =
                  Helpers.make_timeboxed_rpc ~__context timeout
                in
                let from =
                  try
                    XenAPI.Event.from ~rpc:timebox_rpc ~session_id ~classes
                      ~token:!token ~timeout:api_timeout
                    |> event_from_of_rpc
                  with e ->
                    Debug.log_backtrace e (Backtrace.get e) ;
                    raise e
                in
                if List.length from.events > 200 then
                  warn "Warning: received more than 200 events!" ;
                List.iter
                  (function
                    | {ty= "vm"; reference= vm'; _} -> (
                        let vm = Ref.of_string vm' in
                        try
                          let id = id_of_vm ~__context ~self:vm in
                          let resident_here =
                            Db.VM.get_resident_on ~__context ~self:vm
                            = localhost
                          in
                          debug "Event on VM %s; resident_here = %b" id
                            resident_here ;
                          if resident_here then
                            Xenopsd_metadata.update ~__context ~self:vm
                            |> ignore
                        with e ->
                          if not (Db.is_valid_ref __context vm) then
                            debug
                              "VM %s has been removed: event on it will be \
                               ignored"
                              (Ref.string_of vm)
                          else (
                            error
                              "Caught %s while processing XenAPI event for VM \
                               %s"
                              (Printexc.to_string e) (Ref.string_of vm) ;
                            raise e
                          )
                      )
                    | {ty= "host"; reference= t; _} when t = localhost' ->
                        debug
                          "Woken event thread: updating list of event \
                           subscriptions" ;
                        reregister := true
                    | _ ->
                        warn
                          "Received event for something we didn't register for!"
                    )
                  from.events ;
                token := from.token ;
                Events_from_xapi.broadcast !token
              done
          )
        with
        | Api_errors.Server_error (code, _)
          when code = Api_errors.session_invalid ->
            debug
              "Caught SESSION_INVALID listening to events from xapi. \
               Restarting thread immediately."
        | Api_errors.Server_error (code, _)
          when code = Api_errors.xen_incompatible ->
            warn
              "Stopping events-from-xapi thread due to Xen/libxenctrl \
               incompatibility" ;
            stop := true
        | e ->
            debug
              "Caught %s listening to events from xapi. Restarting thread \
               after 15 seconds."
              (string_of_exn e) ;
            (* Start from scratch *)
            token := "" ;
            Thread.delay 15.
      done
  )

let success_task queue_name f dbg id =
  let module Client = (val make_client queue_name : XENOPS) in
  finally
    (fun () ->
      let t = Client.TASK.stat dbg id in
      match t.Task.state with
      | Task.Completed r ->
          f t ; r.Task.result
      | Task.Failed x ->
          let exn =
            match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty x with
            | Ok e ->
                Xenopsd_error e
            | Error (`Msg m) ->
                failwith
                  (Printf.sprintf
                     "Internal error unmarshalling error from xenopsd: %s" m
                  )
          in
          let bt =
            Backtrace.t_of_sexp (Sexplib.Sexp.of_string t.Task.backtrace)
          in
          Backtrace.add exn bt ; raise exn
      | Task.Pending _ ->
          failwith "task pending"
    )
    (fun () -> Client.TASK.destroy dbg id)

(* Catch any uncaught xenops exceptions and transform into the most relevant XenAPI error.
   We do not want a XenAPI client to see a raw xenopsd error. *)
let transform_xenops_exn ~__context ~vm queue_name f =
  try f ()
  with e -> (
    Backtrace.is_important e ;
    let reraise code params =
      error "Re-raising as %s [ %s ]" code (String.concat "; " params) ;
      let e' = Api_errors.Server_error (code, params) in
      Backtrace.reraise e e'
    in
    let internal fmt =
      Printf.ksprintf (fun x -> reraise Api_errors.internal_error [x]) fmt
    in
    match e with
    | Xenopsd_error e' -> (
      match e' with
      | Internal_error msg ->
          internal "xenopsd internal error: %s" msg
      | Already_exists (thing, id) ->
          internal "Object with type %s and id %s already exists in xenopsd"
            thing id
      | Does_not_exist (thing, id) ->
          internal "Object with type %s and id %s does not exist in xenopsd"
            thing id
      | Unimplemented fn ->
          reraise Api_errors.not_implemented [fn]
      | Domain_not_built ->
          internal "domain has not been built"
      | Invalid_vcpus n ->
          internal
            "the maximum number of vcpus configured for this VM is currently: \
             %d"
            n
      | Bad_power_state (found, expected) ->
          let f x =
            xenapi_of_xenops_power_state (Some x)
            |> Record_util.vm_power_state_to_string
          in
          let found = f found and expected = f expected in
          reraise Api_errors.vm_bad_power_state
            [Ref.string_of vm; expected; found]
      | Failed_to_acknowledge_shutdown_request ->
          reraise Api_errors.vm_failed_shutdown_ack [Ref.string_of vm]
      | Failed_to_acknowledge_suspend_request ->
          reraise Api_errors.vm_failed_suspend_ack [Ref.string_of vm]
      | Failed_to_shutdown (id, timeout) ->
          reraise Api_errors.vm_shutdown_timeout
            [vm_of_id ~__context id |> Ref.string_of; string_of_float timeout]
      | Failed_to_suspend (id, timeout) ->
          reraise Api_errors.vm_suspend_timeout
            [vm_of_id ~__context id |> Ref.string_of; string_of_float timeout]
      | Device_is_connected ->
          internal "Cannot remove device because it is connected to a VM"
      | Device_not_connected ->
          internal "Device is not connected"
      | Device_detach_rejected (cls, id, msg) ->
          reraise Api_errors.device_detach_rejected [cls; id; msg]
      | Media_not_ejectable ->
          internal "the media in this drive cannot be ejected"
      | Media_present ->
          internal "there is already media in this drive"
      | Media_not_present ->
          internal "there is no media in this drive"
      | No_bootable_device ->
          internal "there is no bootable device"
      | Bootloader_error (uuid, msg) ->
          let vm = Db.VM.get_by_uuid ~__context ~uuid in
          reraise Api_errors.bootloader_failed [Ref.string_of vm; msg]
      | Cannot_free_this_much_memory (needed, free) ->
          reraise Api_errors.host_not_enough_free_memory
            [Int64.to_string needed; Int64.to_string free]
      | Vms_failed_to_cooperate vms ->
          let vms' =
            List.map
              (fun uuid -> Db.VM.get_by_uuid ~__context ~uuid |> Ref.string_of)
              vms
          in
          reraise Api_errors.vms_failed_to_cooperate vms'
      | IO_error ->
          reraise Api_errors.vdi_io_error ["I/O error saving VM suspend image"]
      | Failed_to_contact_remote_service _ ->
          reraise Api_errors.vm_migrate_contact_remote_service_failed []
      | Hook_failed (script, reason, stdout, i) ->
          reraise Api_errors.xapi_hook_failed [script; reason; stdout; i]
      | Not_enough_memory needed ->
          internal "there was not enough memory (needed %Ld bytes)" needed
      | Cancelled id ->
          let task =
            try TaskHelper.id_to_task_exn (TaskHelper.Xenops (queue_name, id))
            with _ ->
              debug "xenopsd task id %s is not associated with a XenAPI task" id ;
              Ref.null
          in
          reraise Api_errors.task_cancelled [Ref.string_of task]
      | Storage_backend_error (code, params) ->
          reraise code params
      | PCIBack_not_loaded ->
          internal "pciback has not loaded"
      | Failed_to_start_emulator (uuid, name, msg) ->
          let vm = Db.VM.get_by_uuid ~__context ~uuid in
          reraise Api_errors.failed_to_start_emulator
            [Ref.string_of vm; name; msg]
      | Ballooning_timeout_before_migration ->
          reraise Api_errors.ballooning_timeout_before_migration
            [Ref.string_of vm]
      | Unknown_error ->
          internal "Unknown error returned from xenopsd"
      | Failed_to_run_script reason ->
          reraise Api_errors.xenapi_plugin_failure [reason]
    )
    | e ->
        raise e
  )

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
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let id = id_of_vm ~__context ~self in
  debug "VM %s set_resident_on" id ;
  let localhost = Helpers.get_localhost ~__context in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      XenAPI.VM.atomic_set_resident_on ~rpc ~session_id ~vm:self ~host:localhost
  ) ;
  debug
    "Signalling xenapi event thread to re-register, and xenopsd events to sync" ;
  refresh_vm ~__context ~self ;
  !trigger_xenapi_reregister () ;
  (* Any future XenAPI updates will trigger events, but we might have missed one so: *)
  Xenopsd_metadata.update ~__context ~self

let update_debug_info __context t =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let task = Context.get_task_id __context in
  let debug_info =
    List.map (fun (k, v) -> ("debug_info:" ^ k, v)) t.Task.debug_info
  in
  List.iter
    (fun (k, v) ->
      try Db.Task.add_to_other_config ~__context ~self:task ~key:k ~value:v
      with e ->
        debug "Failed to add %s = %s to task %s: %s" k v (Ref.string_of task)
          (Printexc.to_string e)
    )
    debug_info

let sync_with_task_result __context ?cancellable queue_name x =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  x
  |> register_task __context ?cancellable queue_name
  |> wait_for_task queue_name dbg
  |> unregister_task __context queue_name
  |> success_task queue_name (update_debug_info __context) dbg

let sync_with_task __context ?cancellable queue_name x =
  sync_with_task_result __context ?cancellable queue_name x |> ignore

let sync __context queue_name x =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  x
  |> wait_for_task queue_name dbg
  |> success_task queue_name (update_debug_info __context) dbg
  |> ignore

let pause ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.pause %s" id ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VM.pause dbg id |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id () ;
      Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self
        ~expected:`Paused
  )

let unpause ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.unpause %s" id ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VM.unpause dbg id |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id () ;
      check_power_state_is ~__context ~self ~expected:`Running
  )

let request_rdp ~__context ~self enabled =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.request_rdp %s %b" id enabled ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VM.request_rdp dbg id enabled
      |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id ()
  )

let run_script ~__context ~self script =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.run_script %s %s" id script ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      let r =
        Client.VM.run_script dbg id script
        |> sync_with_task_result __context queue_name
      in
      let r = match r with None -> "" | Some rpc -> Jsonrpc.to_string rpc in
      Events_from_xenopsd.wait queue_name dbg id () ;
      r
  )

let set_xenstore_data ~__context ~self xsdata =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.set_xenstore_data %s" id ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VM.set_xsdata dbg id xsdata |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id ()
  )

let set_vcpus ~__context ~self n =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.set_vcpus %s" id ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      try
        Client.VM.set_vcpus dbg id (Int64.to_int n)
        |> sync_with_task __context queue_name ;
        Db.VM.set_VCPUs_at_startup ~__context ~self ~value:n ;
        let metrics = Db.VM.get_metrics ~__context ~self in
        if metrics <> Ref.null then
          Db.VM_metrics.set_VCPUs_number ~__context ~self:metrics ~value:n ;
        Events_from_xenopsd.wait queue_name dbg id ()
      with Xenopsd_error (Invalid_vcpus n) ->
        raise
          (Api_errors.Server_error
             ( Api_errors.invalid_value
             , [
                 "VCPU values must satisfy: 0 < VCPUs ≤ VCPUs_max"
               ; string_of_int n
               ]
             )
          )
  )

let set_shadow_multiplier ~__context ~self target =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.set_shadow_multiplier %s" id ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      try
        Client.VM.set_shadow_multiplier dbg id target
        |> sync_with_task __context queue_name ;
        Events_from_xenopsd.wait queue_name dbg id ()
      with
      | Xenopsd_error (Not_enough_memory needed) ->
          let host = Db.VM.get_resident_on ~__context ~self in
          let free_mem_b =
            Memory_check.host_compute_free_memory_with_maximum_compression
              ~__context ~host None
          in
          raise
            (Api_errors.Server_error
               ( Api_errors.host_not_enough_free_memory
               , [Int64.to_string needed; Int64.to_string free_mem_b]
               )
            )
      | Xenopsd_error (Unimplemented _) ->
          (* The existing behaviour is to ignore this failure *)
          error "VM.set_shadow_multiplier: not supported for PV domains"
  )

let set_memory_dynamic_range ~__context ~self min max =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      debug "xenops: VM.set_memory_dynamic_range %s" id ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VM.set_memory_dynamic_range dbg id min max
      |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id ()
  )

let maybe_refresh_vm ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let queue_name = queue_of_vm ~__context ~self in
  let id = id_of_vm ~__context ~self in
  if vm_exists_in_xenopsd queue_name dbg id then (
    info "VM detected in Xenopsd, flushing outstanding events" ;
    (* By calling with_events_suppressed we can guarentee that an refresh_vm
     * will be called with events enabled and therefore we get Xenopsd into a
     * consistent state with Xapi *)
    Events_from_xenopsd.with_suppressed queue_name dbg id (fun _ -> ())
  )

let start ~__context ~self paused force =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      maybe_refresh_vm ~__context ~self ;
      (* For all devices which we want xenopsd to manage, set currently_attached = true
         so the metadata is pushed. *)
      let empty_vbds_allowed = Helpers.will_have_qemu ~__context ~self in
      let vbds =
        (* xenopsd only manages empty VBDs for HVM guests *)
        let vbds = Db.VM.get_VBDs ~__context ~self in
        if empty_vbds_allowed then
          vbds
        else
          List.filter (fun self -> not (Db.VBD.get_empty ~__context ~self)) vbds
      in
      List.iter
        (fun self -> Db.VBD.set_currently_attached ~__context ~self ~value:true)
        vbds ;
      List.iter
        (fun self -> Db.VIF.set_currently_attached ~__context ~self ~value:true)
        (Db.VM.get_VIFs ~__context ~self) ;
      List.iter
        (fun self -> Db.VUSB.set_currently_attached ~__context ~self ~value:true)
        (Db.VM.get_VUSBs ~__context ~self) ;
      let module Client = (val make_client queue_name : XENOPS) in
      debug "Sending VM %s configuration to xenopsd" (Ref.string_of self) ;
      try
        let id = Xenopsd_metadata.push ~__context ~self in
        Xapi_network.with_networks_attached_for_vm ~__context ~vm:self
          (fun () ->
            info "xenops: VM.start %s" id ;
            if not paused then (
              let vm_start = Client.VM.start dbg id force in
              info "xenops: Queueing VM.unpause %s" id ;
              let vm_unpause = Client.VM.unpause dbg id in
              ( try sync_with_task __context queue_name vm_start
                with e ->
                  (* If the VM.start throws an error, clean up the unpause
                     					     which will fail in an irrelevant manor, then reraise
                     					     the original error *)
                  (try sync __context queue_name vm_unpause with _ -> ()) ;
                  raise e
              ) ;
              (* At this point, the start paused has succeeded. Now
                 					   we _do_ care about any error from unpause *)
              sync_with_task __context queue_name vm_unpause
            ) else
              Client.VM.start dbg id force
              |> sync_with_task __context queue_name
        ) ;
        set_resident_on ~__context ~self ;
        (* set_resident_on syncs both xenopsd and with the xapi event mechanism *)
        check_power_state_is ~__context ~self
          ~expected:(if paused then `Paused else `Running)
      with e ->
        error "Caught exception starting VM: %s" (string_of_exn e) ;
        set_resident_on ~__context ~self ;
        raise e
  )

let start ~__context ~self paused force =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      try start ~__context ~self paused force
      with Xenopsd_error (Bad_power_state (found, expected)) as e ->
        Backtrace.is_important e ;
        let power_state = function
          | Running ->
              "Running"
          | Halted ->
              "Halted"
          | Suspended ->
              "Suspended"
          | Paused ->
              "Paused"
        in
        let exn =
          Api_errors.Server_error
            ( Api_errors.vm_bad_power_state
            , [Ref.string_of self; power_state found; power_state expected]
            )
        in
        Backtrace.reraise e exn
  )

let reboot ~__context ~self timeout =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      assert_resident_on ~__context ~self ;
      let id = id_of_vm ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      maybe_refresh_vm ~__context ~self ;
      (* Ensure we have the latest version of the VM metadata before the reboot *)
      Events_from_xapi.wait ~__context ~self ;
      info "xenops: VM.reboot %s" id ;
      let module Client = (val make_client queue_name : XENOPS) in
      let () =
        finally
          (fun () ->
            Client.VM.reboot dbg id timeout
            |> sync_with_task __context queue_name
          )
          (fun () -> Events_from_xenopsd.wait queue_name dbg id ())
      in
      check_power_state_is ~__context ~self ~expected:`Running
  )

let shutdown ~__context ~self timeout =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      assert_resident_on ~__context ~self ;
      let id = id_of_vm ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      info "xenops: VM.shutdown %s" id ;
      let module Client = (val make_client queue_name : XENOPS) in
      let () =
        finally
          (fun () ->
            Client.VM.shutdown dbg id timeout
            |> sync_with_task __context queue_name
          )
          (fun () -> Events_from_xenopsd.wait queue_name dbg id ())
      in
      Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self
        ~expected:`Halted ;
      (* force_state_reset called from the xenopsd event loop above *)
      if not (Db.VM.get_resident_on ~__context ~self = Ref.null) then
        Helpers.internal_error
          "shutdown: The VM %s is still resident on the host"
          (Ref.string_of self) ;
      List.iter
        (fun vbd ->
          if Db.VBD.get_currently_attached ~__context ~self:vbd then
            Helpers.internal_error
              "shutdown: The VBD %s is still attached to VM %s"
              (Ref.string_of vbd) (Ref.string_of self)
        )
        (Db.VM.get_VBDs ~__context ~self)
  )

let suspend ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      assert_resident_on ~__context ~self ;
      let id = id_of_vm ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      let vm_t, _state = Client.VM.stat dbg id in
      (* XXX: this needs to be at boot time *)
      let space_needed =
        let ram =
          Int64.(of_float (to_float vm_t.Vm.memory_static_max *. 1.2 *. 1.05))
        in
        let vgpu =
          Db.VM.get_VGPUs ~__context ~self
          |> List.map (fun self -> Db.VGPU.get_type ~__context ~self)
          |> List.map (fun self ->
                 Db.VGPU_type.get_framebuffer_size ~__context ~self
             )
          |> List.fold_left Int64.add 0L
        in
        Int64.(ram |> add vgpu |> add 104857600L)
      in
      let suspend_SR = Helpers.choose_suspend_sr ~__context ~vm:self in
      let sm_config =
        [
          (Constants._sm_vm_hint, id)
        ; (* Fully inflate the VDI if the SR supports thin provisioning *)
          (Constants._sm_initial_allocation, Int64.to_string space_needed)
        ]
      in
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          let vdi =
            XenAPI.VDI.create ~rpc ~session_id ~name_label:"Suspend image"
              ~name_description:"Suspend image" ~sR:suspend_SR
              ~virtual_size:space_needed ~sharable:false ~read_only:false
              ~_type:`suspend ~other_config:[] ~xenstore_data:[] ~sm_config
              ~tags:[]
          in
          let d = disk_of_vdi ~__context ~self:vdi |> Option.get in
          Db.VM.set_suspend_VDI ~__context ~self ~value:vdi ;
          try
            let dbg = Context.string_of_task_and_tracing __context in
            info "xenops: VM.suspend %s to %s" id
              (d |> rpc_of disk |> Jsonrpc.to_string) ;
            Client.VM.suspend dbg id d
            |> sync_with_task __context ~cancellable:false queue_name ;
            Events_from_xenopsd.wait queue_name dbg id () ;
            Xapi_vm_lifecycle.assert_final_power_state_is ~__context ~self
              ~expected:`Suspended ;
            if
              (not
                 (Xapi_vm_lifecycle.checkpoint_in_progress ~__context ~vm:self)
              )
              && not (Db.VM.get_resident_on ~__context ~self = Ref.null)
            then
              Helpers.internal_error
                "suspend: The VM %s is still resident on the host"
                (Ref.string_of self)
          with e ->
            error "Caught exception suspending VM: %s" (string_of_exn e) ;
            (* If the domain has suspended, we have to shut it down *)
            Events_from_xenopsd.wait queue_name dbg id () ;
            if Db.VM.get_power_state ~__context ~self = `Suspended then (
              info "VM has already suspended; we must perform a hard_shutdown" ;
              Xapi_vm_lifecycle.force_state_reset ~__context ~self
                ~value:`Halted ;
              !trigger_xenapi_reregister ()
            ) else
              info "VM is still running after failed suspend" ;
            XenAPI.VDI.destroy ~rpc ~session_id ~self:vdi ;
            Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null ;
            raise e
      )
  )

let resume ~__context ~self ~start_paused ~force:_ =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let queue_name = queue_of_vm ~__context ~self in
  let vm_id = id_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name @@ fun () ->
  maybe_refresh_vm ~__context ~self ;
  let vdi = Db.VM.get_suspend_VDI ~__context ~self in
  if vdi = Ref.null then (
    info "VM suspend VDI not found; Performing VM hard_shutdown" ;
    Xapi_vm_lifecycle.force_state_reset ~__context ~self ~value:`Halted ;
    let err_content = ["VM"; Ref.string_of self] in
    raise Api_errors.(Server_error (vm_has_no_suspend_vdi, err_content))
  ) ;
  let d = disk_of_vdi ~__context ~self:vdi |> Option.get in
  let module Client = (val make_client queue_name : XENOPS) in
  (* NB we don't set resident_on because we don't want to
     modify the VM.power_state, {VBD,VIF}.currently_attached in the
     failures cases. This means we must remove the metadata from
     xenopsd on failure. *)
  ( try
      Events_from_xenopsd.with_suppressed queue_name dbg vm_id @@ fun () ->
      debug "Sending VM %s configuration to xenopsd" (Ref.string_of self) ;
      let id = Xenopsd_metadata.push ~__context ~self in

      Xapi_network.with_networks_attached_for_vm ~__context ~vm:self
      @@ fun () ->
      info "%s: VM.resume %s from %s" __FUNCTION__ id
        (d |> rpc_of disk |> Jsonrpc.to_string) ;
      Client.VM.resume dbg id d
      |> sync_with_task __context ~cancellable:false queue_name ;
      if not start_paused then (
        info "%s: VM.unpause %s" __FUNCTION__ id ;
        Client.VM.unpause dbg id
        |> sync_with_task __context ~cancellable:false queue_name
      )
    with e ->
      error "Caught exception resuming VM: %s" (string_of_exn e) ;
      let id = id_of_vm ~__context ~self in
      Xenopsd_metadata.delete ~__context id ;
      raise e
  ) ;
  set_resident_on ~__context ~self ;
  Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null ;
  (* Clearing vGPU metadata should happen as late as possible
   * to make sure we only do it on a successful resume
   *)
  Xapi_gpumon.clear_vgpu_metadata ~__context ~vm:self ;
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      XenAPI.VDI.destroy ~rpc ~session_id ~self:vdi
  ) ;
  check_power_state_is ~__context ~self
    ~expected:(if start_paused then `Paused else `Running)

let s3suspend ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      debug "xenops: VM.s3suspend %s" id ;
      Client.VM.s3suspend dbg id |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id ()
  )

let s3resume ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let queue_name = queue_of_vm ~__context ~self in
  transform_xenops_exn ~__context ~vm:self queue_name (fun () ->
      let id = id_of_vm ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      debug "xenops: VM.s3resume %s" id ;
      Client.VM.s3resume dbg id |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg id ()
  )

let md_of_vbd ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VBD.get_VM ~__context ~self in
  MD.of_vbd ~__context
    ~vm:(Db.VM.get_record ~__context ~self:vm)
    ~vbd:(Db.VBD.get_record ~__context ~self)

let vbd_plug ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VBD.get_VM ~__context ~self in
  let vm_id = id_of_vm ~__context ~self:vm in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      (* Set currently_attached to true before calling VBD.add, so that any
         following metadata push would not rip out the new VBD metadata again.
         Not a great design, but it follows what `start` does. We have plans
         to improve this more generally. *)
      Db.VBD.set_currently_attached ~__context ~self ~value:true ;
      Events_from_xapi.wait ~__context ~self:vm ;
      let vbd = md_of_vbd ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Events_from_xenopsd.with_suppressed queue_name dbg vm_id (fun () ->
          info "xenops: VBD.add %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) ;
          let id = Client.VBD.add dbg vbd in
          info "xenops: VBD.plug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) ;
          Client.VBD.plug dbg id |> sync_with_task __context queue_name
      ) ;
      if not (Db.VBD.get_currently_attached ~__context ~self) then
        Helpers.internal_error "vbd_plug: Unable to plug VBD %s"
          (Ref.string_of self)
  )

let vbd_unplug ~__context ~self force =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VBD.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vbd = md_of_vbd ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      ( try
          info "xenops: VBD.unplug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) ;
          Client.VBD.unplug dbg vbd.Vbd.id force
          |> sync_with_task __context queue_name
        with
      | Xenopsd_error (Does_not_exist _) ->
          info "VBD is not plugged; setting currently_attached to false" ;
          Db.VBD.set_currently_attached ~__context ~self ~value:false
      | Xenopsd_error (Device_detach_rejected (_, _, _)) ->
          raise
            Api_errors.(
              Server_error
                (device_detach_rejected, ["VBD"; Ref.string_of self; ""])
            )
      ) ;
      Events_from_xenopsd.wait queue_name dbg (fst vbd.Vbd.id) () ;
      if Db.VBD.get_currently_attached ~__context ~self then
        Helpers.internal_error "vbd_unplug: Unable to unplug VBD %s"
          (Ref.string_of self)
  )

let vbd_eject_hvm ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VBD.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vbd = md_of_vbd ~__context ~self in
      info "xenops: VBD.eject %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VBD.eject dbg vbd.Vbd.id |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vbd.Vbd.id) () ;
      Events_from_xapi.wait ~__context ~self:vm ;
      if not (Db.VBD.get_empty ~__context ~self) then
        Helpers.internal_error "vbd_eject_hvm: The VBD %s has not been emptied"
          (Ref.string_of self) ;
      let vdi = Db.VBD.get_VDI ~__context ~self in
      if not (vdi = Ref.null) then
        Helpers.internal_error
          "vbd_eject_hvm: The VBD %s is still connected to VDI %s"
          (Ref.string_of self) (Ref.string_of vdi)
  )

let vbd_insert_hvm ~__context ~self ~vdi =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VBD.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vbd = md_of_vbd ~__context ~self in
      let d = disk_of_vdi ~__context ~self:vdi |> Option.get in
      info "xenops: VBD.insert %s.%s %s" (fst vbd.Vbd.id) (snd vbd.Vbd.id)
        (d |> rpc_of disk |> Jsonrpc.to_string) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VBD.insert dbg vbd.Vbd.id d |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vbd.Vbd.id) () ;
      Events_from_xapi.wait ~__context ~self:vm ;
      if Db.VBD.get_empty ~__context ~self then
        Helpers.internal_error "vbd_insert_hvm: The VBD %s is empty"
          (Ref.string_of self) ;
      let vdi' = Db.VBD.get_VDI ~__context ~self in
      if not (vdi' = vdi) then
        Helpers.internal_error
          "vbd_insert_hvm: The VBD %s has been connected to the wrong VDI \
           (expected %s, got %s)"
          (Ref.string_of self) (Ref.string_of vdi) (Ref.string_of vdi)
  )

let has_qemu ~__context ~vm =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let dbg = Context.string_of_task_and_tracing __context in
  let id = Db.VM.get_uuid ~__context ~self:vm in
  let queue_name = queue_of_vm ~__context ~self:vm in
  let module Client = (val make_client queue_name : XENOPS) in
  let _, state = Client.VM.stat dbg id in
  state.Vm.domain_type = Domain_HVM

let ejectable ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VBD.get_VM ~__context ~self in
  has_qemu ~__context ~vm

let vbd_eject ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  if ejectable ~__context ~self then
    vbd_eject_hvm ~__context ~self
  else (
    vbd_unplug ~__context ~self false ;
    Db.VBD.set_empty ~__context ~self ~value:true ;
    Db.VBD.set_VDI ~__context ~self ~value:Ref.null
  )

let vbd_insert ~__context ~self ~vdi =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  if ejectable ~__context ~self then
    vbd_insert_hvm ~__context ~self ~vdi
  else (
    Db.VBD.set_VDI ~__context ~self ~value:vdi ;
    Db.VBD.set_empty ~__context ~self ~value:false ;
    vbd_plug ~__context ~self
  )

let md_of_vif ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  MD.of_vif ~__context
    ~vm:(Db.VM.get_record ~__context ~self:vm)
    ~vif:(self, Db.VIF.get_record ~__context ~self)

let vif_plug ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let vm_id = id_of_vm ~__context ~self:vm in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      (* Set currently_attached to true before calling VIF.add, so that any
         following metadata push would not rip out the new VIF metadata again.
         Not a great design, but it follows what `start` does. We have plans
         to improve this more generally. *)
      Db.VIF.set_currently_attached ~__context ~self ~value:true ;
      Events_from_xapi.wait ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Xapi_network.with_networks_attached_for_vm ~__context ~vm (fun () ->
          Events_from_xenopsd.with_suppressed queue_name dbg vm_id (fun () ->
              info "xenops: VIF.add %s.%s" (fst vif.Vif.id) (snd vif.Vif.id) ;
              let id = Client.VIF.add dbg vif in
              info "xenops: VIF.plug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id) ;
              Client.VIF.plug dbg id |> sync_with_task __context queue_name
          )
      ) ;
      if not (Db.VIF.get_currently_attached ~__context ~self) then
        Helpers.internal_error "vif_plug: Unable to plug VIF %s"
          (Ref.string_of self)
  )

let vif_set_locking_mode ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      info "xenops: VIF.set_locking_mode %s.%s" (fst vif.Vif.id) (snd vif.Vif.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VIF.set_locking_mode dbg vif.Vif.id vif.Vif.locking_mode
      |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ()
  )

let vif_set_pvs_proxy ~__context ~self creating =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      let proxy = if creating then vif.Vif.pvs_proxy else None in
      info "xenops: VIF.set_pvs_proxy %s.%s" (fst vif.Vif.id) (snd vif.Vif.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VIF.set_pvs_proxy dbg vif.Vif.id proxy
      |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ()
  )

let vif_unplug ~__context ~self force =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      info "xenops: VIF.unplug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      try
        Client.VIF.unplug dbg vif.Vif.id force
        |> sync_with_task __context queue_name ;
        (* We need to make sure VIF.stat still works so: wait before calling VIF.remove *)
        Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) () ;
        if Db.VIF.get_currently_attached ~__context ~self then
          Helpers.internal_error "vif_unplug: Unable to unplug VIF %s"
            (Ref.string_of self)
      with Xenopsd_error (Does_not_exist _) ->
        info "VIF is not plugged; setting currently_attached to false" ;
        Db.VIF.set_currently_attached ~__context ~self ~value:false
  )

let vif_move ~__context ~self _network =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      info "xenops: VIF.move %s.%s" (fst vif.Vif.id) (snd vif.Vif.id) ;
      let backend = backend_of_vif ~__context ~vif:self in
      match backend with
      | Network.Sriov _ ->
          Helpers.internal_error
            "vif_move: Unable to move a network SR-IOV backed VIF %s"
            (Ref.string_of self)
      | _ ->
          let dbg = Context.string_of_task_and_tracing __context in
          let module Client = (val make_client queue_name : XENOPS) in
          (* Nb., at this point, the database shows the vif on the new network *)
          Xapi_network.attach_for_vif ~__context ~vif:self () ;
          Client.VIF.move dbg vif.Vif.id backend
          |> sync_with_task __context queue_name ;
          Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) () ;
          if not (Db.VIF.get_currently_attached ~__context ~self) then
            Helpers.internal_error "vif_move: Unable to plug moved VIF %s"
              (Ref.string_of self)
  )

let vif_set_ipv4_configuration ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      info "xenops: VIF.set_ipv4_configuration %s.%s" (fst vif.Vif.id)
        (snd vif.Vif.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VIF.set_ipv4_configuration dbg vif.Vif.id
        vif.Vif.ipv4_configuration
      |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ()
  )

let vif_set_ipv6_configuration ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VIF.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vif = md_of_vif ~__context ~self in
      info "xenops: VIF.set_ipv6_configuration %s.%s" (fst vif.Vif.id)
        (snd vif.Vif.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VIF.set_ipv6_configuration dbg vif.Vif.id
        vif.Vif.ipv6_configuration
      |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vif.Vif.id) ()
  )

let task_cancel ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  try
    let queue_name, id = TaskHelper.task_to_id_exn self |> unwrap in
    let module Client = (val make_client queue_name : XENOPS) in
    let dbg = Context.string_of_task_and_tracing __context in
    info "xenops: TASK.cancel %s" id ;
    Client.TASK.cancel dbg id |> ignore ;
    (* it might actually have completed, we don't care *)
    true
  with
  | Not_found ->
      false
  | Not_a_xenops_task ->
      false

let md_of_vusb ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VUSB.get_VM ~__context ~self in
  let usb_group = Db.VUSB.get_USB_group ~__context ~self in
  let pusb = Helpers.get_first_pusb ~__context usb_group in
  let pusbr = Db.PUSB.get_record ~__context ~self:pusb in
  MD.of_vusb ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~pusb:pusbr

let vusb_unplug_hvm ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VUSB.get_VM ~__context ~self in
  let queue_name = queue_of_vm ~__context ~self:vm in
  transform_xenops_exn ~__context ~vm queue_name (fun () ->
      assert_resident_on ~__context ~self:vm ;
      let vusb = md_of_vusb ~__context ~self in
      info "xenops: VUSB.unplug %s.%s" (fst vusb.Vusb.id) (snd vusb.Vusb.id) ;
      let dbg = Context.string_of_task_and_tracing __context in
      let module Client = (val make_client queue_name : XENOPS) in
      Client.VUSB.unplug dbg vusb.Vusb.id |> sync_with_task __context queue_name ;
      Events_from_xenopsd.wait queue_name dbg (fst vusb.Vusb.id) () ;
      if Db.VUSB.get_currently_attached ~__context ~self then
        Helpers.internal_error "vusb_unplug: Unable to unplug VUSB %s"
          (Ref.string_of self)
  )

let vusb_plugable ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  let vm = Db.VUSB.get_VM ~__context ~self in
  has_qemu ~__context ~vm

let vusb_unplug ~__context ~self =
  let@ __context = Context.with_tracing ~__context __FUNCTION__ in
  if vusb_plugable ~__context ~self then
    vusb_unplug_hvm ~__context ~self
  else
    Helpers.internal_error "vusb_unplug: Unable to unplug vusb %s"
      (Ref.string_of self)

module Observer = struct
  let create ~__context ~uuid ~name_label ~attributes ~endpoints ~enabled =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.create dbg uuid name_label attributes endpoints enabled

  let destroy ~__context ~uuid =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.destroy dbg uuid

  let set_enabled ~__context ~uuid ~enabled =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_enabled dbg uuid enabled

  let set_attributes ~__context ~uuid ~attributes =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_attributes dbg uuid attributes

  let set_endpoints ~__context ~uuid ~endpoints =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_endpoints dbg uuid endpoints

  let init ~__context =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.init dbg

  let set_trace_log_dir ~__context ~dir =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_trace_log_dir dbg dir

  let set_export_interval ~__context ~interval =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_export_interval dbg interval

  let set_max_spans ~__context ~spans =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_max_spans dbg spans

  let set_max_traces ~__context ~traces =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_max_traces dbg traces

  let set_max_file_size ~__context ~file_size =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_max_file_size dbg file_size

  let set_host_id ~__context ~host_id =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_host_id dbg host_id

  let set_compress_tracing_files ~__context ~enabled =
    let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    Client.Observer.set_compress_tracing_files dbg enabled
end
