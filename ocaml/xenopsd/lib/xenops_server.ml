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

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module D = Debug.Make (struct let name = "xenops_server" end)

open D

let internal_error fmt =
  Printf.kprintf
    (fun str ->
      error "%s" str ;
      raise (Xenopsd_error (Internal_error str))
    )
    fmt

let rpc_of ty x = Rpcmarshal.marshal ty.Rpc.Types.ty x

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let domain_shutdown_ack_timeout = ref 60.

type context = {
    transferred_fd: Unix.file_descr option
        (** some API calls take a file descriptor argument *)
}

let make_context () = {transferred_fd= None}

let instance_id = Uuidx.(to_string (make ()))

let query _ _ _ =
  {
    Query.name= "xenops"
  ; vendor= "XCP"
  ; version= "0.1"
  ; features= []
  ; instance_id
  }

let cookie_vgpu_migration = "vgpu_migration"

let cookie_mem_migration = "mem_migration"

let cookie_mem_migration_value = Xapi_version.version

let cookie_mem_compression = "compress"

let cookie_mem_compression_value = "zstd"

let backend = ref None

let get_backend () =
  match !backend with
  | Some x ->
      x
  | None ->
      failwith "No backend implementation set"

let dom0_uuid = ref ""

let set_dom0_uuid uuid = dom0_uuid := uuid

let ignore_exception msg f x =
  try f x
  with Xenopsd_error e ->
    debug "%s: ignoring exception: %s" msg
      (e |> Rpcmarshal.marshal Errors.error.Rpc.Types.ty |> Jsonrpc.to_string)

let filter_prefix prefix xs =
  List.filter_map
    (fun x ->
      if Astring.String.is_prefix ~affix:prefix x then
        Some
          (String.sub x (String.length prefix)
             (String.length x - String.length prefix)
          )
      else
        None
    )
    xs

(* return last/element/in/a/path *)
let basename path =
  match Astring.String.cut ~sep:"/" ~rev:true path with
  | Some (_, base) ->
      base
  | None ->
      internal_error "invalid path: %s" path

type atomic =
  | VBD_eject of Vbd.id
  | VIF_plug of Vif.id
  | VIF_unplug of Vif.id * bool
  | VIF_move of Vif.id * Network.t
  | VIF_set_carrier of Vif.id * bool
  | VIF_set_locking_mode of Vif.id * Vif.locking_mode
  | VIF_set_pvs_proxy of Vif.id * Vif.PVS_proxy.t option
  | VIF_set_ipv4_configuration of Vif.id * Vif.ipv4_configuration
  | VIF_set_ipv6_configuration of Vif.id * Vif.ipv6_configuration
  | VIF_set_active of Vif.id * bool
  (* During migration the domid of a uuid is not stable. To hide this from hooks
     that depend on domids, this allows the caller to provide an additonal uuid
     that can maintain the initial domid *)
  | VM_hook_script_stable of (Vm.id * Xenops_hooks.script * string * Vm.id)
  | VM_hook_script of (Vm.id * Xenops_hooks.script * string)
  | VBD_plug of Vbd.id
  | VBD_epoch_begin of (Vbd.id * disk * bool)
  | VBD_epoch_end of (Vbd.id * disk)
  | VBD_set_qos of Vbd.id
  | VBD_unplug of Vbd.id * bool
  | VBD_insert of Vbd.id * disk
  | VBD_set_active of Vbd.id * bool
  | VM_remove of Vm.id
  | PCI_plug of Pci.id * bool (* use Qmp.add_device *)
  | PCI_unplug of Pci.id
  | PCI_dequarantine of Xenops_interface.Pci.address
  | VUSB_plug of Vusb.id
  | VUSB_unplug of Vusb.id
  | VGPU_set_active of Vgpu.id * bool
  | VGPU_start of (Vgpu.id list * bool)
  | VM_set_xsdata of (Vm.id * (string * string) list)
  | VM_set_vcpus of (Vm.id * int)
  | VM_set_shadow_multiplier of (Vm.id * float)
  | VM_set_memory_dynamic_range of (Vm.id * int64 * int64)
  | VM_pause of Vm.id
  | VM_softreboot of Vm.id
  | VM_unpause of Vm.id
  | VM_request_rdp of (Vm.id * bool)
  | VM_run_script of (Vm.id * string)
  | VM_set_domain_action_request of (Vm.id * domain_action_request option)
  | VM_create_device_model of (Vm.id * bool)
  | VM_destroy_device_model of Vm.id
  | VM_destroy of Vm.id
  | VM_create of (Vm.id * int64 option * Vm.id option * bool) (*no_sharept*)
  | VM_build of (Vm.id * bool)
  | VM_shutdown_domain of (Vm.id * shutdown_request * float)
  | VM_s3suspend of Vm.id
  | VM_s3resume of Vm.id
  | VM_save of (Vm.id * flag list * data * data option)
      (** takes suspend data, plus optionally vGPU state data *)
  | VM_restore of (Vm.id * data * data option)
      (** takes suspend data, plus optionally vGPU state data *)
  | VM_delay of (Vm.id * float)  (** used to suppress fast reboot loops *)
  | VM_rename of (Vm.id * Vm.id * rename_when)
  | VM_import_metadata of (Vm.id * Metadata.t)
  | Parallel of Vm.id * string * atomic list
  | Best_effort of atomic
[@@deriving rpcty]

let string_of_atomic x = x |> rpc_of atomic |> Jsonrpc.to_string

let rec name_of_atomic = function
  | VBD_eject _ ->
      "VBD_eject"
  | VIF_plug _ ->
      "VIF_plug"
  | VIF_unplug _ ->
      "VIF_unplug"
  | VIF_move _ ->
      "VIF_move"
  | VIF_set_carrier _ ->
      "VIF_set_carrier"
  | VIF_set_locking_mode _ ->
      "VIF_set_locking_mode"
  | VIF_set_pvs_proxy _ ->
      "VIF_set_pvs_proxy"
  | VIF_set_ipv4_configuration _ ->
      "VIF_set_ipv4_configuration"
  | VIF_set_ipv6_configuration _ ->
      "VIF_set_ipv6_configuration"
  | VIF_set_active _ ->
      "VIF_set_active"
  | VM_hook_script_stable _ ->
      "VM_hook_script_stable"
  | VM_hook_script _ ->
      "VM_hook_script"
  | VBD_plug _ ->
      "VBD_plug"
  | VBD_epoch_begin _ ->
      "VBD_epoch_begin"
  | VBD_epoch_end _ ->
      "VBD_epoch_end"
  | VBD_set_qos _ ->
      "VBD_set_qos"
  | VBD_unplug _ ->
      "VBD_unplug"
  | VBD_insert _ ->
      "VBD_insert"
  | VBD_set_active _ ->
      "VBD_set_active"
  | VM_remove _ ->
      "VM_remove"
  | PCI_plug _ ->
      "PCI_plug"
  | PCI_unplug _ ->
      "PCI_unplug"
  | PCI_dequarantine _ ->
      "PCI_dequarantine"
  | VUSB_plug _ ->
      "VUSB_plug"
  | VUSB_unplug _ ->
      "VUSB_unplug"
  | VGPU_set_active _ ->
      "VGPU_set_active"
  | VGPU_start _ ->
      "VGPU_start"
  | VM_set_xsdata _ ->
      "VM_set_xsdata"
  | VM_set_vcpus _ ->
      "VM_set_vcpus"
  | VM_set_shadow_multiplier _ ->
      "VM_set_shadow_multiplier"
  | VM_set_memory_dynamic_range _ ->
      "VM_set_memory_dynamic_range"
  | VM_pause _ ->
      "VM_pause"
  | VM_softreboot _ ->
      "VM_softreboot"
  | VM_unpause _ ->
      "VM_unpause"
  | VM_request_rdp _ ->
      "VM_request_rdp"
  | VM_run_script _ ->
      "VM_run_script"
  | VM_set_domain_action_request _ ->
      "VM_set_domain_action_request"
  | VM_create_device_model _ ->
      "VM_create_device_model"
  | VM_destroy_device_model _ ->
      "VM_destroy_device_model"
  | VM_destroy _ ->
      "VM_destroy"
  | VM_create _ ->
      "VM_create"
  | VM_build _ ->
      "VM_build"
  | VM_shutdown_domain _ ->
      "VM_shutdown_domain"
  | VM_s3suspend _ ->
      "VM_s3suspend"
  | VM_s3resume _ ->
      "VM_s3resume"
  | VM_save _ ->
      "VM_save"
  | VM_restore _ ->
      "VM_restore"
  | VM_delay _ ->
      "VM_delay"
  | VM_rename _ ->
      "VM_rename"
  | VM_import_metadata _ ->
      "VM_import_metadata"
  | Parallel (_, _, atomics) ->
      Printf.sprintf "Parallel (%s)"
        (String.concat " | " (List.map name_of_atomic atomics))
  | Best_effort atomic ->
      Printf.sprintf "Best_effort (%s)" (name_of_atomic atomic)

type vm_migrate_op = {
    vmm_id: Vm.id
  ; vmm_vdi_map: (string * string) list
  ; vmm_vif_map: (string * Network.t) list
  ; vmm_vgpu_pci_map: (string * Pci.address) list
  ; vmm_url: string
  ; vmm_tmp_src_id: Vm.id
  ; vmm_tmp_dest_id: Vm.id
  ; vmm_compress: bool
  ; vmm_verify_dest: bool
}
[@@deriving rpcty]

type vm_receive_op = {
    vmr_id: Vm.id
  ; vmr_final_id: Vm.id
  ; vmr_memory_limit: int64
  ; vmr_socket: Unix.file_descr
  ; vmr_handshake: string option  (** handshake protocol *)
  ; vmr_compressed: bool
}
[@@deriving rpcty]

type operation =
  | VM_start of (Vm.id * bool) (* VM id * 'force' *)
  | VM_poweroff of (Vm.id * float option)
  | VM_shutdown of (Vm.id * float option)
  | VM_reboot of (Vm.id * float option)
  | VM_suspend of (Vm.id * data)
  | VM_resume of (Vm.id * data)
  | VM_restore_vifs of Vm.id
  | VM_restore_devices of (Vm.id * bool)
  | VM_migrate of vm_migrate_op
  | VM_receive_memory of vm_receive_op
  | VBD_hotplug of Vbd.id
  | VBD_hotunplug of Vbd.id * bool
  | VIF_hotplug of Vbd.id
  | VIF_hotunplug of Vbd.id * bool
  | VM_check_state of Vm.id
  | PCI_check_state of Pci.id
  | VBD_check_state of Vbd.id
  | VIF_check_state of Vif.id
  | VUSB_check_state of Vusb.id
  | Atomic of atomic
[@@deriving rpcty]

let string_of_operation x = x |> rpc_of operation |> Jsonrpc.to_string

let name_of_operation = function
  | VM_start _ ->
      "VM_start"
  | VM_poweroff _ ->
      "VM_poweroff"
  | VM_shutdown _ ->
      "VM_shutdown"
  | VM_reboot _ ->
      "VM_reboot"
  | VM_suspend _ ->
      "VM_suspend"
  | VM_resume _ ->
      "VM_resume"
  | VM_restore_vifs _ ->
      "VM_restore_vifs"
  | VM_restore_devices _ ->
      "VM_restore_devices"
  | VM_migrate _ ->
      "VM_migrate"
  | VM_receive_memory _ ->
      "VM_receive_memory"
  | VBD_hotplug _ ->
      "VBD_hotplug"
  | VBD_hotunplug _ ->
      "VBD_hotunplug"
  | VIF_hotplug _ ->
      "VIF_hotplug"
  | VIF_hotunplug _ ->
      "VIF_hotunplug"
  | VM_check_state _ ->
      "VM_check_state"
  | PCI_check_state _ ->
      "PCI_check_state"
  | VBD_check_state _ ->
      "VBD_check_state"
  | VIF_check_state _ ->
      "VIF_check_state"
  | VUSB_check_state _ ->
      "VUSB_check_state"
  | Atomic atomic ->
      Printf.sprintf "Atomic (%s)" (name_of_atomic atomic)

module TASK = struct
  open Xenops_task

  let cancel _ _dbg id = Xenops_task.cancel (handle_of_id tasks id)

  let stat' id = handle_of_id tasks id |> to_interface_task

  let signal id =
    try
      let handle = handle_of_id tasks id in
      let state = get_state handle in
      debug "TASK.signal %s = %s" id
        (state |> rpc_of Task.state |> Jsonrpc.to_string) ;
      Updates.add (Dynamic.Task id) updates
    with Xenopsd_error (Does_not_exist _) ->
      debug "TASK.signal %s (object deleted)" id

  let stat _ _dbg id = stat' id

  let destroy' id =
    destroy (handle_of_id tasks id) ;
    Updates.remove (Dynamic.Task id) updates

  let destroy _ _dbg id = destroy' id

  let list _ _dbg = list tasks |> List.map Xenops_task.to_interface_task

  let destroy_on_finish () _dbg id =
    id |> handle_of_id tasks |> destroy_on_finish
end

module VM_DB = struct
  include TypedTable (struct
    include Vm

    let namespace = "VM"

    type key = id

    let key id = [id; "config"]
  end)

  let ids () : Vm.id list = list []

  let list () =
    debug "VM.list" ;
    let vms = ids () |> List.map read |> List.filter_map (fun x -> x) in
    let module B = (val get_backend () : S) in
    let states = List.map B.VM.get_state vms in
    List.combine vms states

  let m = Mutex.create ()

  let signal id =
    debug "VM_DB.signal %s" id ;
    with_lock m (fun () ->
        if exists id then
          Updates.add (Dynamic.Vm id) updates
    )

  let remove id =
    with_lock m (fun () ->
        Updates.remove (Dynamic.Vm id) updates ;
        remove id
    )

  let add' x =
    debug "VM.add %s" (Jsonrpc.to_string (rpc_of Vm.t x)) ;
    write x.id x ;
    let module B = (val get_backend () : S) in
    B.VM.add x ; x.id
end

module PCI_DB = struct
  include TypedTable (struct
    include Pci

    let namespace = "PCI"

    type key = id

    let key k = [fst k; "pci." ^ snd k]
  end)

  let vm_of = fst

  let string_of_id (a, b) = a ^ "." ^ b

  let ids vm : Pci.id list =
    list [vm] |> filter_prefix "pci." |> List.map (fun id -> (vm, id))

  let pcis vm = ids vm |> List.map read |> dropnone

  let list vm =
    debug "PCI.list" ;
    let xs = pcis vm in
    let module B = (val get_backend () : S) in
    let states = List.map (B.PCI.get_state vm) xs in
    List.combine xs states

  let m = Mutex.create ()

  let signal id =
    debug "PCI_DB.signal %s" (string_of_id id) ;
    with_lock m (fun () ->
        if exists id then
          Updates.add (Dynamic.Pci id) updates
    )

  let remove id =
    with_lock m (fun () ->
        Updates.remove (Dynamic.Pci id) updates ;
        remove id
    )

  let add' x =
    let open Pci in
    debug "PCI.add %s %s" (string_of_id x.id)
      (Jsonrpc.to_string (rpc_of Pci.t x)) ;
    (* Only if the corresponding VM actually exists *)
    let vm = vm_of x.id in
    if not (VM_DB.exists vm) then (
      debug "VM %s not managed by me" vm ;
      raise (Xenopsd_error (Does_not_exist ("VM", vm)))
    ) ;
    write x.id x ;
    x.id

  let remove' id =
    debug "PCI.remove %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    if (B.PCI.get_state (vm_of id) (read_exn id)).Pci.plugged then
      raise (Xenopsd_error Device_is_connected)
    else
      remove id
end

module VBD_DB = struct
  include TypedTable (struct
    include Vbd

    let namespace = "VM"

    type key = id

    let key k = [fst k; "vbd." ^ snd k]
  end)

  let vm_of = fst

  let string_of_id (a, b) = a ^ "." ^ b

  let ids vm : Vbd.id list =
    list [vm] |> filter_prefix "vbd." |> List.map (fun id -> (vm, id))

  let vbds vm = ids vm |> List.map read |> dropnone

  let list vm =
    debug "VBD.list" ;
    let vbds' = vbds vm in
    let module B = (val get_backend () : S) in
    let states = List.map (B.VBD.get_state vm) vbds' in
    List.combine vbds' states

  let m = Mutex.create ()

  let signal id =
    debug "VBD_DB.signal %s" (string_of_id id) ;
    with_lock m (fun () ->
        if exists id then
          Updates.add (Dynamic.Vbd id) updates
    )

  let remove id =
    with_lock m (fun () ->
        Updates.remove (Dynamic.Vbd id) updates ;
        remove id
    )

  let add' x =
    debug "VBD.add %s %s" (string_of_id x.Vbd.id)
      (Jsonrpc.to_string (rpc_of Vbd.t x)) ;
    (* Only if the corresponding VM actually exists *)
    let vm = vm_of x.id in
    if not (VM_DB.exists vm || vm = !dom0_uuid) then (
      debug "VM %s not managed by me" vm ;
      raise (Xenopsd_error (Does_not_exist ("VM", vm)))
    ) ;
    write x.id x ;
    x.id

  let remove' id =
    debug "VBD.remove %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    if (B.VBD.get_state (vm_of id) (read_exn id)).Vbd.plugged then
      raise (Xenopsd_error Device_is_connected)
    else
      remove id
end

module VIF_DB = struct
  include TypedTable (struct
    include Vif

    let namespace = "VM"

    type key = id

    let key k = [fst k; "vif." ^ snd k]
  end)

  let vm_of = fst

  let string_of_id (a, b) = a ^ "." ^ b

  let ids vm : Vif.id list =
    list [vm] |> filter_prefix "vif." |> List.map (fun id -> (vm, id))

  let vifs vm = ids vm |> List.map read |> dropnone

  let list vm =
    let vifs' = vifs vm in
    let module B = (val get_backend () : S) in
    let states = List.map (B.VIF.get_state vm) vifs' in
    List.combine vifs' states

  let m = Mutex.create ()

  let signal id =
    debug "VIF_DB.signal %s" (string_of_id id) ;
    with_lock m (fun () -> Updates.add (Dynamic.Vif id) updates)

  let remove id =
    with_lock m (fun () ->
        Updates.remove (Dynamic.Vif id) updates ;
        remove id
    )

  let add' x =
    debug "VIF.add %s" (Jsonrpc.to_string (rpc_of Vif.t x)) ;
    (* Only if the corresponding VM actually exists *)
    let vm = vm_of x.id in
    if not (VM_DB.exists vm) then (
      debug "VM %s not managed by me" vm ;
      raise (Xenopsd_error (Does_not_exist ("VM", vm)))
    ) ;
    (* Generate MAC if necessary *)
    let mac =
      match x.mac with
      | "random" ->
          Mac.random_local_mac ()
      | "" ->
          Mac.hashchain_local_mac x.position (vm_of x.id)
      | mac ->
          mac
    in
    write x.id {x with mac} ;
    x.id

  let remove' id =
    debug "VIF.remove %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    if (B.VIF.get_state (vm_of id) (read_exn id)).Vif.plugged then
      raise (Xenopsd_error Device_is_connected)
    else
      remove id
end

module VGPU_DB = struct
  include TypedTable (struct
    include Vgpu

    let namespace = "VM"

    type key = id

    let key k = [fst k; "vgpu." ^ snd k]
  end)

  let vm_of = fst

  let string_of_id (a, b) = a ^ "." ^ b

  let id_of_string str =
    match Astring.String.cuts ~sep:"." str with
    | [a; b] ->
        (a, b)
    | _ ->
        internal_error "String cannot be interpreted as vgpu id: %s" str

  let ids vm : Vgpu.id list =
    list [vm] |> filter_prefix "vgpu." |> List.map (fun id -> (vm, id))

  let vgpus vm = ids vm |> List.map read |> dropnone

  let list vm =
    debug "VGPU.list" ;
    let xs = vgpus vm in
    let module B = (val get_backend () : S) in
    let states = List.map (B.VGPU.get_state vm) xs in
    List.combine xs states

  let m = Mutex.create ()

  let signal id =
    debug "VGPU_DB.signal %s" (string_of_id id) ;
    with_lock m (fun () ->
        if exists id then
          Updates.add (Dynamic.Vgpu id) updates
    )

  let remove id =
    with_lock m (fun () ->
        Updates.remove (Dynamic.Vgpu id) updates ;
        remove id
    )

  let add' x =
    debug "VGPU.add %s %s" (string_of_id x.Vgpu.id)
      (Jsonrpc.to_string (rpc_of Vgpu.t x)) ;
    (* Only if the corresponding VM actually exists *)
    let vm = vm_of x.id in
    if not (VM_DB.exists vm) then (
      debug "VM %s not managed by me" vm ;
      raise (Xenopsd_error (Does_not_exist ("VM", vm)))
    ) ;
    write x.id x ;
    x.id

  let remove' id =
    debug "VGPU.remove %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    if (B.VGPU.get_state (vm_of id) (read_exn id)).Vgpu.plugged then
      raise (Xenopsd_error Device_is_connected)
    else
      remove id
end

module VUSB_DB = struct
  include TypedTable (struct
    include Vusb

    let namespace = "VM"

    type key = id

    let key k = [fst k; "vusb." ^ snd k]
  end)

  let vm_of = fst

  let string_of_id (a, b) = a ^ "." ^ b

  let ids vm : Vusb.id list =
    list [vm] |> filter_prefix "vusb." |> List.map (fun id -> (vm, id))

  let vusbs vm = ids vm |> List.map read |> dropnone

  let list vm =
    debug "VUSB.list" ;
    let vusbs' = vusbs vm in
    let module B = (val get_backend () : S) in
    let states = List.map (B.VUSB.get_state vm) vusbs' in
    List.combine vusbs' states

  let m = Mutex.create ()

  let signal id =
    debug "VUSB_DB.signal %s" (string_of_id id) ;
    with_lock m (fun () -> Updates.add (Dynamic.Vusb id) updates)

  let remove id =
    with_lock m (fun () ->
        Updates.remove (Dynamic.Vusb id) updates ;
        remove id
    )

  let add' x =
    debug "VUSB.add %s %s" (string_of_id x.Vusb.id)
      (Jsonrpc.to_string (rpc_of Vusb.t x)) ;
    (* Only if the corresponding VM actually exists *)
    let vm = vm_of x.id in
    if not (VM_DB.exists vm) then (
      debug "VM %s not managed by me" vm ;
      raise (Xenopsd_error (Does_not_exist ("VM", vm)))
    ) ;
    write x.id x ;
    x.id

  let remove' id =
    debug "VUSB.remove %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    if (B.VUSB.get_state (vm_of id) (read_exn id)).Vusb.plugged then
      raise (Xenopsd_error Device_is_connected)
    else
      remove id
end

module StringMap = Map.Make (String)

let push_with_coalesce should_keep item queue =
  (* [filter_with_memory p xs] returns elements [x \in xs] where [p (x_i,
     [x_0...x_i-1])] *)
  let filter_with_memory p xs =
    List.fold_left (fun (acc, xs) x -> (xs :: acc, x :: xs)) ([], []) xs
    |> fst
    |> List.rev
    |> List.combine xs
    (* association list of (element, all previous elements) *)
    |> List.filter p
    |> List.map fst
  in
  let to_list queue = Queue.fold (fun xs x -> x :: xs) [] queue |> List.rev in
  let of_list xs =
    let q = Queue.create () in
    List.iter (fun x -> Queue.push x q) xs ;
    q
  in
  Queue.push item queue ;
  let queue' =
    to_list queue
    |> filter_with_memory (fun (this, prev) -> should_keep this prev)
    |> of_list
  in
  Queue.clear queue ;
  Queue.transfer queue' queue

module Queues = struct
  (** A set of queues where 'pop' operates on each queue in a round-robin
      fashion *)

  (** Each distinct 'tag' value creates a separate virtual queue *)
  type tag = string

  type 'a t = {
      mutable qs: 'a Queue.t StringMap.t
    ; mutable last_tag: string
    ; m: Mutex.t
    ; c: Condition.t
  }

  let create () =
    {
      qs= StringMap.empty
    ; last_tag= ""
    ; m= Mutex.create ()
    ; c= Condition.create ()
    }

  let get tag qs =
    with_lock qs.m (fun () ->
        if StringMap.mem tag qs.qs then
          StringMap.find tag qs.qs
        else
          Queue.create ()
    )

  let tags qs =
    with_lock qs.m (fun () -> StringMap.fold (fun x _ acc -> x :: acc) qs.qs [])

  let get_last_tag qs = with_lock qs.m (fun () -> qs.last_tag)

  let push_with_coalesce should_keep tag item qs =
    with_lock qs.m (fun () ->
        let q =
          if StringMap.mem tag qs.qs then
            StringMap.find tag qs.qs
          else
            Queue.create ()
        in
        push_with_coalesce should_keep item q ;
        qs.qs <- StringMap.add tag q qs.qs ;
        Condition.signal qs.c
    )

  let pop qs =
    with_lock qs.m (fun () ->
        while StringMap.is_empty qs.qs do
          Condition.wait qs.c qs.m
        done ;
        (* partition based on last_tag *)
        let before, after =
          StringMap.partition (fun x _ -> x <= qs.last_tag) qs.qs
        in
        (* the min_binding in the 'after' is the next queue *)
        let last_tag, q =
          StringMap.min_binding
            (if StringMap.is_empty after then before else after)
        in
        qs.last_tag <- last_tag ;
        let item = Queue.pop q in
        (* remove empty queues from the whole mapping *)
        qs.qs <-
          (if Queue.is_empty q then StringMap.remove last_tag qs.qs else qs.qs) ;
        (last_tag, item)
    )

  let transfer_tag tag a b =
    with_lock a.m (fun () ->
        with_lock b.m (fun () ->
            if StringMap.mem tag a.qs then (
              b.qs <- StringMap.add tag (StringMap.find tag a.qs) b.qs ;
              a.qs <- StringMap.remove tag a.qs ;
              Condition.signal b.c
            )
        )
    )
end

module Redirector = struct
  type t = {
      queues: (operation * Xenops_task.task_handle) Queues.t
    ; mutex: Mutex.t
  }

  (* When a thread is not actively processing a queue, items are placed here: *)
  let default = {queues= Queues.create (); mutex= Mutex.create ()}

  (* We create another queue only for Parallel atoms so as to avoid a situation
     where Parallel atoms can not progress because all the workers available for
     the default queue are used up by other operations depending on further
     Parallel atoms, creating a deadlock. *)
  let parallel_queues = {queues= Queues.create (); mutex= Mutex.create ()}

  (* When a thread is actively processing a queue, items are redirected to a
     thread-private queue *)
  let overrides = ref StringMap.empty

  let aliases = ref StringMap.empty

  let m = Mutex.create ()

  let should_keep (op, _) prev =
    match op with
    | VM_check_state _
    | PCI_check_state _
    | VBD_check_state _
    | VUSB_check_state _
    | VIF_check_state _ ->
        let prev' = List.map fst prev in
        not (List.mem op prev')
    | _ ->
        true

  let push t tag item =
    Debug.with_thread_associated "queue"
      (fun () ->
        with_lock m (fun () ->
            let real_tag, aliased =
              match StringMap.find_opt tag !aliases with
              | Some x ->
                  (x, true)
              | None ->
                  (tag, false)
            in
            let q, redirected =
              match StringMap.find_opt real_tag !overrides with
              | Some x ->
                  (x, true)
              | None ->
                  (t.queues, false)
            in
            debug "Queue.push %s onto %s%s:[ %s ]"
              (string_of_operation (fst item))
              ( if aliased then
                  "aliased "
                else if redirected then
                  "redirected "
                else
                  ""
              )
              real_tag
              (String.concat ", "
                 (List.rev
                    (Queue.fold
                       (fun acc (b, _) -> string_of_operation b :: acc)
                       [] (Queues.get tag q)
                    )
                 )
              ) ;
            Queues.push_with_coalesce should_keep real_tag item q
        )
      )
      ()

  let pop t () =
    (* We must prevent worker threads all calling Queues.pop before we've
       successfully put the redirection in place. Otherwise we end up with
       parallel threads operating on the same VM. *)
    with_lock t.mutex (fun () ->
        let tag, item = Queues.pop t.queues in
        with_lock m (fun () ->
            let q = Queues.create () in
            Queues.transfer_tag tag t.queues q ;
            overrides := StringMap.add tag q !overrides ;
            (* All items with [tag] will enter queue [q] *)
            (tag, q, item)
        )
    )

  let finished t tag queue =
    with_lock m (fun () ->
        Queues.transfer_tag tag queue t.queues ;
        overrides := StringMap.remove tag !overrides ;
        (* All items with [tag] will enter the queues queue *)
        (* Sanity check: there should be no override for tag in overrides *)
        aliases := StringMap.filter (fun _ v -> v <> tag) !aliases
    )

  let alias ~tag ~alias =
    with_lock m (fun () ->
        if StringMap.mem tag !overrides then (
          debug "Queue: Aliasing existing tag '%s' to new tag '%s'" tag alias ;
          aliases := StringMap.add alias tag !aliases
        ) else
          debug "Queue: Warning: Not aliasing non-existing tag"
    )

  module Dump = struct
    type q = {tag: string; items: operation list} [@@deriving rpcty]

    type t = q list [@@deriving rpcty]

    let make () =
      with_lock m (fun () ->
          let one queue =
            List.map
              (fun t ->
                {
                  tag= t
                ; items=
                    List.rev
                      (Queue.fold
                         (fun acc (b, _) -> b :: acc)
                         [] (Queues.get t queue)
                      )
                }
              )
              (Queues.tags queue)
          in
          List.concat
            (List.map one
               (default.queues
               :: parallel_queues.queues
               :: List.map snd (StringMap.bindings !overrides)
               )
            )
      )
  end
end

module Worker = struct
  type state =
    | Idle
    | Processing of (operation * Xenops_task.task_handle)
    | Shutdown_requested
    | Shutdown

  type t = {
      mutable state: state
    ; mutable shutdown_requested: bool
    ; m: Mutex.t
    ; c: Condition.t
    ; mutable t: Thread.t option
    ; redirector: Redirector.t
  }

  let get_state_locked t =
    if t.shutdown_requested then
      Shutdown_requested
    else
      t.state

  let get_state t = with_lock t.m (fun () -> get_state_locked t)

  let join t =
    with_lock t.m (fun () ->
        assert (t.state = Shutdown) ;
        Option.iter Thread.join t.t
    )

  let is_active t =
    with_lock t.m (fun () ->
        match get_state_locked t with
        | Idle | Processing (_, _) ->
            true
        | Shutdown_requested | Shutdown ->
            false
    )

  let shutdown t =
    with_lock t.m (fun () ->
        if not t.shutdown_requested then (
          t.shutdown_requested <- true ;
          true (* success *)
        ) else
          false
    )

  let restart t =
    with_lock t.m (fun () ->
        if t.shutdown_requested && t.state <> Shutdown then (
          t.shutdown_requested <- false ;
          true (* success *)
        ) else
          false
    )

  let create redirector =
    let t =
      {
        state= Idle
      ; shutdown_requested= false
      ; m= Mutex.create ()
      ; c= Condition.create ()
      ; t= None
      ; redirector
      }
    in
    let thread =
      Thread.create
        (fun () ->
          while
            not
              (with_lock t.m (fun () ->
                   if t.shutdown_requested then t.state <- Shutdown ;
                   t.shutdown_requested
               )
              )
          do
            with_lock t.m (fun () -> t.state <- Idle) ;
            let tag, queue, (op, item) = Redirector.pop redirector () in
            (* blocks here *)
            let id = Xenops_task.id_of_handle item in
            debug "Queue.pop returned %s" (string_of_operation op) ;
            with_lock t.m (fun () -> t.state <- Processing (op, item)) ;
            ( try
                let t' = Xenops_task.to_interface_task item in
                Debug.with_thread_associated t'.Task.dbg
                  (fun () ->
                    debug "Task %s reference %s: %s" id t'.Task.dbg
                      (string_of_operation op) ;
                    Xenops_task.run item
                  )
                  ()
              with e -> debug "Queue caught: %s" (Printexc.to_string e)
            ) ;
            Redirector.finished redirector tag queue ;
            (* The task must have succeeded or failed. *)
            ( match Xenops_task.get_state item with
            | Task.Pending _ ->
                error "Task %s has been left in a Pending state" id ;
                let e = Errors.Internal_error "Task left in Pending state" in
                let e = rpc_of Errors.error e in
                Xenops_task.set_state item (Task.Failed e)
            | _ ->
                ()
            ) ;
            TASK.signal id
          done
        )
        ()
    in
    t.t <- Some thread ;
    t
end

module WorkerPool = struct
  module Date = Xapi_stdext_date.Date

  (* Store references to Worker.ts here *)
  let pool = ref []

  let m = Mutex.create ()

  module Dump = struct
    type task = {
        id: string
      ; ctime: string
      ; dbg: string
      ; subtasks: (string * string) list
    }
    [@@deriving rpcty]

    let of_task t =
      let t' = Xenops_task.to_interface_task t in
      {
        id= t'.Task.id
      ; ctime= t'.Task.ctime |> Date.of_float |> Date.to_string
      ; dbg= t'.Task.dbg
      ; subtasks=
          List.map
            (fun (name, state) ->
              (name, state |> rpc_of Task.state |> Jsonrpc.to_string)
            )
            t'.Task.subtasks
          |> List.rev
      }

    type w = {state: string; task: task option} [@@deriving rpcty]

    type t = w list [@@deriving rpcty]

    let make () =
      with_lock m (fun () ->
          List.map
            (fun t ->
              match Worker.get_state t with
              | Worker.Idle ->
                  {state= "Idle"; task= None}
              | Worker.Processing (op, task) ->
                  {
                    state=
                      Printf.sprintf "Processing %s" (string_of_operation op)
                  ; task= Some (of_task task)
                  }
              | Worker.Shutdown_requested ->
                  {state= "Shutdown_requested"; task= None}
              | Worker.Shutdown ->
                  {state= "Shutdown"; task= None}
            )
            !pool
      )
  end

  (* Compute the number of active threads ie those which will continue to
     operate *)
  let count_active queues =
    with_lock m (fun () ->
        (* we do not want to use = when comparing queues: queues can contain
           (uncomparable) functions, and we are only interested in comparing the
           equality of their static references *)
        List.map
          (fun w -> w.Worker.redirector == queues && Worker.is_active w)
          !pool
        |> List.filter (fun x -> x)
        |> List.length
    )

  let find_one queues f =
    List.fold_left
      (fun acc x -> acc || (x.Worker.redirector == queues && f x))
      false

  (* Clean up any shutdown threads and remove them from the master list *)
  let gc queues pool =
    List.fold_left
      (fun acc w ->
        (* we do not want to use = when comparing queues: queues can contain
           (uncomparable) functions, and we are only interested in comparing the
           equality of their static references *)
        if w.Worker.redirector == queues && Worker.get_state w = Worker.Shutdown
        then (
          Worker.join w ; acc
        ) else
          w :: acc
      )
      [] pool

  let incr queues =
    debug "Adding a new worker to the thread pool" ;
    with_lock m (fun () ->
        pool := gc queues !pool ;
        if not (find_one queues Worker.restart !pool) then
          pool := Worker.create queues :: !pool
    )

  let decr queues =
    debug "Removing a worker from the thread pool" ;
    with_lock m (fun () ->
        pool := gc queues !pool ;
        if not (find_one queues Worker.shutdown !pool) then
          debug "There are no worker threads left to shutdown."
    )

  let start size =
    for _i = 1 to size do
      incr Redirector.default ;
      incr Redirector.parallel_queues
    done

  let set_size size =
    let inner queues =
      let active = count_active queues in
      debug "XXX active = %d" active ;
      for _i = 1 to max 0 (size - active) do
        incr queues
      done ;
      for _i = 1 to max 0 (active - size) do
        decr queues
      done
    in
    inner Redirector.default ;
    inner Redirector.parallel_queues
end

(* Keep track of which VMs we're rebooting so we avoid transient glitches where
   the power_state becomes Halted *)
let rebooting_vms = ref []

let rebooting_vms_m = Mutex.create ()

let rebooting id f =
  with_lock rebooting_vms_m (fun () -> rebooting_vms := id :: !rebooting_vms) ;
  finally f (fun () ->
      with_lock rebooting_vms_m (fun () ->
          rebooting_vms := List.filter (fun x -> x <> id) !rebooting_vms
      )
  )

let is_rebooting id =
  with_lock rebooting_vms_m (fun () -> List.mem id !rebooting_vms)

let export_metadata vdi_map vif_map vgpu_pci_map id =
  let module B = (val get_backend () : S) in
  let vm_t = VM_DB.read_exn id in
  debug "Remapping bootloader VDIs" ;
  (* Remap the bootloader vdis *)
  let vm_t =
    {
      vm_t with
      Vm.ty=
        ( match vm_t.Vm.ty with
        | Vm.HVM _ ->
            vm_t.Vm.ty
        | Vm.PV pv_info ->
            Vm.PV
              {
                pv_info with
                Vm.boot=
                  ( match pv_info.Vm.boot with
                  | Vm.Direct _ ->
                      pv_info.Vm.boot
                  | Vm.Indirect pv_indirect_boot ->
                      Vm.Indirect
                        {
                          pv_indirect_boot with
                          Vm.devices=
                            List.map (remap_vdi vdi_map)
                              pv_indirect_boot.Vm.devices
                        }
                  )
              }
        | Vm.PVinPVH pv_info ->
            Vm.PVinPVH
              {
                pv_info with
                Vm.boot=
                  ( match pv_info.Vm.boot with
                  | Vm.Direct _ ->
                      pv_info.Vm.boot
                  | Vm.Indirect pv_indirect_boot ->
                      Vm.Indirect
                        {
                          pv_indirect_boot with
                          Vm.devices=
                            List.map (remap_vdi vdi_map)
                              pv_indirect_boot.Vm.devices
                        }
                  )
              }
        | Vm.PVH pv_info ->
            Vm.PVH
              {
                pv_info with
                Vm.boot=
                  ( match pv_info.Vm.boot with
                  | Vm.Direct _ ->
                      pv_info.Vm.boot
                  | Vm.Indirect pv_indirect_boot ->
                      Vm.Indirect
                        {
                          pv_indirect_boot with
                          Vm.devices=
                            List.map (remap_vdi vdi_map)
                              pv_indirect_boot.Vm.devices
                        }
                  )
              }
        )
    }
  in
  let diff src dst =
    let pf =
      match (src.Vgpu.physical_pci_address, dst.Vgpu.physical_pci_address) with
      | src, dst when src <> dst ->
          [(src, dst)]
      | _ ->
          []
    in
    let vf =
      match (src.Vgpu.virtual_pci_address, dst.Vgpu.virtual_pci_address) with
      | Some src, Some dst when src <> dst ->
          [(src, dst)]
      | _ ->
          []
    in
    List.concat [pf; vf]
  in
  let pci_map vgpus_src vgpus_dst =
    List.map2 diff vgpus_src vgpus_dst |> List.concat
  in
  let remap_pci pci_map pci =
    let to_string = Pci.string_of_address in
    match List.assoc_opt pci.Pci.address pci_map with
    | Some addr ->
        (* rewrite addr and id (which is constructed from addr *)
        debug "Remapping PCI %s -> %s"
          (to_string pci.Pci.address)
          (to_string addr) ;
        let vm = fst pci.Pci.id in
        let ad =
          Printf.sprintf "%04x:%02x:%02x.%01x" addr.Pci.domain addr.Pci.bus
            addr.Pci.dev addr.Pci.fn
        in
        {pci with Pci.id= (vm, ad); Pci.address= addr}
    | None ->
        pci
  in
  let vbds = VBD_DB.vbds id in
  let vifs = List.map (fun vif -> remap_vif vif_map vif) (VIF_DB.vifs id) in
  (* we update PCI addrs in vgpus and then apply these updates to PCIs *)
  let vgpus' = VGPU_DB.vgpus id in
  let vgpus = List.map (remap_vgpu vgpu_pci_map) (VGPU_DB.vgpus id) in
  let pcis = List.map (remap_pci @@ pci_map vgpus' vgpus) (PCI_DB.pcis id) in
  let vusbs = VUSB_DB.vusbs id in
  let domains = B.VM.get_internal_state vdi_map vif_map vm_t in
  (* Remap VDIs *)
  debug "Remapping normal VDIs" ;
  let vbds =
    List.map
      (fun vbd ->
        {vbd with Vbd.backend= Option.map (remap_vdi vdi_map) vbd.Vbd.backend}
      )
      vbds
  in
  {Metadata.vm= vm_t; vbds; vifs; pcis; vgpus; vusbs; domains= Some domains}
  |> rpc_of Metadata.t
  |> Jsonrpc.to_string

let import_metadata id md =
  let module B = (val get_backend () : S) in
  let platformdata = md.Metadata.vm.Vm.platformdata in
  debug "Platformdata:featureset=%s"
    (try List.assoc "featureset" platformdata with Not_found -> "(absent)") ;
  let platformdata =
    (* If platformdata does not contain a featureset, then we are
       importing a VM that comes from a levelling-v1 host. In this case,
       give it a featureset that contains all features that this host has
       to offer. *)
    if not (List.mem_assoc "featureset" platformdata) then (
      let fs =
        let stat = B.HOST.stat () in
        match md.Metadata.vm.Vm.ty with
        | HVM _ | PVinPVH _ | PVH _ ->
            Host.(stat.cpu_info.features_hvm)
        | PV _ ->
            Host.(stat.cpu_info.features_pv)
      in
      let fs' = CPU_policy.to_string fs in
      debug "Setting Platformdata:featureset=%s" fs' ;
      ("featureset", fs') :: platformdata
    ) else
      platformdata
  in
  let vm = VM_DB.add' {md.Metadata.vm with platformdata} in
  let vbds =
    List.map
      (fun x ->
        (* If receiving an HVM migration from XS 6.2 or earlier, the hd*
           device names need to be upgraded to xvd*. *)
        let new_device_name =
          Device_number.upgrade_linux_device (snd x.Vbd.id)
        in
        {x with Vbd.id= (vm, new_device_name)}
      )
      md.Metadata.vbds
  in
  let vifs =
    List.map (fun x -> {x with Vif.id= (vm, snd x.Vif.id)}) md.Metadata.vifs
  in
  let pcis =
    List.map (fun x -> {x with Pci.id= (vm, snd x.Pci.id)}) md.Metadata.pcis
  in
  let vgpus =
    List.map (fun x -> {x with Vgpu.id= (vm, snd x.Vgpu.id)}) md.Metadata.vgpus
  in
  let vusbs =
    List.map (fun x -> {x with Vusb.id= (vm, snd x.Vusb.id)}) md.Metadata.vusbs
  in
  (* Remove any VBDs, VIFs, PCIs and VGPUs not passed in - they must have
     been destroyed in the higher level *)
  let gc old cur remove =
    let set_difference a b = List.filter (fun x -> not (List.mem x b)) a in
    let to_remove = set_difference old cur in
    List.iter remove to_remove
  in
  gc (VBD_DB.ids id) (List.map (fun x -> x.Vbd.id) vbds) VBD_DB.remove' ;
  gc (VIF_DB.ids id) (List.map (fun x -> x.Vif.id) vifs) VIF_DB.remove' ;
  gc (PCI_DB.ids id) (List.map (fun x -> x.Pci.id) pcis) PCI_DB.remove' ;
  gc (VGPU_DB.ids id) (List.map (fun x -> x.Vgpu.id) vgpus) VGPU_DB.remove' ;
  gc (VUSB_DB.ids id) (List.map (fun x -> x.Vusb.id) vusbs) VUSB_DB.remove' ;
  let (_ : Vbd.id list) = List.map VBD_DB.add' vbds in
  let (_ : Vif.id list) = List.map VIF_DB.add' vifs in
  let (_ : Pci.id list) = List.map PCI_DB.add' pcis in
  let (_ : Vgpu.id list) = List.map VGPU_DB.add' vgpus in
  let (_ : Vusb.id list) = List.map VUSB_DB.add' vusbs in
  md.Metadata.domains
  |> Option.iter (B.VM.set_internal_state (VM_DB.read_exn vm)) ;
  vm

(* This is a symptom of the ordering-sensitivity of the SM backend: it is not
   possible to upgrade RO -> RW or downgrade RW -> RO on the fly. One possible
   fix is to always attach RW and enforce read/only-ness at the VBD-level.
   However we would need to fix the LVHD "attach provisioning mode". *)
let vbd_plug_sets vbds =
  List.partition (fun vbd -> vbd.Vbd.mode = Vbd.ReadWrite) vbds

let vbd_plug_order vbds =
  (* return RW devices first since the storage layer can't upgrade a 'RO attach'
     into a 'RW attach' *)
  let rw, ro = vbd_plug_sets vbds in
  rw @ ro

let vif_plug_order vifs =
  List.sort (fun a b -> compare a.Vif.position b.Vif.position) vifs

let pci_plug_order pcis =
  List.sort (fun a b -> compare a.Pci.position b.Pci.position) pcis

let vgpu_plug_order vgpus =
  List.sort (fun a b -> compare a.Vgpu.position b.Vgpu.position) vgpus

type vgpu_fd_info = {vgpu_fd: Unix.file_descr; vgpu_channel: unit Event.channel}

let vgpu_receiver_sync : (Vm.id, vgpu_fd_info) Hashtbl.t = Hashtbl.create 10

let vgpu_receiver_sync_m = Mutex.create ()

type mem_fd_info = {mem_fd: Unix.file_descr; mem_channel: unit Event.channel}

let mem_receiver_sync : (Vm.id, mem_fd_info) Hashtbl.t = Hashtbl.create 10

let mem_receiver_sync_m = Mutex.create ()

(* The IOMMU_NO_SHAREPT flag is required for domains that use SRIOV Nvidia
   VGPUs. This predicate identifies the situation *)
let is_no_sharept vgpu =
  match (vgpu.Vgpu.virtual_pci_address, vgpu.Vgpu.implementation) with
  | Some _, Vgpu.Nvidia _ ->
      true
  | _ ->
      false

(* [is_nvidia_sriov vgpus pci] is true, if [pci] belongs to an SRIOV Nvidia vGPU
   in [vgpus] and false otherwise *)
let is_nvidia_sriov vgpus pci =
  let is_sriov = function
    | Vgpu.{implementation= Nvidia _; virtual_pci_address= Some addr; _} ->
        addr = pci.Pci.address
    | _ ->
        false
  in
  List.exists is_sriov vgpus

let is_not_nvidia_sriov vgpus pci = not (is_nvidia_sriov vgpus pci)

(* [must_dequarantine] identifies GPUs that need to be de-quarantined *)
let must_dequarantine vgpu =
  Xenops_interface.Vgpu.(
    match vgpu.implementation with
    | GVT_g _ ->
        true
    | Nvidia _ ->
        true
    | _ ->
        false
  )

(* compute a list of PCI_dequarantine operations for PCI devices *)
let dequarantine_ops vgpus =
  vgpus
  |> List.filter must_dequarantine
  |> List.map
       Xenops_interface.Vgpu.(
         fun vgpu -> PCI_dequarantine vgpu.physical_pci_address
       )

let rec atomics_of_operation = function
  | VM_start (id, force) ->
      let vbds_rw, vbds_ro = VBD_DB.vbds id |> vbd_plug_sets in
      let vifs = VIF_DB.vifs id |> vif_plug_order in
      let vgpus = VGPU_DB.vgpus id in
      let pcis = PCI_DB.pcis id |> pci_plug_order in
      let vusbs = VUSB_DB.vusbs id in
      let pcis_sriov, pcis_other =
        List.partition (is_nvidia_sriov vgpus) pcis
      in
      let no_sharept = List.exists is_no_sharept vgpus in
      [
        dequarantine_ops vgpus
      ; [
          VM_hook_script
            (id, Xenops_hooks.VM_pre_start, Xenops_hooks.reason__none)
        ; VM_create (id, None, None, no_sharept)
        ; VM_build (id, force)
        ]
      ; List.map
          (fun vbd -> VBD_set_active (vbd.Vbd.id, true))
          (vbds_rw @ vbds_ro)
        (* keeping behaviour of vbd_plug_order: rw vbds must be plugged before
           ro vbds, see vbd_plug_sets *)
      ; List.map
          (fun (ty, vbds) ->
            Parallel
              ( id
              , Printf.sprintf "VBD.epoch_begin %s vm=%s" ty id
              , List.filter_map
                  (fun vbd ->
                    Option.map
                      (fun x ->
                        VBD_epoch_begin (vbd.Vbd.id, x, vbd.Vbd.persistent)
                      )
                      vbd.Vbd.backend
                  )
                  vbds
              )
          )
          [("RW", vbds_rw); ("RO", vbds_ro)]
      ; [
          (* rw vbds must be plugged before ro vbds, see vbd_plug_sets *)
          Parallel
            ( id
            , Printf.sprintf "VBD.plug RW vm=%s" id
            , List.map (fun vbd -> VBD_plug vbd.Vbd.id) vbds_rw
            )
        ; Parallel
            ( id
            , Printf.sprintf "VBD.plug RO vm=%s" id
            , List.map (fun vbd -> VBD_plug vbd.Vbd.id) vbds_ro
            )
        ]
      ; List.map (fun vif -> VIF_set_active (vif.Vif.id, true)) vifs
      ; List.map (fun vif -> VIF_plug vif.Vif.id) vifs
      ; List.map (fun vgpu -> VGPU_set_active (vgpu.Vgpu.id, true)) vgpus
      ; List.map (fun pci -> PCI_plug (pci.Pci.id, false)) pcis_sriov
      ; [VM_create_device_model (id, false)]
        (* PCI and USB devices are hot-plugged into HVM guests via QEMU, so the
           following operations occur after creating the device models *)
      ; List.map (fun pci -> PCI_plug (pci.Pci.id, true)) pcis_other
      ; List.map (fun vusb -> VUSB_plug vusb.Vusb.id) vusbs
        (* At this point the domain is considered survivable. *)
      ; [VM_set_domain_action_request (id, None)]
      ]
      |> List.concat
  | VM_shutdown (id, timeout) ->
      let vbds = VBD_DB.vbds id in
      let vifs = VIF_DB.vifs id in
      let pcis = PCI_DB.pcis id in
      let vusbs = VUSB_DB.vusbs id in
      [
        Option.value ~default:[]
          (Option.map (fun x -> [VM_shutdown_domain (id, PowerOff, x)]) timeout)
        (* Before shutting down a VM, we need to unplug its VUSBs. *)
      ; List.map (fun vusb -> VUSB_unplug vusb.Vusb.id) vusbs
      ; [
          (* CA-315450: in a hard shutdown or snapshot revert, timeout=None and
             VM_shutdown_domain is not called. To avoid any interference, we
             pause the domain before destroying the device model. *)
          Best_effort (VM_pause id)
        ; VM_destroy_device_model id
        ; Parallel
            ( id
            , Printf.sprintf "VBD.unplug vm=%s" id
            , List.map (fun vbd -> VBD_unplug (vbd.Vbd.id, true)) vbds
            )
        ]
      ; List.map (fun vif -> VIF_unplug (vif.Vif.id, true)) vifs
      ; List.map (fun pci -> PCI_unplug pci.Pci.id) pcis
      ; [VM_destroy id]
      ]
      |> List.concat
  | VM_restore_vifs id ->
      let vifs = VIF_DB.vifs id in
      [
        List.map (fun vif -> VIF_set_active (vif.Vif.id, true)) vifs
      ; List.map (fun vif -> VIF_plug vif.Vif.id) vifs
      ]
      |> List.concat
  | VM_restore_devices (id, restore_vifs) ->
      let vbds_rw, vbds_ro = VBD_DB.vbds id |> vbd_plug_sets in
      let vgpus = VGPU_DB.vgpus id in
      let pcis = PCI_DB.pcis id |> pci_plug_order in
      let pcis_other = List.filter (is_not_nvidia_sriov vgpus) pcis in
      [
        List.map
          (fun vbd -> VBD_set_active (vbd.Vbd.id, true))
          (vbds_rw @ vbds_ro)
      ; [
          (* rw vbds must be plugged before ro vbds, see vbd_plug_sets *)
          Parallel
            ( id
            , Printf.sprintf "VBD.plug RW vm=%s" id
            , List.map (fun vbd -> VBD_plug vbd.Vbd.id) vbds_rw
            )
        ; Parallel
            ( id
            , Printf.sprintf "VBD.plug RO vm=%s" id
            , List.map (fun vbd -> VBD_plug vbd.Vbd.id) vbds_ro
            )
        ]
      ; (if restore_vifs then atomics_of_operation (VM_restore_vifs id) else [])
      ; List.map (fun vgpu -> VGPU_set_active (vgpu.Vgpu.id, true)) vgpus
        (* Nvidia SRIOV PCI devices have been already been plugged *)
      ; [
          VM_create_device_model (id, true)
          (* PCI and USB devices are hot-plugged into HVM guests via QEMU, so
             the following operations occur after creating the device models *)
        ]
      ; List.map (fun pci -> PCI_plug (pci.Pci.id, true)) pcis_other
      ]
      |> List.concat
  | VM_poweroff (id, timeout) ->
      let vbds = VBD_DB.vbds id in
      let vifs = VIF_DB.vifs id in
      let vgpus = VGPU_DB.vgpus id in
      let reason =
        if timeout = None then
          Xenops_hooks.reason__hard_shutdown
        else
          Xenops_hooks.reason__clean_shutdown
      in
      [
        [VM_hook_script (id, Xenops_hooks.VM_pre_destroy, reason)]
      ; atomics_of_operation (VM_shutdown (id, timeout))
      ; [
          Parallel
            ( id
            , Printf.sprintf "VBD.epoch_end vm=%s" id
            , List.filter_map
                (fun vbd ->
                  Option.map
                    (fun x -> VBD_epoch_end (vbd.Vbd.id, x))
                    vbd.Vbd.backend
                )
                vbds
            )
        ]
      ; List.map (fun vbd -> VBD_set_active (vbd.Vbd.id, false)) vbds
      ; List.map (fun vif -> VIF_set_active (vif.Vif.id, false)) vifs
      ; List.map (fun vgpu -> VGPU_set_active (vgpu.Vgpu.id, false)) vgpus
      ; [VM_hook_script (id, Xenops_hooks.VM_post_destroy, reason)]
      ]
      |> List.concat
  | VM_reboot (id, timeout) ->
      let vbds = VBD_DB.vbds id in
      let reason =
        if timeout = None then
          Xenops_hooks.reason__hard_reboot
        else
          Xenops_hooks.reason__clean_reboot
      in
      [
        Option.value ~default:[]
          (Option.map (fun x -> [VM_shutdown_domain (id, Reboot, x)]) timeout)
      ; [VM_hook_script (id, Xenops_hooks.VM_pre_destroy, reason)]
      ; atomics_of_operation (VM_shutdown (id, None))
      ; [
          Parallel
            ( id
            , Printf.sprintf "VBD.epoch_end vm=%s" id
            , List.filter_map
                (fun vbd ->
                  Option.map
                    (fun x -> VBD_epoch_end (vbd.Vbd.id, x))
                    vbd.Vbd.backend
                )
                vbds
            )
        ]
      ; [
          VM_hook_script (id, Xenops_hooks.VM_post_destroy, reason)
        ; VM_hook_script
            (id, Xenops_hooks.VM_pre_reboot, Xenops_hooks.reason__none)
        ]
      ; atomics_of_operation (VM_start (id, false))
      ; [VM_unpause id]
      ]
      |> List.concat
  | VM_suspend (id, data) ->
      (* If we've got a vGPU, then save its state to the same file *)
      let vgpu_data = if VGPU_DB.ids id = [] then None else Some data in
      [
        [
          VM_hook_script
            (id, Xenops_hooks.VM_pre_suspend, Xenops_hooks.reason__suspend)
        ; VM_save (id, [], data, vgpu_data)
        ; VM_hook_script
            (id, Xenops_hooks.VM_pre_destroy, Xenops_hooks.reason__suspend)
        ]
      ; atomics_of_operation (VM_shutdown (id, None))
      ; [
          VM_hook_script
            (id, Xenops_hooks.VM_post_destroy, Xenops_hooks.reason__suspend)
        ]
      ]
      |> List.concat
  | VM_resume (id, data) ->
      (* If we've got a vGPU, then save its state will be in the same file *)
      let vgpu_data = if VGPU_DB.ids id = [] then None else Some data in
      let pcis = PCI_DB.pcis id |> pci_plug_order in
      let vgpu_start_operations =
        match VGPU_DB.ids id with
        | [] ->
            []
        | vgpus ->
            let vgpus' = VGPU_DB.vgpus id in
            let pcis_sriov = List.filter (is_nvidia_sriov vgpus') pcis in
            List.concat
              [
                List.map (fun pci -> PCI_plug (pci.Pci.id, false)) pcis_sriov
              ; [VGPU_start (vgpus, true)]
              ]
      in
      let vgpus = VGPU_DB.vgpus id in
      let no_sharept = List.exists is_no_sharept vgpus in
      [
        dequarantine_ops vgpus
      ; [
          VM_create (id, None, None, no_sharept)
        ; VM_hook_script
            (id, Xenops_hooks.VM_pre_resume, Xenops_hooks.reason__none)
        ]
      ; vgpu_start_operations
      ; [VM_restore (id, data, vgpu_data)]
      ; atomics_of_operation (VM_restore_devices (id, true))
      ; [
          (* At this point the domain is considered survivable. *)
          VM_set_domain_action_request (id, None)
        ; VM_hook_script
            (id, Xenops_hooks.VM_post_resume, Xenops_hooks.reason__none)
        ]
      ]
      |> List.concat
  | VBD_hotplug id ->
      [VBD_set_active (id, true); VBD_plug id]
  | VBD_hotunplug (id, force) ->
      [VBD_unplug (id, force); VBD_set_active (id, false)]
  | VIF_hotplug id ->
      [VIF_set_active (id, true); VIF_plug id]
  | VIF_hotunplug (id, force) ->
      [VIF_unplug (id, force); VIF_set_active (id, false)]
  | _ ->
      []

let with_tracing ~name ~task f =
  let open Tracing in
  let parent = Xenops_task.tracing task in
  let tracer = get_tracer ~name in
  match Tracer.start ~tracer ~name ~parent () with
  | Ok span -> (
      Xenops_task.set_tracing task span ;
      try
        let result = f () in
        ignore @@ Tracer.finish span ;
        Xenops_task.set_tracing task parent ;
        result
      with exn ->
        let backtrace = Printexc.get_backtrace () in
        let error = (exn, backtrace) in
        ignore @@ Tracer.finish span ~error ;
        raise exn
    )
  | Error e ->
      warn "Failed to start tracing: %s" (Printexc.to_string e) ;
      f ()

let rec perform_atomic ~progress_callback ?subtask:_ ?result (op : atomic)
    (t : Xenops_task.task_handle) : unit =
  let module B = (val get_backend () : S) in
  with_tracing ~name:(name_of_atomic op) ~task:t @@ fun () ->
  Xenops_task.check_cancelling t ;
  match op with
  | Best_effort atom -> (
    try perform_atomic ~progress_callback ?result atom t
    with e ->
      debug "Ignoring error during best-effort operation: %s"
        (Printexc.to_string e)
  )
  | Parallel (_id, description, atoms) ->
      (* parallel_id is a unused unique name prefix for a parallel worker queue *)
      let parallel_id =
        Printf.sprintf "Parallel:task=%s.atoms=%d.(%s)"
          (Xenops_task.id_of_handle t)
          (List.length atoms) description
      in
      let with_tracing = parallel_id_with_tracing parallel_id t in
      debug "begin_%s" parallel_id ;
      let task_list =
        queue_atomics_and_wait ~progress_callback ~max_parallel_atoms:10
          with_tracing parallel_id atoms
      in
      debug "end_%s" parallel_id ;
      (* make sure that we destroy all the parallel tasks that finished *)
      let errors =
        List.map
          (fun (id, task_handle, task_state) ->
            match task_state with
            | Some (Task.Completed _) ->
                TASK.destroy' id ; None
            | Some (Task.Failed e) ->
                TASK.destroy' id ;
                let e =
                  match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty e with
                  | Ok x ->
                      Xenopsd_error x
                  | Error (`Msg x) ->
                      internal_error "Error unmarshalling failure: %s" x
                in
                Some e
            | None | Some (Task.Pending _) ->
                (* Because pending tasks are filtered out in
                   queue_atomics_and_wait with task_ended the second case will
                   never be encountered. The previous boolean used in
                   event_wait was enough to express the possible cases *)
                let err_msg =
                  Printf.sprintf "Timed out while waiting on task %s (%s)" id
                    (Xenops_task.get_dbg task_handle)
                in
                error "%s" err_msg ;
                Xenops_task.cancel task_handle ;
                Some (Xenopsd_error (Internal_error err_msg))
          )
          task_list
      in
      (* if any error was present, raise first one, so that
         trigger_cleanup_after_failure is called *)
      List.iter
        (fun err -> match err with None -> () | Some e -> raise e)
        errors
  | VIF_plug id ->
      debug "VIF.plug %s" (VIF_DB.string_of_id id) ;
      B.VIF.plug t (VIF_DB.vm_of id) (VIF_DB.read_exn id) ;
      VIF_DB.signal id
  | VIF_unplug (id, force) ->
      debug "VIF.unplug %s" (VIF_DB.string_of_id id) ;
      finally
        (fun () -> B.VIF.unplug t (VIF_DB.vm_of id) (VIF_DB.read_exn id) force)
        (fun () -> VIF_DB.signal id)
  | VIF_move (id, network) ->
      debug "VIF.move %s" (VIF_DB.string_of_id id) ;
      finally
        (fun () ->
          let vif = VIF_DB.read_exn id in
          (* Nb, this VIF_DB write needs to come before the call to move as the
             scripts will read from the disk! *)
          VIF_DB.write id {vif with Vif.backend= network} ;
          B.VIF.move t (VIF_DB.vm_of id) vif network
        )
        (fun () -> VIF_DB.signal id)
  | VIF_set_carrier (id, carrier) ->
      debug "VIF.set_carrier %s %b" (VIF_DB.string_of_id id) carrier ;
      finally
        (fun () ->
          let vif = VIF_DB.read_exn id in
          B.VIF.set_carrier t (VIF_DB.vm_of id) vif carrier ;
          VIF_DB.write id {vif with Vif.carrier}
        )
        (fun () -> VIF_DB.signal id)
  | VIF_set_locking_mode (id, mode) ->
      debug "VIF.set_locking_mode %s %s" (VIF_DB.string_of_id id)
        (mode |> rpc_of Vif.locking_mode |> Jsonrpc.to_string) ;
      finally
        (fun () ->
          let vif = VIF_DB.read_exn id in
          (* Nb, this VIF_DB write needs to come before the call to
             set_locking_mode as the scripts will read from the disk! *)
          VIF_DB.write id {vif with Vif.locking_mode= mode} ;
          B.VIF.set_locking_mode t (VIF_DB.vm_of id) vif mode
        )
        (fun () -> VIF_DB.signal id)
  | VIF_set_pvs_proxy (id, proxy) ->
      let s =
        match proxy with
        | None ->
            "(none)"
        | Some p ->
            p |> rpc_of Vif.PVS_proxy.t |> Jsonrpc.to_string
      in
      debug "VIF.set_pvs_proxy %s %s" (VIF_DB.string_of_id id) s ;
      finally
        (fun () ->
          let vif = VIF_DB.read_exn id in
          VIF_DB.write id {vif with Vif.pvs_proxy= proxy} ;
          B.VIF.set_pvs_proxy t (VIF_DB.vm_of id) vif proxy
        )
        (fun () -> VIF_DB.signal id)
  | VIF_set_ipv4_configuration (id, ipv4_configuration) ->
      let setting =
        match ipv4_configuration with
        | Vif.Unspecified4 ->
            ""
        | Vif.Static4 (address, gateway) -> (
          match gateway with
          | None ->
              Printf.sprintf "address:%s" (String.concat "; " address)
          | Some value ->
              Printf.sprintf "address:%s gateway:%s"
                (String.concat "; " address)
                value
        )
      in
      debug "VIF.set_ipv4_configuration %s %s" (VIF_DB.string_of_id id) setting ;
      finally
        (fun () ->
          let vif = VIF_DB.read_exn id in
          VIF_DB.write id {vif with Vif.ipv4_configuration} ;
          B.VIF.set_ipv4_configuration t (VIF_DB.vm_of id) vif
            ipv4_configuration
        )
        (fun () -> VIF_DB.signal id)
  | VIF_set_ipv6_configuration (id, ipv6_configuration) ->
      let setting =
        match ipv6_configuration with
        | Vif.Unspecified6 ->
            ""
        | Vif.Static6 (address6, gateway6) -> (
          match gateway6 with
          | None ->
              Printf.sprintf "address6:%s" (String.concat "; " address6)
          | Some value ->
              Printf.sprintf "address6:%s gateway6:%s"
                (String.concat "; " address6)
                value
        )
      in
      debug "VIF.set_ipv6_configuration %s %s" (VIF_DB.string_of_id id) setting ;
      finally
        (fun () ->
          let vif = VIF_DB.read_exn id in
          VIF_DB.write id {vif with Vif.ipv6_configuration} ;
          B.VIF.set_ipv6_configuration t (VIF_DB.vm_of id) vif
            ipv6_configuration
        )
        (fun () -> VIF_DB.signal id)
  | VIF_set_active (id, b) ->
      debug "VIF.set_active %s %b" (VIF_DB.string_of_id id) b ;
      B.VIF.set_active t (VIF_DB.vm_of id) (VIF_DB.read_exn id) b ;
      VIF_DB.signal id
  | VM_hook_script_stable (id, script, reason, backend_vm_id) ->
      let extra_args = B.VM.get_hook_args backend_vm_id in
      Xenops_hooks.vm ~script ~reason ~id ~extra_args
  | VM_hook_script (id, script, reason) ->
      let extra_args = B.VM.get_hook_args id in
      Xenops_hooks.vm ~script ~reason ~id ~extra_args
  | VBD_plug id ->
      debug "VBD.plug %s" (VBD_DB.string_of_id id) ;
      B.VBD.plug t (VBD_DB.vm_of id) (VBD_DB.read_exn id) ;
      VBD_DB.signal id
  | VBD_set_active (id, b) ->
      debug "VBD.set_active %s %b" (VBD_DB.string_of_id id) b ;
      B.VBD.set_active t (VBD_DB.vm_of id) (VBD_DB.read_exn id) b ;
      VBD_DB.signal id
  | VBD_epoch_begin (id, d, persistent) ->
      debug "VBD.epoch_begin %s" (d |> rpc_of disk |> Jsonrpc.to_string) ;
      B.VBD.epoch_begin t (VBD_DB.vm_of id) d persistent
  | VBD_epoch_end (id, d) ->
      debug "VBD.epoch_end %s" (d |> rpc_of disk |> Jsonrpc.to_string) ;
      B.VBD.epoch_end t (VBD_DB.vm_of id) d
  | VBD_set_qos id ->
      debug "VBD.set_qos %s" (VBD_DB.string_of_id id) ;
      B.VBD.set_qos t (VBD_DB.vm_of id) (VBD_DB.read_exn id) ;
      VBD_DB.signal id
  | VBD_unplug (id, force) ->
      debug "VBD.unplug %s" (VBD_DB.string_of_id id) ;
      finally
        (fun () -> B.VBD.unplug t (VBD_DB.vm_of id) (VBD_DB.read_exn id) force)
        (fun () -> VBD_DB.signal id)
  | VBD_insert (id, disk) -> (
      (* NB this is also used to "refresh" ie signal a qemu that it should
         re-open a device, useful for when a physical CDROM is inserted into the
         host. *)
      debug "VBD.insert %s" (VBD_DB.string_of_id id) ;
      let vbd_t = VBD_DB.read_exn id in
      let power = (B.VM.get_state (VM_DB.read_exn (fst id))).Vm.power_state in
      match power with
      | Running | Paused ->
          B.VBD.insert t (VBD_DB.vm_of id) vbd_t disk ;
          VBD_DB.signal id
      | _ ->
          raise (Xenopsd_error (Bad_power_state (power, Running)))
    )
  | VBD_eject id -> (
      debug "VBD.eject %s" (VBD_DB.string_of_id id) ;
      let vbd_t = VBD_DB.read_exn id in
      if vbd_t.Vbd.ty = Vbd.Disk then raise (Xenopsd_error Media_not_ejectable) ;
      let power = (B.VM.get_state (VM_DB.read_exn (fst id))).Vm.power_state in
      match power with
      | Running | Paused ->
          B.VBD.eject t (VBD_DB.vm_of id) vbd_t ;
          VBD_DB.signal id
      | _ ->
          raise (Xenopsd_error (Bad_power_state (power, Running)))
    )
  | VM_remove id -> (
      debug "VM.remove %s" id ;
      let vm_t = VM_DB.read_exn id in
      let power = (B.VM.get_state vm_t).Vm.power_state in
      match power with
      | Running | Paused ->
          raise (Xenopsd_error (Bad_power_state (power, Halted)))
      | Halted | Suspended ->
          B.VM.remove vm_t ;
          List.iter (fun vbd -> VBD_DB.remove vbd.Vbd.id) (VBD_DB.vbds id) ;
          List.iter (fun vif -> VIF_DB.remove vif.Vif.id) (VIF_DB.vifs id) ;
          List.iter (fun pci -> PCI_DB.remove pci.Pci.id) (PCI_DB.pcis id) ;
          List.iter (fun vgpu -> VGPU_DB.remove vgpu.Vgpu.id) (VGPU_DB.vgpus id) ;
          List.iter (fun vusb -> VUSB_DB.remove vusb.Vusb.id) (VUSB_DB.vusbs id) ;
          VM_DB.remove id
    )
  | VM_rename (id1, id2, when') ->
      if id1 = id2 then (
        error "VM.rename called with the same src/dest = %s" id1 ;
        failwith "rename called with the same src/dest"
      ) ;
      debug "VM.rename %s -> %s" id1 id2 ;
      (* TODO: Check that there are no items in the queue for id2 *)
      let vbds = VBD_DB.vbds id1 in
      let vifs = VIF_DB.vifs id1 in
      let pcis = PCI_DB.pcis id1 in
      let vgpus = VGPU_DB.vgpus id1 in
      let vusbs = VUSB_DB.vusbs id1 in
      B.VM.rename id1 id2 when' ;
      let new_key (_, y) = (id2, y) in
      let fixup items rm add get_id set_id =
        List.iter
          (fun i ->
            let id = get_id i in
            rm id ;
            let id' = new_key id in
            add id' (set_id id' i)
          )
          items
      in
      let vm = VM_DB.read_exn id1 in
      VM_DB.remove id1 ;
      VM_DB.add id2 {vm with Vm.id= id2} ;
      fixup vbds VBD_DB.remove VBD_DB.add
        (fun vbd -> vbd.Vbd.id)
        (fun id' vbd -> {vbd with Vbd.id= id'}) ;
      fixup vifs VIF_DB.remove VIF_DB.add
        (fun vif -> vif.Vif.id)
        (fun id' vif -> {vif with Vif.id= id'}) ;
      fixup pcis PCI_DB.remove PCI_DB.add
        (fun pci -> pci.Pci.id)
        (fun id' pci -> {pci with Pci.id= id'}) ;
      fixup vgpus VGPU_DB.remove VGPU_DB.add
        (fun vgpu -> vgpu.Vgpu.id)
        (fun id' vgpu -> {vgpu with Vgpu.id= id'}) ;
      fixup vusbs VUSB_DB.remove VUSB_DB.add
        (fun vusb -> vusb.Vusb.id)
        (fun id' vusb -> {vusb with Vusb.id= id'})
  | VM_import_metadata (id, md) ->
      debug "VM.import_metadata: overwriting VM metadata for VM: %s" id ;
      let _ = import_metadata id md in
      ()
  | PCI_plug (id, qmp_add_device) ->
      debug "PCI.plug %s" (PCI_DB.string_of_id id) ;
      let vm = PCI_DB.vm_of id in
      let pci = PCI_DB.read_exn id in
      B.PCI.plug t vm pci qmp_add_device ;
      PCI_DB.signal id
  | PCI_unplug id ->
      debug "PCI.unplug %s" (PCI_DB.string_of_id id) ;
      finally
        (fun () -> B.PCI.unplug t (PCI_DB.vm_of id) (PCI_DB.read_exn id))
        (fun () -> PCI_DB.signal id)
  | PCI_dequarantine addr ->
      debug "PCI.dequarantine %s" (Xenops_interface.Pci.string_of_address addr) ;
      B.PCI.(dequarantine addr)
  | VUSB_plug id ->
      debug "VUSB.plug %s" (VUSB_DB.string_of_id id) ;
      B.VUSB.plug t (VUSB_DB.vm_of id) (VUSB_DB.read_exn id) ;
      VUSB_DB.signal id
  | VUSB_unplug id ->
      debug "VUSB.unplug %s" (VUSB_DB.string_of_id id) ;
      finally
        (fun () -> B.VUSB.unplug t (VUSB_DB.vm_of id) (VUSB_DB.read_exn id))
        (fun () -> VUSB_DB.signal id)
  | VGPU_set_active (id, b) ->
      debug "VGPU set_active %s %b" (VGPU_DB.string_of_id id) b ;
      B.VGPU.set_active t (VGPU_DB.vm_of id) (VGPU_DB.read_exn id) b ;
      VGPU_DB.signal id
  | VGPU_start (ids, saved_state) ->
      debug "VGPU.start %s"
        (ids |> List.map VGPU_DB.string_of_id |> String.concat " ") ;
      let vgpus = List.map (fun id -> VGPU_DB.read_exn id) ids in
      B.VGPU.start t (VGPU_DB.vm_of (List.hd ids)) vgpus saved_state
  | VM_set_xsdata (id, xsdata) ->
      debug "VM.set_xsdata (%s, [ %s ])" id
        (String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) xsdata)) ;
      B.VM.set_xsdata t (VM_DB.read_exn id) xsdata
  | VM_set_vcpus (id, n) ->
      debug "VM.set_vcpus (%s, %d)" id n ;
      let vm_t = VM_DB.read_exn id in
      if n <= 0 || n > vm_t.Vm.vcpu_max then
        raise (Xenopsd_error (Invalid_vcpus vm_t.Vm.vcpu_max)) ;
      B.VM.set_vcpus t vm_t n
  | VM_set_shadow_multiplier (id, m) ->
      debug "VM.set_shadow_multiplier (%s, %.2f)" id m ;
      B.VM.set_shadow_multiplier t (VM_DB.read_exn id) m ;
      VM_DB.signal id
  | VM_set_memory_dynamic_range (id, min, max) ->
      debug "VM.set_memory_dynamic_range (%s, %Ld, %Ld)" id min max ;
      B.VM.set_memory_dynamic_range t (VM_DB.read_exn id) min max ;
      VM_DB.signal id
  | VM_pause id ->
      debug "VM.pause %s" id ;
      B.VM.pause t (VM_DB.read_exn id) ;
      VM_DB.signal id
  | VM_unpause id -> (
      debug "VM.unpause %s" id ;
      let vm_t = VM_DB.read_exn id in
      let power = (B.VM.get_state vm_t).Vm.power_state in
      match power with
      | Paused ->
          B.VM.unpause t vm_t ; VM_DB.signal id
      | _ ->
          info "VM %s is not paused" id
    )
  | VM_request_rdp (id, enabled) ->
      debug "VM.request_rdp %s %b" id enabled ;
      B.VM.request_rdp (VM_DB.read_exn id) enabled
  | VM_run_script (id, script) -> (
      debug "VM.run_script %s %s" id script ;
      let res = B.VM.run_script t (VM_DB.read_exn id) script in
      VM_DB.signal id ;
      match result with None -> () | Some r -> r := Some res
    )
  | VM_set_domain_action_request (id, dar) ->
      debug "VM.set_domain_action_request %s %s" id
        (Option.value ~default:"None"
           (Option.map
              (fun x -> x |> rpc_of domain_action_request |> Jsonrpc.to_string)
              dar
           )
        ) ;
      B.VM.set_domain_action_request (VM_DB.read_exn id) dar
  | VM_create_device_model (id, save_state) ->
      debug "VM.create_device_model %s" id ;
      let vbds : Vbd.t list = VBD_DB.vbds id in
      let vifs : Vif.t list = VIF_DB.vifs id in
      let vgpus : Vgpu.t list = VGPU_DB.vgpus id in
      let vusbs : Vusb.t list = VUSB_DB.vusbs id in
      B.VM.create_device_model t (VM_DB.read_exn id) vbds vifs vgpus vusbs
        save_state ;
      List.iter VGPU_DB.signal (VGPU_DB.ids id) ;
      List.iter VUSB_DB.signal (VUSB_DB.ids id)
  | VM_destroy_device_model id ->
      debug "VM.destroy_device_model %s" id ;
      B.VM.destroy_device_model t (VM_DB.read_exn id)
  | VM_destroy id ->
      debug "VM.destroy %s" id ;
      B.VM.destroy t (VM_DB.read_exn id)
  | VM_create (id, memory_upper_bound, final_id, no_sharept) ->
      debug "VM.create %s memory_upper_bound = %s" id
        (Option.value ~default:"None"
           (Option.map Int64.to_string memory_upper_bound)
        ) ;
      B.VM.create t memory_upper_bound (VM_DB.read_exn id) final_id no_sharept
  | VM_build (id, force) ->
      debug "VM.build %s" id ;
      let vbds : Vbd.t list = VBD_DB.vbds id |> vbd_plug_order in
      let vifs : Vif.t list = VIF_DB.vifs id |> vif_plug_order in
      let vgpus : Vgpu.t list = VGPU_DB.vgpus id |> vgpu_plug_order in
      let vusbs : Vusb.t list = VUSB_DB.vusbs id in
      let extras : string list =
        match PCI_DB.pcis id |> pci_plug_order with
        | [] ->
            []
        | pcis ->
            let sbdfs =
              List.map (fun p -> Pci.string_of_address p.Pci.address) pcis
            in
            ["-pci_passthrough"; String.concat "," sbdfs]
      in
      B.VM.build t (VM_DB.read_exn id) vbds vifs vgpus vusbs extras force
  | VM_shutdown_domain (id, reason, timeout) ->
      debug "VM.shutdown_domain %s, reason = %s, timeout = %f, ack timeout = %f"
        id
        (string_of_shutdown_request reason)
        timeout
        !domain_shutdown_ack_timeout ;
      let start = Unix.gettimeofday () in
      let vm = VM_DB.read_exn id in
      (* wait for a clean shutdown ack; this allows us to abort early. *)
      if
        not
          (B.VM.request_shutdown t vm reason
             (min !domain_shutdown_ack_timeout timeout)
          )
      then
        raise (Xenopsd_error Failed_to_acknowledge_shutdown_request) ;
      let remaining_timeout =
        max 0. (timeout -. (Unix.gettimeofday () -. start))
      in
      if not (B.VM.wait_shutdown t vm reason remaining_timeout) then
        raise (Xenopsd_error (Failed_to_shutdown (id, timeout)))
  | VM_s3suspend id ->
      debug "VM.s3suspend %s" id ;
      B.VM.s3suspend t (VM_DB.read_exn id) ;
      VM_DB.signal id
  | VM_s3resume id ->
      debug "VM.s3resume %s" id ;
      B.VM.s3resume t (VM_DB.read_exn id) ;
      VM_DB.signal id
  | VM_save (id, flags, data, vgpu_data) ->
      debug "VM.save %s" id ;
      let set_task_not_cancellable task =
        (* Prohibit cancelation of the task just before suspending the domain *)
        let task_id = Xenops_task.id_of_handle task in
        debug "VM %s will suspend, mark task %s to not cancellable." id task_id ;
        Xenops_task.prohibit_cancellation task ;
        TASK.signal task_id
      in
      B.VM.save t progress_callback (VM_DB.read_exn id) flags data vgpu_data
        set_task_not_cancellable
  | VM_restore (id, data, vgpu_data) ->
      debug "VM.restore %s" id ;
      if id |> VM_DB.exists |> not then
        failwith (Printf.sprintf "%s doesn't exist" id) ;
      let vbds : Vbd.t list = VBD_DB.vbds id in
      let vifs : Vif.t list = VIF_DB.vifs id in
      let extras = [] in
      B.VM.restore t progress_callback (VM_DB.read_exn id) vbds vifs data
        vgpu_data extras
  | VM_delay (id, t) ->
      debug "VM %s: waiting for %.2f before next VM action" id t ;
      Thread.delay t
  | VM_softreboot id ->
      debug "VM.soft_reset %s" id ;
      B.VM.soft_reset t (VM_DB.read_exn id)

and queue_atomic_int ~progress_callback dbg id op =
  let task =
    Xenops_task.add tasks dbg
      (let r = ref None in
       fun t ->
         perform_atomic ~progress_callback ~result:r op t ;
         !r
      )
  in
  Redirector.push Redirector.parallel_queues id (Atomic op, task) ;
  task

and queue_atomics_and_wait ~progress_callback ~max_parallel_atoms dbg id ops =
  let from = Updates.last_id dbg updates in
  Xenops_utils.chunks max_parallel_atoms ops
  |> List.mapi (fun chunk_idx ops ->
         debug "queue_atomics_and_wait: %s: chunk of %d atoms" dbg
           (List.length ops) ;
         let task_list =
           List.mapi
             (fun atom_idx op ->
               (* atom_id is a unique name for a parallel atom worker queue *)
               let atom_id =
                 Printf.sprintf "%s.chunk=%d.atom=%d" id chunk_idx atom_idx
               in
               queue_atomic_int ~progress_callback dbg atom_id op
             )
             ops
         in
         let timeout_start = Unix.gettimeofday () in
         List.map
           (fun task ->
             let task_id = Xenops_task.id_of_handle task in
             let completion =
               event_wait updates task ~from ~timeout_start 1200.0
                 (is_task task_id) task_ended
             in
             (task_id, task, completion)
           )
           task_list
     )
  |> List.concat

(* Used to divide up the progress (bar) amongst atomic operations *)
let weight_of_atomic = function
  | VM_save (_, _, _, _) ->
      10.
  | VM_restore (_, _, _) ->
      10.
  | _ ->
      1.

let progress_callback start len t y =
  let new_progress = start +. (y *. len) in
  let id = Xenops_task.id_of_handle t in
  Xenops_task.set_state t (Task.Pending new_progress) ;
  TASK.signal id

let perform_atomics atomics t =
  let total_weight =
    List.fold_left ( +. ) 0. (List.map weight_of_atomic atomics)
  in
  let (_ : float) =
    List.fold_left
      (fun progress x ->
        let weight = weight_of_atomic x in
        let progress_callback =
          progress_callback progress (weight /. total_weight) t
        in
        debug "Performing: %s" (string_of_atomic x) ;
        perform_atomic ~subtask:(string_of_atomic x) ~progress_callback x t ;
        progress_callback 1. ;
        progress +. (weight /. total_weight)
      )
      0. atomics
  in
  ()

let rec immediate_operation dbg _id op =
  let task = Xenops_task.add tasks dbg (fun t -> perform op t ; None) in
  let task_id = Xenops_task.id_of_handle task in
  TASK.destroy' task_id ;
  Debug.with_thread_associated dbg
    (fun () ->
      debug "Task %s reference %s: %s" task_id
        (Xenops_task.to_interface_task task).Task.dbg (string_of_operation op) ;
      Xenops_task.run task
    )
    () ;
  match Xenops_task.get_state task with
  | Task.Pending _ ->
      assert false
  | Task.Completed _ ->
      ()
  | Task.Failed e -> (
    match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty e with
    | Ok e ->
        raise (Xenopsd_error e)
    | Error (`Msg m) ->
        internal_error "Failed to unmarshal error: %s" m
  )

(* At all times we ensure that an operation which partially fails leaves the
   system in a recoverable state. All that should be necessary is to call the
   {VM,VBD,VIF,PCI}_check_state function. *)
and trigger_cleanup_after_failure op t =
  let dbg = (Xenops_task.to_interface_task t).Task.dbg in
  match op with
  | VM_check_state _
  | PCI_check_state _
  | VBD_check_state _
  | VUSB_check_state _
  | VIF_check_state _ ->
      () (* not state changing operations *)
  | VM_start (id, _)
  | VM_poweroff (id, _)
  | VM_reboot (id, _)
  | VM_shutdown (id, _)
  | VM_suspend (id, _)
  | VM_restore_vifs id
  | VM_restore_devices (id, _)
  | VM_resume (id, _) ->
      immediate_operation dbg id (VM_check_state id)
  | VM_receive_memory {vmr_id= id; vmr_final_id= final_id; _} ->
      immediate_operation dbg id (VM_check_state id) ;
      immediate_operation dbg final_id (VM_check_state final_id)
  | VM_migrate {vmm_id; vmm_tmp_src_id; _} ->
      immediate_operation dbg vmm_id (VM_check_state vmm_id) ;
      immediate_operation dbg vmm_tmp_src_id (VM_check_state vmm_tmp_src_id)
  | VBD_hotplug id | VBD_hotunplug (id, _) ->
      immediate_operation dbg (fst id) (VBD_check_state id)
  | VIF_hotplug id | VIF_hotunplug (id, _) ->
      immediate_operation dbg (fst id) (VIF_check_state id)
  | Atomic op ->
      trigger_cleanup_after_failure_atom op t

and trigger_cleanup_after_failure_atom op t =
  let dbg = (Xenops_task.to_interface_task t).Task.dbg in
  match op with
  | VBD_eject id
  | VBD_plug id
  | VBD_set_active (id, _)
  | VBD_epoch_begin (id, _, _)
  | VBD_epoch_end (id, _)
  | VBD_set_qos id
  | VBD_unplug (id, _)
  | VBD_insert (id, _) ->
      immediate_operation dbg (fst id) (VBD_check_state id)
  | VIF_plug id
  | VIF_set_active (id, _)
  | VIF_unplug (id, _)
  | VIF_move (id, _)
  | VIF_set_carrier (id, _)
  | VIF_set_locking_mode (id, _)
  | VIF_set_pvs_proxy (id, _)
  | VIF_set_ipv4_configuration (id, _)
  | VIF_set_ipv6_configuration (id, _) ->
      immediate_operation dbg (fst id) (VIF_check_state id)
  | PCI_plug (id, _) | PCI_unplug id ->
      immediate_operation dbg (fst id) (PCI_check_state id)
  | PCI_dequarantine _ ->
      ()
  | VUSB_plug id | VUSB_unplug id ->
      immediate_operation dbg (fst id) (VUSB_check_state id)
  | VGPU_set_active (id, _) ->
      immediate_operation dbg (fst id) (VM_check_state (VGPU_DB.vm_of id))
  | VGPU_start (ids, _) ->
      let id = List.hd ids in
      immediate_operation dbg (fst id) (VM_check_state (VGPU_DB.vm_of id))
  | VM_hook_script_stable (id, _, _, _)
  | VM_hook_script (id, _, _)
  | VM_remove id
  | VM_set_xsdata (id, _)
  | VM_set_vcpus (id, _)
  | VM_set_shadow_multiplier (id, _)
  | VM_set_memory_dynamic_range (id, _, _)
  | VM_pause id
  | VM_unpause id
  | VM_request_rdp (id, _)
  | VM_run_script (id, _)
  | VM_set_domain_action_request (id, _)
  | VM_create_device_model (id, _)
  | VM_destroy_device_model id
  | VM_destroy id
  | VM_create (id, _, _, _)
  | VM_build (id, _)
  | VM_shutdown_domain (id, _, _)
  | VM_s3suspend id
  | VM_s3resume id
  | VM_save (id, _, _, _)
  | VM_restore (id, _, _)
  | VM_delay (id, _)
  | VM_softreboot id ->
      immediate_operation dbg id (VM_check_state id)
  | Best_effort op ->
      trigger_cleanup_after_failure_atom op t
  | Parallel (_id, _description, ops) ->
      List.iter (fun op -> trigger_cleanup_after_failure_atom op t) ops
  | VM_rename (id1, id2, _) ->
      immediate_operation dbg id1 (VM_check_state id1) ;
      immediate_operation dbg id2 (VM_check_state id2)
  | VM_import_metadata _ ->
      ()

and perform_exn ?subtask ?result (op : operation) (t : Xenops_task.task_handle)
    : unit =
  let module B = (val get_backend () : S) in
  with_tracing ~name:(name_of_operation op) ~task:t @@ fun () ->
  match op with
  | VM_start (id, force) -> (
      debug "VM.start %s (force=%b)" id force ;
      let power = (B.VM.get_state (VM_DB.read_exn id)).Vm.power_state in
      match power with
      | Running ->
          info "VM %s is already running" id
      | _ ->
          perform_atomics (atomics_of_operation op) t ;
          VM_DB.signal id
    )
  | VM_poweroff (id, _timeout) ->
      debug "VM.poweroff %s" id ;
      perform_atomics (atomics_of_operation op) t ;
      VM_DB.signal id
  | VM_reboot (id, _timeout) ->
      debug "VM.reboot %s" id ;
      rebooting id (fun () -> perform_atomics (atomics_of_operation op) t) ;
      VM_DB.signal id
  | VM_shutdown (id, _timeout) ->
      debug "VM.shutdown %s" id ;
      perform_atomics (atomics_of_operation op) t ;
      VM_DB.signal id
  | VM_suspend (id, _data) ->
      debug "VM.suspend %s" id ;
      perform_atomics (atomics_of_operation op) t ;
      VM_DB.signal id
  | VM_restore_vifs id ->
      debug "VM_restore_vifs %s" id ;
      perform_atomics (atomics_of_operation op) t
  | VM_restore_devices (id, restore_vifs) ->
      (* XXX: this is delayed due to the 'attach'/'activate' behaviour *)
      debug "VM_restore_devices %s %b" id restore_vifs ;
      perform_atomics (atomics_of_operation op) t
  | VM_resume (id, _data) ->
      debug "VM.resume %s" id ;
      perform_atomics (atomics_of_operation op) t ;
      VM_DB.signal id
  | VBD_hotplug id ->
      debug "VBD_hotplug %s.%s" (fst id) (snd id) ;
      perform_atomics (atomics_of_operation op) t
  | VBD_hotunplug (id, force) ->
      debug "VBD_hotplug %s.%s %b" (fst id) (snd id) force ;
      perform_atomics (atomics_of_operation op) t
  | VIF_hotplug id ->
      debug "VIF_hotplug %s.%s" (fst id) (snd id) ;
      perform_atomics (atomics_of_operation op) t
  | VIF_hotunplug (id, force) ->
      debug "VIF_hotplug %s.%s %b" (fst id) (snd id) force ;
      perform_atomics (atomics_of_operation op) t
  | VM_migrate vmm ->
      debug "VM.migrate %s -> %s" vmm.vmm_id vmm.vmm_url ;
      let id = vmm.vmm_id in
      let new_src_id = vmm.vmm_tmp_src_id in
      let new_dest_id = vmm.vmm_tmp_dest_id in
      let vm = VM_DB.read_exn id in
      let dbg = (Xenops_task.to_interface_task t).Task.dbg in
      let url = Uri.of_string vmm.vmm_url in
      let compress_memory = vmm.vmm_compress in
      let compress =
        match compress_memory with
        | true ->
            Zstd.Fast.compress
        | false ->
            fun fd fn -> fn fd
        (* do nothing *)
      in
      let compress_vgpu vgpu_fd f =
        match vgpu_fd with
        | Some (FD fd) when compress_memory ->
            compress fd (fun fd -> f (Some (FD fd)))
        | vgpu_fd ->
            f vgpu_fd
      in
      debug "%s compress memory: %b" __FUNCTION__ compress_memory ;
      let verify_cert =
        (* Stunnel_client.pool (which xapi normally uses) is not right here,
           because xenopsd does not get notified if certificate checking is
           turned on or off in xapi. Xapi takes the global on/off switch into
           account when setting `verify_dest`. *)
        if vmm.vmm_verify_dest then Some Stunnel.pool else None
      in
      (* We need to perform version exchange here *)
      let module B = (val get_backend () : S) in
      B.VM.assert_can_save vm ;
      let extra_args = B.VM.get_hook_args id in
      Xenops_hooks.vm ~script:Xenops_hooks.VM_pre_migrate
        ~reason:Xenops_hooks.reason__migrate_source ~id ~extra_args ;
      let module Remote = Xenops_interface.XenopsAPI (Idl.Exn.GenClient (struct
        let rpc =
          Xcp_client.xml_http_rpc ~srcstr:"xenops" ~dststr:"dst_xenops"
            ~verify_cert (fun () -> vmm.vmm_url
          )
      end)) in
      let regexp = Re.Pcre.regexp id in
      debug "Destination domain will be built with uuid=%s" new_dest_id ;
      debug "Original domain will be moved to uuid=%s" new_src_id ;
      (* Redirect operations on new_src_id to our worker thread. *)
      (* This is the id our domain will have when we've streamed its memory *)
      (* image to the destination. *)
      Redirector.alias ~tag:id ~alias:new_src_id ;
      let id' =
        let dbg = dbg_with_traceparent_of_task t in
        Remote.VM.import_metadata dbg
          (Re.replace_string regexp ~by:new_dest_id
             (export_metadata vmm.vmm_vdi_map vmm.vmm_vif_map
                vmm.vmm_vgpu_pci_map id
             )
          )
      in
      debug "Received vm-id = %s" id' ;
      let make_url snippet id_str =
        Uri.make ?scheme:(Uri.scheme url) ?host:(Uri.host url)
          ?port:(Uri.port url)
          ~path:(Uri.path url ^ snippet ^ id_str)
          ~query:(Uri.query url) ()
      in
      (* CA-78365: set the memory dynamic range to a single value to stop
         ballooning. *)
      let atomic =
        VM_set_memory_dynamic_range
          (id, vm.Vm.memory_dynamic_min, vm.Vm.memory_dynamic_min)
      in
      let (_ : unit) =
        perform_atomic ~subtask:(string_of_atomic atomic)
          ~progress_callback:(fun _ -> ())
          atomic t
      in
      (* Waiting here is not essential but adds a degree of safety and
         reducess unnecessary memory copying. *)
      ( try B.VM.wait_ballooning t vm
        with Xenopsd_error Ballooning_timeout_before_migration -> ()
      ) ;
      (* Find out the VM's current memory_limit: this will be used to allocate
         memory on the receiver *)
      let state = B.VM.get_state vm in
      info "VM %s has memory_limit = %Ld" id state.Vm.memory_limit ;
      let url = make_url "/migrate/vm/" new_dest_id in
      let https = Uri.scheme url = Some "https" in
      Open_uri.with_open_uri ~verify_cert url (fun vm_fd ->
          let module Handshake = Xenops_migrate.Handshake in
          let do_request fd extra_cookies url =
            if not https then
              Sockopt.set_sock_keepalives fd ;
            let module Request =
              Cohttp.Request.Make (Cohttp_posix_io.Unbuffered_IO) in
            let cookies =
              List.concat
                [
                  [
                    ("instance_id", instance_id)
                  ; ("final_id", id)
                  ; ("dbg", dbg)
                  ; (cookie_mem_migration, cookie_mem_migration_value)
                  ]
                ; ( if compress_memory then
                      [(cookie_mem_compression, cookie_mem_compression_value)]
                    else
                      []
                  )
                ; extra_cookies
                ]
            in
            let headers =
              Cohttp.Header.of_list
                ([
                   Cohttp.Cookie.Cookie_hdr.serialize cookies
                 ; ("Connection", "keep-alive")
                 ; ("User-agent", "xenopsd")
                 ]
                @ traceparent_header_of_task t
                )
            in
            let request =
              Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers url
            in
            Request.write (fun _ -> ()) request fd
          in
          do_request vm_fd
            [("memory_limit", Int64.to_string state.Vm.memory_limit)]
            url ;
          let first_handshake () =
            ( match Handshake.recv vm_fd with
            | Handshake.Success ->
                ()
            | Handshake.Error msg -> (
                error "cannot transmit vm to host: %s" msg ;
                match
                  Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty
                    (Jsonrpc.of_string msg)
                with
                | Ok e ->
                    raise (Xenopsd_error e)
                | Error _ ->
                    raise (Xenopsd_error (Internal_error msg))
                | exception _ ->
                    raise (Xenopsd_error (Internal_error msg))
              )
            ) ;
            debug "VM.migrate: Synchronisation point 1"
          in
          let final_handshake () =
            Handshake.send vm_fd Handshake.Success ;
            debug "VM.migrate: Synchronisation point 3" ;
            match Handshake.recv vm_fd with
            | Success ->
                debug "VM.migrate: Synchronisation point 4"
            | Error msg ->
                (* at this point, the VM has already been transferred to
                   the destination host. even though the destination host
                   failed to respond successfully to our handshake, the VM
                   should still be running correctly *)
                internal_error
                  "VM.migrate: Failed during Synchronisation point 4. msg: %s"
                  msg
          in
          let save ?vgpu_fd () =
            let url = make_url "/migrate/mem/" new_dest_id in
            Open_uri.with_open_uri ~verify_cert url (fun mem_fd ->
                (* vm_fd: signaling channel, mem_fd: memory stream *)
                do_request mem_fd [] url ;
                Handshake.recv_success mem_fd ;
                debug "VM.migrate: Synchronisation point 1-mem" ;
                Handshake.send vm_fd Handshake.Success ;
                debug "VM.migrate: Synchronisation point 1-mem ACK" ;

                compress mem_fd @@ fun mem_fd ->
                compress_vgpu vgpu_fd @@ fun vgpu_fd ->
                perform_atomics
                  [
                    VM_save (id, [Live], FD mem_fd, vgpu_fd)
                  ; VM_rename (id, new_src_id, Pre_migration)
                  ]
                  t ;
                debug "VM.migrate: Synchronisation point 2"
            )
          in
          (* If we have a vGPU, kick off its migration process before starting
             the main VM migration sequence. *)
          match VGPU_DB.ids id with
          | [] ->
              first_handshake () ; save () ; final_handshake ()
          | (_vm_id, dev_id) :: _ ->
              let url =
                make_url "/migrate/vgpu/"
                  (VGPU_DB.string_of_id (new_dest_id, dev_id))
              in
              Open_uri.with_open_uri ~verify_cert url (fun vgpu_fd ->
                  if not https then
                    Sockopt.set_sock_keepalives vgpu_fd ;
                  do_request vgpu_fd [(cookie_vgpu_migration, "")] url ;
                  Handshake.recv_success vgpu_fd ;
                  debug "VM.migrate: Synchronisation point 1-vgpu" ;
                  Handshake.send vm_fd Handshake.Success ;
                  debug "VM.migrate: Synchronisation point 1-vgpu ACK" ;
                  first_handshake () ;
                  save ~vgpu_fd:(FD vgpu_fd) ()
              ) ;
              final_handshake ()
      ) ;
      (* cleanup tmp src VM *)
      let atomics =
        [
          VM_hook_script_stable
            ( id
            , Xenops_hooks.VM_pre_destroy
            , Xenops_hooks.reason__suspend
            , new_src_id
            )
        ]
        @ atomics_of_operation (VM_shutdown (new_src_id, None))
        @ [
            VM_hook_script_stable
              ( id
              , Xenops_hooks.VM_post_destroy
              , Xenops_hooks.reason__suspend
              , new_src_id
              )
          ; VM_remove new_src_id
          ]
      in
      perform_atomics atomics t
  | VM_receive_memory
      {
        vmr_id= id
      ; vmr_final_id= final_id
      ; vmr_memory_limit= memory_limit
      ; vmr_socket= s
      ; vmr_handshake= handshake
      ; vmr_compressed
      } -> (
      let decompress =
        match vmr_compressed with
        | true ->
            Zstd.Fast.decompress_passive
        | false ->
            fun fd fn -> fn fd
      in
      let decompress_vgpu vgpu_info f =
        match vgpu_info with
        | Some info when vmr_compressed ->
            decompress info.vgpu_fd (fun fd -> f (Some (FD fd)))
        | Some info ->
            f (Some (FD info.vgpu_fd))
        | None ->
            f None
      in

      if final_id <> id then
        (* Note: In the localhost case, there are necessarily two worker
           threads operating on the same VM. The first one is using tag
           xxxxxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxx and has tag
           xxxxxxxxxxxx-xxxx-xxxx-xxxx-00000000 aliased to it (so actions
           being queued against either will get queued on that worker thread).
           The second worker thread has just started up at this point and has
           tag xxxxxxxxxxxx-xxxx-xxxx-xxxx-00000001. The following line will
           add a new alias of the original id to this second worker thread so
           we end up with a situation where there are two worker threads that
           can conceivably queue actions related to the original uuid
           xxxxxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxx. However, the alias is always
           resolved first and hence in practice any further actions related to
           the real uuid will be queued up on this worker thread's queue. *)
        Redirector.alias ~tag:id ~alias:final_id ;
      debug "VM.receive_memory: %s (compressed=%b)" id vmr_compressed ;
      Sockopt.set_sock_keepalives s ;
      let open Xenops_migrate in
      (* set up the destination domain *)
      debug "VM.receive_memory: creating domain and restoring VIFs" ;
      finally
        (fun () ->
          (* If we have a vGPU, wait for the vgpu-1 ACK, which indicates that
             the vgpu_receiver_sync entry for this vm id has already been
             initialised by the parallel receive_vgpu thread in this receiving
             host *)
          ( match VGPU_DB.ids id with
          | [] ->
              ()
          | _ ->
              Handshake.recv_success s ;
              debug "VM.receive_memory: Synchronisation point 1-vgpu ACK"
              (* After this point, vgpu_receiver_sync is initialised by the
                 corresponding receive_vgpu thread and therefore can be used
                 by this VM_receive_memory thread *)
          ) ;
          ( try
              let no_sharept = VGPU_DB.vgpus id |> List.exists is_no_sharept in
              debug "VM %s no_sharept=%b (%s)" id no_sharept __LOC__ ;
              perform_atomics
                ([VM_create (id, Some memory_limit, Some final_id, no_sharept)]
                @ (* Perform as many operations as possible on the destination
                     domain before pausing the original domain *)
                atomics_of_operation (VM_restore_vifs id)
                )
                t ;
              Handshake.send s Handshake.Success
            with e ->
              Backtrace.is_important e ;
              let msg =
                match e with
                | Xenopsd_error error ->
                    Rpcmarshal.marshal Errors.error.Rpc.Types.ty error
                    |> Jsonrpc.to_string
                | _ ->
                    Printexc.to_string e
              in
              Handshake.send s (Handshake.Error msg) ;
              raise e
          ) ;
          debug "VM.receive_memory: Synchronisation point 1" ;
          debug "VM.receive_memory: restoring VM" ;
          try
            (* Check if there is a separate vGPU data channel *)
            let vgpu_info =
              with_lock vgpu_receiver_sync_m (fun () ->
                  Hashtbl.find_opt vgpu_receiver_sync id
              )
            in
            let pcis = PCI_DB.pcis id |> pci_plug_order in
            let receive_mem_fd id s =
              debug "VM.receive_memory: using new handshake protocol for VM %s"
                id ;
              Handshake.recv_success s ;
              debug "VM.receive_memory: Synchronisation point 1-mem ACK" ;
              with_lock mem_receiver_sync_m @@ fun () ->
              match Hashtbl.find_opt mem_receiver_sync id with
              | Some fd ->
                  fd.mem_fd
              | None ->
                  error
                    "VM.receive_memory: Failed to receive FD for VM memory \
                     (id=%s)"
                    id ;
                  failwith __FUNCTION__
            in
            let vgpu_start_operations () =
              match VGPU_DB.ids id with
              | [] ->
                  []
              | vgpus ->
                  let vgpus' = VGPU_DB.vgpus id in
                  let pcis_sriov = List.filter (is_nvidia_sriov vgpus') pcis in
                  List.concat
                    [
                      dequarantine_ops vgpus'
                    ; List.map
                        (fun pci -> PCI_plug (pci.Pci.id, false))
                        pcis_sriov
                    ; [VGPU_start (vgpus, true)]
                    ]
            in
            let mem_fd =
              match handshake with
              | Some _ ->
                  receive_mem_fd id s (* new handshake protocol *)
              | None ->
                  s
              (* receiving memory on this connection *)
            in
            Sockopt.set_sock_keepalives mem_fd ;
            decompress mem_fd @@ fun mem_fd ->
            decompress_vgpu vgpu_info @@ fun vgpu_info ->
            perform_atomics
              (List.concat
                 [
                   vgpu_start_operations ()
                 ; [VM_restore (id, FD mem_fd, vgpu_info)]
                 ]
              )
              t ;
            debug "VM.receive_memory: restore complete"
          with e ->
            Backtrace.is_important e ;
            Debug.log_backtrace e (Backtrace.get e) ;
            debug "Caught %s during VM_restore: cleaning up VM state"
              (Printexc.to_string e) ;
            perform_atomics [VM_destroy id; VM_remove id] t
        )
        (fun () ->
          (* Inform the vgpu and mem handler threads we are done *)
          let vgpu_info =
            with_lock vgpu_receiver_sync_m (fun () ->
                Hashtbl.find_opt vgpu_receiver_sync id
            )
          in
          let mem_info =
            with_lock mem_receiver_sync_m (fun () ->
                Hashtbl.find_opt mem_receiver_sync id
            )
          in
          Option.iter
            (fun x -> Event.send x.vgpu_channel () |> Event.sync)
            vgpu_info ;
          Option.iter
            (fun x -> Event.send x.mem_channel () |> Event.sync)
            mem_info
        ) ;
      debug "VM.receive_memory: Synchronisation point 2" ;
      try
        (* Receive the all-clear to unpause *)
        Handshake.recv_success s ;
        debug "VM.receive_memory: Synchronisation point 3" ;
        if final_id <> id then (
          debug "VM.receive_memory: Renaming domain" ;
          perform_atomics [VM_rename (id, final_id, Post_migration)] t
        ) ;
        debug "VM.receive_memory: restoring remaining devices and unpausing" ;
        perform_atomics
          (atomics_of_operation (VM_restore_devices (final_id, false))
          @ [
              VM_unpause final_id
            ; VM_set_domain_action_request (final_id, None)
            ; VM_hook_script
                ( final_id
                , Xenops_hooks.VM_post_migrate
                , Xenops_hooks.reason__migrate_dest
                )
            ]
          )
          t ;
        Handshake.send s Handshake.Success ;
        debug "VM.receive_memory: Synchronisation point 4"
      with e ->
        finally
          (fun () ->
            Backtrace.is_important e ;
            Debug.log_backtrace e (Backtrace.get e) ;
            debug "Caught %s: cleaning up VM state" (Printexc.to_string e) ;
            perform_atomics
              (atomics_of_operation (VM_shutdown (id, None)) @ [VM_remove id])
              t
          )
          (fun () -> Handshake.send s (Handshake.Error (Printexc.to_string e)))
    )
  | VM_check_state id ->
      let vm = VM_DB.read_exn id in
      let state = B.VM.get_state vm in
      let run_time = Unix.gettimeofday () -. state.Vm.last_start_time in
      let actions =
        match B.VM.get_domain_action_request vm with
        | Some Needs_reboot ->
            vm.Vm.on_reboot
        | Some Needs_poweroff ->
            vm.Vm.on_shutdown
        | Some Needs_crashdump ->
            (* A VM which crashes too quickly should be shutdown *)
            if run_time < 120. then (
              warn
                "VM %s crashed too quickly after start (%.2f seconds); \
                 shutting down"
                id run_time ;
              vm.Vm.on_crash
              |> List.map (function Vm.Start -> Vm.Shutdown | other -> other)
            ) else
              vm.Vm.on_crash
        | Some Needs_suspend ->
            warn "VM %s has unexpectedly suspended" id ;
            [Vm.Shutdown]
        | Some Needs_softreset ->
            vm.Vm.on_softreboot
        | None ->
            debug "VM %s is not requesting any attention" id ;
            []
      in
      let operations_of_action = function
        | Vm.Coredump ->
            []
        | Vm.Shutdown ->
            [VM_shutdown (id, None)]
        | Vm.Start ->
            let delay =
              if run_time < B.VM.minimum_reboot_delay then (
                debug "VM %s rebooted too quickly; inserting delay" id ;
                [Atomic (VM_delay (id, 15.))]
              ) else
                []
            in
            delay @ [VM_reboot (id, None)]
        | Vm.Pause ->
            [Atomic (VM_pause id)]
        | Vm.Softreboot ->
            [Atomic (VM_softreboot id)]
      in
      let operations = List.concat (List.map operations_of_action actions) in
      List.iter (fun x -> perform_exn x t) operations ;
      VM_DB.signal id
  | PCI_check_state id ->
      debug "PCI.check_state %s" (PCI_DB.string_of_id id) ;
      let vif_t = PCI_DB.read_exn id in
      let vm_state = B.VM.get_state (VM_DB.read_exn (PCI_DB.vm_of id)) in
      let request =
        if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
        then
          B.PCI.get_device_action_request (VIF_DB.vm_of id) vif_t
        else
          Some Needs_unplug
      in
      let operations_of_request = function
        | Needs_unplug ->
            Some (Atomic (PCI_unplug id))
        | Needs_set_qos ->
            None
      in
      let operations =
        List.filter_map operations_of_request (Option.to_list request)
      in
      List.iter (fun x -> perform_exn x t) operations
  | VBD_check_state id ->
      debug "VBD.check_state %s" (VBD_DB.string_of_id id) ;
      let vbd_t = VBD_DB.read_exn id in
      let vm_state = B.VM.get_state (VM_DB.read_exn (VBD_DB.vm_of id)) in
      let request =
        if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
        then
          B.VBD.get_device_action_request (VBD_DB.vm_of id) vbd_t
        else (
          debug "VM %s is not running: VBD_unplug needed" (VBD_DB.vm_of id) ;
          Some Needs_unplug
        )
      in
      let operations_of_request = function
        | Needs_unplug ->
            Some (Atomic (VBD_unplug (id, true)))
        | Needs_set_qos ->
            Some (Atomic (VBD_set_qos id))
      in
      let operations =
        List.filter_map operations_of_request (Option.to_list request)
      in
      List.iter (fun x -> perform_exn x t) operations ;
      (* Needed (eg) to reflect a spontaneously-ejected CD *)
      VBD_DB.signal id
  | VIF_check_state id ->
      debug "VIF.check_state %s" (VIF_DB.string_of_id id) ;
      let vif_t = VIF_DB.read_exn id in
      let vm_state = B.VM.get_state (VM_DB.read_exn (VIF_DB.vm_of id)) in
      let request =
        if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
        then
          B.VIF.get_device_action_request (VIF_DB.vm_of id) vif_t
        else
          Some Needs_unplug
      in
      let operations_of_request = function
        | Needs_unplug ->
            Some (Atomic (VIF_unplug (id, true)))
        | Needs_set_qos ->
            None
      in
      let operations =
        List.filter_map operations_of_request (Option.to_list request)
      in
      List.iter (fun x -> perform_exn x t) operations
  | VUSB_check_state id ->
      debug "VUSB.check_state %s" (VUSB_DB.string_of_id id) ;
      let vusb_t = VUSB_DB.read_exn id in
      let vm_state = B.VM.get_state (VM_DB.read_exn (VUSB_DB.vm_of id)) in
      let request =
        if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
        then
          B.VUSB.get_device_action_request (VUSB_DB.vm_of id) vusb_t
        else (
          debug "VM %s is not running: VUSB_unplug needed" (VUSB_DB.vm_of id) ;
          Some Needs_unplug
        )
      in
      let operations_of_request = function
        | Needs_unplug ->
            Some (Atomic (VUSB_unplug id))
        | Needs_set_qos ->
            None
      in
      let operations =
        List.filter_map operations_of_request (Option.to_list request)
      in
      List.iter (fun x -> perform_exn x t) operations ;
      VUSB_DB.signal id
  | Atomic op ->
      let progress_callback = progress_callback 0. 1. t in
      perform_atomic ~progress_callback ?subtask ?result op t

and verify_power_state op =
  let module B = (val get_backend () : S) in
  let assert_power_state_is vm_id expected =
    let power = (B.VM.get_state (VM_DB.read_exn vm_id)).Vm.power_state in
    if not (List.mem power expected) then
      let expected' =
        match expected with x :: _ -> x | [] -> failwith "Expectation missing"
      in
      raise (Xenopsd_error (Bad_power_state (power, expected')))
  in
  match op with
  | VM_start (id, _) ->
      assert_power_state_is id [Halted]
  | VM_reboot (id, _) ->
      assert_power_state_is id [Running; Paused]
  | VM_resume (id, _) ->
      (* We also accept Halted here: when resuming a pre-Lima VM, the
         suspend_memory_bytes field in the internal state is always 0, causing
         B.VM.get_state to return Halted. *)
      assert_power_state_is id [Suspended; Halted]
  | _ ->
      ()

and perform ?subtask ?result (op : operation) (t : Xenops_task.task_handle) :
    unit =
  let one op =
    verify_power_state op ;
    try perform_exn ?subtask ?result op t
    with e ->
      Backtrace.is_important e ;
      info "Caught %s executing %s: triggering cleanup actions"
        (Printexc.to_string e) (string_of_operation op) ;
      ( try trigger_cleanup_after_failure op t
        with e ->
          Backtrace.is_important e ;
          error "Triggering cleanup actions failed: %s" (Printexc.to_string e)
      ) ;
      raise e
  in
  match subtask with
  | None ->
      one op
  | Some name ->
      Xenops_task.with_subtask t name (fun () -> one op)

let uses_mxgpu id =
  List.exists
    (fun vgpu_id ->
      let vgpu = VGPU_DB.read_exn vgpu_id in
      match vgpu.Vgpu.implementation with Vgpu.MxGPU _ -> true | _ -> false
    )
    (VGPU_DB.ids id)

let queue_operation_int ?traceparent dbg id op =
  let task =
    Xenops_task.add ?traceparent tasks dbg
      (let r = ref None in
       fun t -> perform ~result:r op t ; !r
      )
  in
  let tag = if uses_mxgpu id then "mxgpu" else id in
  Redirector.push Redirector.default tag (op, task) ;
  task

let queue_operation ?traceparent dbg id op =
  let task = queue_operation_int ?traceparent dbg id op in
  Xenops_task.id_of_handle task

let queue_operation_and_wait dbg id op =
  let from = Updates.last_id dbg updates in
  let task = queue_operation_int dbg id op in
  let task_id = Xenops_task.id_of_handle task in
  event_wait updates task ~from 1200.0 (is_task task_id) task_ended |> ignore ;
  task

module PCI = struct
  open Pci
  module DB = PCI_DB

  let string_of_id (a, b) = a ^ "." ^ b

  let add _ dbg x = Debug.with_thread_associated dbg (fun () -> DB.add' x) ()

  let remove _ dbg id =
    Debug.with_thread_associated dbg (fun () -> DB.remove' id) ()

  let stat' id =
    debug "PCI.stat %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    let pci_t = PCI_DB.read_exn id in
    let state = B.PCI.get_state (DB.vm_of id) pci_t in
    (pci_t, state)

  let stat _ dbg id = Debug.with_thread_associated dbg (fun () -> stat' id) ()

  let list _ dbg vm =
    Debug.with_thread_associated dbg
      (fun () -> debug "PCI.list %s" vm ; DB.list vm)
      ()

  let dequarantine _ dbg (pci_addr : Pci.address) =
    let module B = (val get_backend () : S) in
    let f () =
      debug "PCI.dequarantine %s" (string_of_address pci_addr) ;
      B.PCI.dequarantine pci_addr
    in
    Debug.with_thread_associated dbg f ()
end

module VGPU = struct
  module DB = VGPU_DB

  let string_of_id (a, b) = a ^ "." ^ b

  let add _ dbg x = Debug.with_thread_associated dbg (fun () -> DB.add' x) ()

  let remove _ dbg x =
    Debug.with_thread_associated dbg (fun () -> DB.remove' x) ()

  let stat' id =
    debug "VGPU.stat %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    let vgpu_t = VGPU_DB.read_exn id in
    let state = B.VGPU.get_state (DB.vm_of id) vgpu_t in
    (vgpu_t, state)

  let stat _ dbg id = Debug.with_thread_associated dbg (fun () -> stat' id) ()

  let list _ dbg vm =
    Debug.with_thread_associated dbg
      (fun () -> debug "VGPU.list %s" vm ; DB.list vm)
      ()
end

module VUSB = struct
  module DB = VUSB_DB

  let string_of_id (a, b) = a ^ "." ^ b

  let add _ dbg x = Debug.with_thread_associated dbg (fun () -> DB.add' x) ()

  let plug _ dbg id = queue_operation dbg (DB.vm_of id) (Atomic (VUSB_plug id))

  let unplug _ dbg id =
    queue_operation dbg (DB.vm_of id) (Atomic (VUSB_unplug id))

  let remove _ dbg id =
    Debug.with_thread_associated dbg (fun () -> DB.remove' id) ()

  let stat' id =
    debug "VUSB.stat %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    let vusb_t = VUSB_DB.read_exn id in
    let state = B.VUSB.get_state (DB.vm_of id) vusb_t in
    (vusb_t, state)

  let stat _ dbg id = Debug.with_thread_associated dbg (fun () -> stat' id) ()

  let list _ dbg vm =
    Debug.with_thread_associated dbg
      (fun () -> debug "VUSB.list %s" vm ; DB.list vm)
      ()
end

module VBD = struct
  module DB = VBD_DB

  let string_of_id (a, b) = a ^ "." ^ b

  let add _ dbg x = Debug.with_thread_associated dbg (fun () -> DB.add' x) ()

  let plug _ dbg id = queue_operation dbg (DB.vm_of id) (VBD_hotplug id)

  let unplug _ dbg id force =
    queue_operation dbg (DB.vm_of id) (VBD_hotunplug (id, force))

  let insert _ dbg id disk =
    queue_operation dbg (DB.vm_of id) (Atomic (VBD_insert (id, disk)))

  let eject _ dbg id = queue_operation dbg (DB.vm_of id) (Atomic (VBD_eject id))

  let remove _ dbg id =
    Debug.with_thread_associated dbg (fun () -> DB.remove' id) ()

  let stat' id =
    debug "VBD.stat %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    let vbd_t = VBD_DB.read_exn id in
    let state = B.VBD.get_state (DB.vm_of id) vbd_t in
    (vbd_t, state)

  let stat _ dbg id = Debug.with_thread_associated dbg (fun () -> stat' id) ()

  let list _ dbg vm =
    Debug.with_thread_associated dbg
      (fun () -> debug "VBD.list %s" vm ; DB.list vm)
      ()
end

module VIF = struct
  module DB = VIF_DB

  let string_of_id (a, b) = a ^ "." ^ b

  let add _ dbg x = Debug.with_thread_associated dbg (fun () -> DB.add' x) ()

  let plug _ dbg id = queue_operation dbg (DB.vm_of id) (VIF_hotplug id)

  let unplug _ dbg id force =
    queue_operation dbg (DB.vm_of id) (VIF_hotunplug (id, force))

  let move _ dbg id network =
    queue_operation dbg (DB.vm_of id) (Atomic (VIF_move (id, network)))

  let set_carrier _ dbg id carrier =
    queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_carrier (id, carrier)))

  let set_locking_mode _ dbg id mode =
    queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_locking_mode (id, mode)))

  let set_pvs_proxy _ dbg id proxy =
    queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_pvs_proxy (id, proxy)))

  let set_ipv4_configuration _ dbg id ipv4_configuration =
    queue_operation dbg (DB.vm_of id)
      (Atomic (VIF_set_ipv4_configuration (id, ipv4_configuration)))

  let set_ipv6_configuration _ dbg id ipv6_configuration =
    queue_operation dbg (DB.vm_of id)
      (Atomic (VIF_set_ipv6_configuration (id, ipv6_configuration)))

  let remove _ dbg id =
    Debug.with_thread_associated dbg (fun () -> DB.remove' id) ()

  let stat' id =
    debug "VIF.stat %s" (string_of_id id) ;
    let module B = (val get_backend () : S) in
    let vif_t = VIF_DB.read_exn id in
    let state = B.VIF.get_state (DB.vm_of id) vif_t in
    (vif_t, state)

  let stat _ dbg id = Debug.with_thread_associated dbg (fun () -> stat' id) ()

  let list _ dbg vm =
    Debug.with_thread_associated dbg
      (fun () -> debug "VIF.list %s" vm ; DB.list vm)
      ()
end

let numa_placement = ref Xenops_interface.Host.Any

module HOST = struct
  let stat _ dbg =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.stat" ;
        let module B = (val get_backend () : S) in
        B.HOST.stat ()
      )
      ()

  let set_numa_affinity_policy _ dbg =
    Debug.with_thread_associated dbg @@ fun policy ->
    debug "HOST.set_numa_affinity_policy" ;
    numa_placement := policy

  let get_console_data _ dbg =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.get_console_data" ;
        let module B = (val get_backend () : S) in
        B.HOST.get_console_data ()
      )
      ()

  let get_total_memory_mib _ dbg =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.get_total_memory_mib" ;
        let module B = (val get_backend () : S) in
        B.HOST.get_total_memory_mib ()
      )
      ()

  let send_debug_keys _ dbg keys =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.send_debug_keys %s" keys ;
        let module B = (val get_backend () : S) in
        B.HOST.send_debug_keys keys
      )
      ()

  let set_worker_pool_size _ dbg size =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.set_worker_pool_size %d" size ;
        WorkerPool.set_size size
      )
      ()

  let update_guest_agent_features _ dbg features =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.update_guest_agent_features %s"
          (Rpc.Enum (List.map (rpc_of Host.guest_agent_feature) features)
          |> Jsonrpc.to_string
          ) ;
        let module B = (val get_backend () : S) in
        B.HOST.update_guest_agent_features features
      )
      ()

  let combine_cpu_policies _ dbg policy1 policy2 =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.combine_cpu_policies %s %s"
          (CPU_policy.to_string policy1)
          (CPU_policy.to_string policy2) ;
        let module B = (val get_backend () : S) in
        B.HOST.combine_cpu_policies policy1 policy2
      )
      ()

  let is_compatible _ dbg vm_policy host_policy =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "HOST.is_compatible %s %s"
          (CPU_policy.to_string vm_policy)
          (CPU_policy.to_string host_policy) ;
        let module B = (val get_backend () : S) in
        B.HOST.is_compatible vm_policy host_policy
      )
      ()
end

module VM = struct
  module DB = VM_DB

  let add _ dbg x = Debug.with_thread_associated dbg (fun () -> DB.add' x) ()

  let rename _ dbg id1 id2 when' =
    queue_operation dbg id1 (Atomic (VM_rename (id1, id2, when')))

  let remove _ dbg id =
    let task = queue_operation_and_wait dbg id (Atomic (VM_remove id)) in
    let task_id = Xenops_task.id_of_handle task in
    match Xenops_task.get_state task with
    | Task.Completed _ ->
        TASK.destroy' task_id
    | Task.Failed e ->
        TASK.destroy' task_id ;
        let e =
          match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty e with
          | Ok e ->
              Xenopsd_error e
          | Error (`Msg m) ->
              internal_error "Error unmarshalling error: %s" m
        in
        raise e
    | Task.Pending _ ->
        error "VM.remove: queue_operation_and_wait returned a pending task" ;
        Xenops_task.cancel task ;
        raise (Xenopsd_error (Cancelled task_id))

  let stat' x =
    debug "VM.stat %s" x ;
    let module B = (val get_backend () : S) in
    let vm_t = VM_DB.read_exn x in
    let state = B.VM.get_state vm_t in
    (* If we're rebooting the VM then keep the power state running *)
    let state =
      if is_rebooting x then {state with Vm.power_state= Running} else state
    in
    (vm_t, state)

  let stat _ dbg id = Debug.with_thread_associated dbg (fun () -> stat' id) ()

  let exists _ _dbg id = match DB.read id with Some _ -> true | None -> false

  let list _ dbg () = Debug.with_thread_associated dbg (fun () -> DB.list ()) ()

  let create _ dbg id =
    let no_sharept = false in
    queue_operation dbg id (Atomic (VM_create (id, None, None, no_sharept)))

  let build _ dbg id force =
    queue_operation dbg id (Atomic (VM_build (id, force)))

  let create_device_model _ dbg id save_state =
    queue_operation dbg id (Atomic (VM_create_device_model (id, save_state)))

  let destroy _ dbg id = queue_operation dbg id (Atomic (VM_destroy id))

  let pause _ dbg id = queue_operation dbg id (Atomic (VM_pause id))

  let unpause _ dbg id = queue_operation dbg id (Atomic (VM_unpause id))

  let request_rdp _ dbg id enabled =
    queue_operation dbg id (Atomic (VM_request_rdp (id, enabled)))

  let run_script _ dbg id script =
    queue_operation dbg id (Atomic (VM_run_script (id, script)))

  let set_xsdata _ dbg id xsdata =
    queue_operation dbg id (Atomic (VM_set_xsdata (id, xsdata)))

  let set_vcpus _ dbg id n =
    queue_operation dbg id (Atomic (VM_set_vcpus (id, n)))

  let set_shadow_multiplier _ dbg id n =
    queue_operation dbg id (Atomic (VM_set_shadow_multiplier (id, n)))

  let set_memory_dynamic_range _ dbg id min max =
    queue_operation dbg id (Atomic (VM_set_memory_dynamic_range (id, min, max)))

  let delay _ dbg id t = queue_operation dbg id (Atomic (VM_delay (id, t)))

  let start _ dbg id force = queue_operation dbg id (VM_start (id, force))

  let shutdown _ dbg id timeout =
    queue_operation dbg id (VM_poweroff (id, timeout))

  let reboot _ dbg id timeout = queue_operation dbg id (VM_reboot (id, timeout))

  let suspend _ dbg id disk = queue_operation dbg id (VM_suspend (id, Disk disk))

  let resume _ dbg id disk = queue_operation dbg id (VM_resume (id, Disk disk))

  let s3suspend _ dbg id = queue_operation dbg id (Atomic (VM_s3suspend id))

  let s3resume _ dbg id = queue_operation dbg id (Atomic (VM_s3resume id))

  let migrate _context dbg id vmm_vdi_map vmm_vif_map vmm_vgpu_pci_map vmm_url
      (compress : bool) (verify_dest : bool) =
    let tmp_uuid_of uuid ~kind =
      Printf.sprintf "%s00000000000%c" (String.sub uuid 0 24)
        (match kind with `dest -> '1' | `src -> '0')
    in
    queue_operation dbg id
      (VM_migrate
         {
           vmm_id= id
         ; vmm_vdi_map
         ; vmm_vif_map
         ; vmm_vgpu_pci_map
         ; vmm_url
         ; vmm_tmp_src_id= tmp_uuid_of id ~kind:`src
         ; vmm_tmp_dest_id= tmp_uuid_of id ~kind:`dest
         ; vmm_compress= compress
         ; vmm_verify_dest= verify_dest
         }
      )

  let migrate_receive_memory _ _ _ _ _ _ = failwith "Unimplemented"

  let get_compression cookies =
    let key = cookie_mem_compression in
    match List.assoc_opt key cookies with Some _ -> true | _ -> false

  let not_found_response =
    let headers = Cohttp.Header.of_list [("User-agent", "xenopsd")] in
    Cohttp.Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ()

  let receive_memory uri cookies traceparent s context : unit =
    let module Response = Cohttp.Response.Make (Cohttp_posix_io.Unbuffered_IO) in
    let dbg = List.assoc "dbg" cookies in
    let memory_limit = List.assoc "memory_limit" cookies |> Int64.of_string in
    let handshake = List.assoc_opt cookie_mem_migration cookies in
    let compressed_memory = get_compression cookies in
    Debug.with_thread_associated dbg
      (fun () ->
        debug "traceparent: %s" (Option.value ~default:"(none)" traceparent) ;
        let id, final_id =
          (* The URI is /service/xenops/memory/id *)
          let bits = Astring.String.cuts ~sep:"/" (Uri.path uri) in
          let id = bits |> List.rev |> List.hd in
          let final_id =
            match List.assoc_opt "final_id" cookies with
            | Some x ->
                x
            | None ->
                id
          in
          debug "VM.receive_memory id = %s, final_id=%s" id final_id ;
          (id, final_id)
        in
        match context.transferred_fd with
        | Some transferred_fd ->
            let op =
              VM_receive_memory
                {
                  vmr_id= id
                ; vmr_final_id= final_id
                ; vmr_memory_limit= memory_limit
                ; vmr_socket= transferred_fd
                ; vmr_handshake= handshake
                ; vmr_compressed= compressed_memory
                }
            in
            let task = Some (queue_operation ?traceparent dbg id op) in
            Option.iter
              (fun t -> t |> Xenops_client.wait_for_task dbg |> ignore)
              task
        | None ->
            Response.write (fun _ -> ()) not_found_response s
      )
      ()

  (* This is modelled closely on receive_memory and there is significant scope
     for refactoring. *)
  let receive_vgpu uri cookies _traceparent s context : unit =
    let module Response = Cohttp.Response.Make (Cohttp_posix_io.Unbuffered_IO) in
    let dbg = List.assoc "dbg" cookies in
    Debug.with_thread_associated dbg
      (fun () ->
        let vgpu_id =
          (* The URI is /service/xenops/migrate-vgpu/id *)
          let path = Uri.path uri in
          let bits = Astring.String.cut ~sep:"/" ~rev:true path in
          let vgpu_id_str =
            match bits with
            | Some (_, vgpu_id_str) ->
                vgpu_id_str
            | None ->
                internal_error "Could not retrieve vgpu id from path %s" path
          in
          let vgpu_id = VGPU_DB.id_of_string vgpu_id_str in
          debug "VM.receive_vgpu vgpu_id_str = %s" vgpu_id_str ;
          vgpu_id
        in
        let vm_id = VGPU_DB.vm_of vgpu_id in
        match context.transferred_fd with
        | Some transferred_fd ->
            (* prevent vgpu-migration from pre-Jura to Jura and later *)
            if not (List.mem_assoc cookie_vgpu_migration cookies) then (
              (* only Jura and later hosts send this cookie; fail the migration
                 from pre-Jura hosts *)
              let msg =
                Printf.sprintf
                  "VM.migrate: version of sending host incompatible with \
                   receiving host: no cookie %s"
                  cookie_vgpu_migration
              in
              Xenops_migrate.(
                Handshake.send transferred_fd (Handshake.Error msg)
              ) ;
              debug "VM.receive_vgpu: Synchronisation point 1-vgpu ERR %s" msg ;
              raise (Xenopsd_error (Internal_error msg))
            ) ;
            debug "VM.receive_vgpu: passed fd %d" (Obj.magic transferred_fd) ;
            (* Store away the fd for VM_receive_memory/restore to use *)
            let info =
              {vgpu_fd= transferred_fd; vgpu_channel= Event.new_channel ()}
            in
            with_lock vgpu_receiver_sync_m (fun () ->
                Hashtbl.add vgpu_receiver_sync vm_id info
            ) ;
            (* Inform the sender that everything is in place to start
               save/restore *)
            Xenops_migrate.(Handshake.send transferred_fd Handshake.Success) ;
            debug "VM.receive_vgpu: Synchronisation point 1-vgpu" ;
            (* Keep the thread/connection open until the restore is done on the
               other thread *)
            Event.receive info.vgpu_channel |> Event.sync ;
            debug "VM.receive_vgpu: Synchronisation point 2-vgpu" ;
            with_lock vgpu_receiver_sync_m (fun () ->
                Hashtbl.remove vgpu_receiver_sync vm_id
            )
        | None ->
            Response.write (fun _ -> ()) not_found_response s
      )
      ()

  (* This handler /service/xenops/migrate-mem/id  receives a connection for
     VM memory. *)
  let receive_mem uri cookies _traceparent socket context : unit =
    let module Response = Cohttp.Response.Make (Cohttp_posix_io.Unbuffered_IO) in
    let dbg = List.assoc "dbg" cookies in
    Debug.with_thread_associated dbg
      (fun () ->
        let vm = basename (Uri.path uri) in
        match context.transferred_fd with
        | Some fd ->
            debug "VM.receive_mem: passed fd %d" (Obj.magic fd) ;
            debug "VM.receive_mem: vm=%s" vm ;
            (* Store away the fd for VM_receive_memory/restore to use *)
            let info = {mem_fd= fd; mem_channel= Event.new_channel ()} in
            with_lock mem_receiver_sync_m (fun () ->
                Hashtbl.add mem_receiver_sync vm info
            ) ;
            (* Inform the sender that everything is in place to start
               save/restore *)
            Xenops_migrate.(Handshake.send fd Handshake.Success) ;
            debug "VM.receive_mem: Synchronisation point 1-mem" ;
            (* Keep the thread/connection open until the restore is done on the
               other thread *)
            Event.receive info.mem_channel |> Event.sync ;
            debug "VM.receive_mem Synchronisation point 2-mem" ;
            with_lock mem_receiver_sync_m (fun () ->
                Hashtbl.remove mem_receiver_sync vm
            )
        | None ->
            Response.write (fun _ -> ()) not_found_response socket
      )
      ()

  let generate_state_string _ _dbg vm =
    let module B = (val get_backend () : S) in
    B.VM.generate_state_string vm

  let export_metadata _ _dbg id = export_metadata [] [] [] id

  let parse_metadata s =
    let module B = (val get_backend () : S) in
    let md =
      match
        Rpcmarshal.unmarshal Metadata.t.Rpc.Types.ty (s |> Jsonrpc.of_string)
      with
      | Ok md ->
          md
      | Error (`Msg m) ->
          internal_error "Unable to unmarshal metadata: %s" m
    in
    (md.Metadata.vm.Vm.id, md)

  let import_metadata _ dbg s =
    Debug.with_thread_associated dbg
      (fun () ->
        let id, md = parse_metadata s in
        let op = Atomic (VM_import_metadata (id, md)) in
        (* We allow a higher-level toolstack to replace the metadata of a
           running VM. Any changes will take place on next reboot.
           The metadata update will be queued so that ongoing operations
           do not see unexpected state changes. *)
        if DB.exists id then
          ignore (queue_operation_and_wait dbg id op)
        else
          immediate_operation dbg id op ;
        id
      )
      ()

  let import_metadata_async _ dbg s =
    let id, md = parse_metadata s in
    queue_operation dbg id (Atomic (VM_import_metadata (id, md)))
end

module DEBUG = struct
  let trigger _ dbg cmd args =
    Debug.with_thread_associated dbg
      (fun () ->
        let module B = (val get_backend () : S) in
        match (cmd, args) with
        | "set-cancel-trigger", [dbg; n] ->
            debug "Will automatically cancel any task with dbg = %s at step %s"
              dbg n ;
            Xenops_task.set_cancel_trigger tasks dbg (int_of_string n)
        | _, _ ->
            B.DEBUG.trigger cmd args
      )
      ()

  let shutdown _ dbg () =
    Debug.with_thread_associated dbg
      (fun () -> debug "DEBUG.shutdown" ; exit 0)
      ()
end

module UPDATES = struct
  let get _ dbg last timeout =
    Debug.with_thread_associated dbg
      (fun () -> Updates.get dbg last timeout updates)
      ()

  let last_id _ dbg =
    Debug.with_thread_associated dbg (fun () -> Updates.last_id dbg updates) ()

  let inject_barrier _ dbg vm_id id =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "UPDATES.inject_barrier %s %d" vm_id id ;
        let filter k _ =
          match k with
          | Dynamic.Task _ ->
              false
          | Dynamic.Vm id
          | Dynamic.Vbd (id, _)
          | Dynamic.Vif (id, _)
          | Dynamic.Pci (id, _)
          | Dynamic.Vusb (id, _)
          | Dynamic.Vgpu (id, _) ->
              id = vm_id
        in
        Updates.inject_barrier id filter updates
      )
      ()

  let remove_barrier _ dbg id =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "UPDATES.remove_barrier %d" id ;
        Updates.remove_barrier id updates
      )
      ()

  let refresh_vm _ dbg id =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "UPDATES.refresh_vm %s" id ;
        VM_DB.signal id ;
        List.iter VBD_DB.signal (VBD_DB.ids id) ;
        List.iter VIF_DB.signal (VIF_DB.ids id) ;
        List.iter PCI_DB.signal (PCI_DB.ids id) ;
        List.iter VGPU_DB.signal (VGPU_DB.ids id) ;
        List.iter VUSB_DB.signal (VUSB_DB.ids id) ;
        ()
      )
      ()
end

let internal_event_thread = ref None

let internal_event_thread_body =
  Debug.with_thread_associated "events" (fun () ->
      debug "Starting internal event thread" ;
      let dbg = "events" in
      let module B = (val get_backend () : S) in
      let id = ref None in
      while true do
        try
          while true do
            let _, updates, next_id = B.UPDATES.get !id None in
            (* Note, backend updates don't use barriers so we should always get
               updates. *)
            if updates = [] then
              error
                "Event thread received an empty list of events: this should \
                 never happen" ;
            List.iter
              (function
                | Dynamic.Vm id ->
                    debug "Received an event on managed VM %s" id ;
                    queue_operation dbg id (VM_check_state id) |> TASK.destroy'
                | Dynamic.Vbd id ->
                    debug "Received an event on managed VBD %s.%s" (fst id)
                      (snd id) ;
                    queue_operation dbg (VBD_DB.vm_of id) (VBD_check_state id)
                    |> TASK.destroy'
                | Dynamic.Vif id ->
                    debug "Received an event on managed VIF %s.%s" (fst id)
                      (snd id) ;
                    queue_operation dbg (VIF_DB.vm_of id) (VIF_check_state id)
                    |> TASK.destroy'
                | Dynamic.Pci id ->
                    debug "Received an event on managed PCI %s.%s" (fst id)
                      (snd id) ;
                    queue_operation dbg (PCI_DB.vm_of id) (PCI_check_state id)
                    |> TASK.destroy'
                | Dynamic.Vusb id ->
                    debug "Received an event on managed VUSB %s.%s" (fst id)
                      (snd id) ;
                    queue_operation dbg (VUSB_DB.vm_of id) (VUSB_check_state id)
                    |> TASK.destroy'
                | x ->
                    debug "Ignoring event on %s"
                      (Jsonrpc.to_string (rpc_of Dynamic.id x))
                )
              updates ;
            id := Some next_id
          done
        with e ->
          error "Event thread caught: %s; restarting after 5s"
            (Printexc.to_string e) ;
          Thread.delay 5.
      done
  )

let set_backend m =
  backend := m ;
  (* start the internal event thread *)
  internal_event_thread := Some (Thread.create internal_event_thread_body ()) ;
  let module B = (val get_backend () : S) in
  B.init ()

let register_objects () =
  (* Make sure all objects are 'registered' with the updates system *)
  List.iter
    (fun vm ->
      VM_DB.signal vm ;
      List.iter VBD_DB.signal (VBD_DB.ids vm) ;
      List.iter VBD_DB.signal (VIF_DB.ids vm) ;
      List.iter PCI_DB.signal (PCI_DB.ids vm) ;
      List.iter VGPU_DB.signal (VGPU_DB.ids vm) ;
      List.iter VUSB_DB.signal (VUSB_DB.ids vm)
    )
    (VM_DB.ids ())

let upgrade_internal_state_of_running_vms () =
  (* B.VM.set_internal_state contains upgrade code, needed for the case that
     the type of the backend's internal state is extended in a xenopsd update. *)
  let module B = (val get_backend () : S) in
  List.iter
    (fun vm ->
      try
        let vm_t = VM_DB.read_exn vm in
        B.VM.get_internal_state [] [] vm_t |> B.VM.set_internal_state vm_t
      with
      | Xenops_interface.Xenopsd_error (Does_not_exist _) ->
          warn "Missing VM extra metadata for VM %s" vm
      | e ->
          warn "VM metadata upgrade failed for VM %s (%s)" vm
            (Printexc.to_string e)
    )
    (VM_DB.ids ())

type rpc_t = Rpc.t

let typ_of_rpc_t =
  Rpc.Types.(
    Abstract
      {
        aname= "Rpc.t"
      ; test_data= [Rpc.Null]
      ; rpc_of= (fun x -> x)
      ; of_rpc= (fun x -> Ok x)
      }
  )

module Diagnostics = struct
  type t = {
      queues: Redirector.Dump.t
    ; workers: WorkerPool.Dump.t
    ; scheduler: rpc_t
    ; updates: rpc_t
    ; tasks: WorkerPool.Dump.task list
    ; vm_actions: (string * domain_action_request option) list
  }
  [@@deriving rpcty]

  let make () =
    let module B = (val get_backend () : S) in
    {
      queues= Redirector.Dump.make ()
    ; workers= WorkerPool.Dump.make ()
    ; scheduler= Scheduler.Dump.make scheduler |> Scheduler.Dump.rpc_of_dump
    ; updates= Updates.Dump.make updates |> Updates.Dump.rpc_of_dump
    ; tasks= List.map WorkerPool.Dump.of_task (Xenops_task.list tasks)
    ; vm_actions=
        List.filter_map
          (fun id ->
            match VM_DB.read id with
            | Some vm ->
                Some (id, B.VM.get_domain_action_request vm)
            | None ->
                None
          )
          (VM_DB.ids ())
    }
end

module Observer = struct
  let create _ dbg uuid name_label attributes endpoints enabled =
    debug "Observer.create : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () ->
        Tracing.create ~uuid ~name_label ~attributes ~endpoints ~enabled
      )
      ()

  let destroy _ dbg uuid =
    debug "Observer.destroy : dbg=%s" dbg ;
    Debug.with_thread_associated dbg (fun () -> Tracing.destroy ~uuid) ()

  let set_enabled _ dbg uuid enabled =
    debug "Observer.set_enabled : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing.set ~uuid ~enabled ())
      ()

  let set_attributes _ dbg uuid attributes =
    debug "Observer.set_attributes : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing.set ~uuid ~attributes ())
      ()

  let set_endpoints _ dbg uuid endpoints =
    debug "Observer.set_endpoint : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing.set ~uuid ~endpoints ())
      ()

  let init _ dbg =
    debug "Observer.init : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> ignore @@ Tracing_export.main ())
      ()

  let set_trace_log_dir _ dbg dir =
    debug "Observer.set_trace_log_dir : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing_export.Destination.File.set_trace_log_dir dir)
      ()

  let set_export_interval _ dbg interval =
    debug "Observer.set_export_interval : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing_export.set_export_interval interval)
      ()

  let set_max_spans _ dbg spans =
    debug "Observer.set_max_spans : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing.Spans.set_max_spans spans)
      ()

  let set_max_traces _ dbg traces =
    debug "Observer.set_max_traces : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing.Spans.set_max_traces traces)
      ()

  let set_max_file_size _ dbg file_size =
    debug "Observer.set_max_file_size : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing_export.Destination.File.set_max_file_size file_size)
      ()

  let set_host_id _ dbg host_id =
    debug "Observer.set_host_id : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () -> Tracing_export.set_host_id host_id)
      ()

  let set_compress_tracing_files _ dbg enabled =
    debug "Observer.set_compress_tracing_files : dbg=%s" dbg ;
    Debug.with_thread_associated dbg
      (fun () ->
        Tracing_export.Destination.File.set_compress_tracing_files enabled
      )
      ()
end

let get_diagnostics _ _ () =
  Diagnostics.make () |> rpc_of Diagnostics.t |> Jsonrpc.to_string

module Server = Xenops_interface.XenopsAPI (Idl.Exn.GenServer ())

let _ =
  Server.query (query ()) ;
  Server.get_diagnostics (get_diagnostics ()) ;
  Server.TASK.cancel (TASK.cancel ()) ;
  Server.TASK.stat (TASK.stat ()) ;
  Server.TASK.list (TASK.list ()) ;
  Server.TASK.destroy (TASK.destroy ()) ;
  Server.TASK.destroy_on_finish (TASK.destroy_on_finish ()) ;
  Server.HOST.stat (HOST.stat ()) ;
  Server.HOST.set_numa_affinity_policy (HOST.set_numa_affinity_policy ()) ;
  Server.HOST.get_console_data (HOST.get_console_data ()) ;
  Server.HOST.get_total_memory_mib (HOST.get_total_memory_mib ()) ;
  Server.HOST.send_debug_keys (HOST.send_debug_keys ()) ;
  Server.HOST.set_worker_pool_size (HOST.set_worker_pool_size ()) ;
  Server.HOST.update_guest_agent_features (HOST.update_guest_agent_features ()) ;
  Server.HOST.combine_cpu_policies (HOST.combine_cpu_policies ()) ;
  Server.HOST.is_compatible (HOST.is_compatible ()) ;
  Server.VM.add (VM.add ()) ;
  Server.VM.remove (VM.remove ()) ;
  Server.VM.migrate (VM.migrate ()) ;
  Server.VM.create (VM.create ()) ;
  Server.VM.build (VM.build ()) ;
  Server.VM.create_device_model (VM.create_device_model ()) ;
  Server.VM.destroy (VM.destroy ()) ;
  Server.VM.pause (VM.pause ()) ;
  Server.VM.unpause (VM.unpause ()) ;
  Server.VM.request_rdp (VM.request_rdp ()) ;
  Server.VM.run_script (VM.run_script ()) ;
  Server.VM.set_xsdata (VM.set_xsdata ()) ;
  Server.VM.set_vcpus (VM.set_vcpus ()) ;
  Server.VM.set_shadow_multiplier (VM.set_shadow_multiplier ()) ;
  Server.VM.set_memory_dynamic_range (VM.set_memory_dynamic_range ()) ;
  Server.VM.stat (VM.stat ()) ;
  Server.VM.exists (VM.exists ()) ;
  Server.VM.list (VM.list ()) ;
  Server.VM.delay (VM.delay ()) ;
  Server.VM.start (VM.start ()) ;
  Server.VM.shutdown (VM.shutdown ()) ;
  Server.VM.reboot (VM.reboot ()) ;
  Server.VM.suspend (VM.suspend ()) ;
  Server.VM.resume (VM.resume ()) ;
  Server.VM.s3suspend (VM.s3suspend ()) ;
  Server.VM.s3resume (VM.s3resume ()) ;
  Server.VM.export_metadata (VM.export_metadata ()) ;
  Server.VM.import_metadata (VM.import_metadata ()) ;
  Server.VM.import_metadata_async (VM.import_metadata_async ()) ;
  Server.VM.generate_state_string (VM.generate_state_string ()) ;
  Server.PCI.add (PCI.add ()) ;
  Server.PCI.remove (PCI.remove ()) ;
  Server.PCI.stat (PCI.stat ()) ;
  Server.PCI.list (PCI.list ()) ;
  Server.PCI.dequarantine (PCI.dequarantine ()) ;
  Server.VBD.add (VBD.add ()) ;
  Server.VBD.remove (VBD.remove ()) ;
  Server.VBD.stat (VBD.stat ()) ;
  Server.VBD.list (VBD.list ()) ;
  Server.VBD.plug (VBD.plug ()) ;
  Server.VBD.unplug (VBD.unplug ()) ;
  Server.VBD.eject (VBD.eject ()) ;
  Server.VBD.insert (VBD.insert ()) ;
  Server.VUSB.add (VUSB.add ()) ;
  Server.VUSB.remove (VUSB.remove ()) ;
  Server.VUSB.stat (VUSB.stat ()) ;
  Server.VUSB.list (VUSB.list ()) ;
  Server.VUSB.plug (VUSB.plug ()) ;
  Server.VUSB.unplug (VUSB.unplug ()) ;
  Server.VIF.add (VIF.add ()) ;
  Server.VIF.remove (VIF.remove ()) ;
  Server.VIF.move (VIF.move ()) ;
  Server.VIF.stat (VIF.stat ()) ;
  Server.VIF.list (VIF.list ()) ;
  Server.VIF.plug (VIF.plug ()) ;
  Server.VIF.unplug (VIF.unplug ()) ;
  Server.VIF.set_carrier (VIF.set_carrier ()) ;
  Server.VIF.set_locking_mode (VIF.set_locking_mode ()) ;
  Server.VIF.set_ipv4_configuration (VIF.set_ipv4_configuration ()) ;
  Server.VIF.set_ipv6_configuration (VIF.set_ipv6_configuration ()) ;
  Server.VIF.set_pvs_proxy (VIF.set_pvs_proxy ()) ;
  Server.VGPU.add (VGPU.add ()) ;
  Server.VGPU.remove (VGPU.remove ()) ;
  Server.VGPU.stat (VGPU.stat ()) ;
  Server.VGPU.list (VGPU.list ()) ;
  Server.UPDATES.get (UPDATES.get ()) ;
  Server.UPDATES.last_id (UPDATES.last_id ()) ;
  Server.UPDATES.inject_barrier (UPDATES.inject_barrier ()) ;
  Server.UPDATES.remove_barrier (UPDATES.remove_barrier ()) ;
  Server.UPDATES.refresh_vm (UPDATES.refresh_vm ()) ;
  Server.DEBUG.trigger (DEBUG.trigger ()) ;
  Server.DEBUG.shutdown (DEBUG.shutdown ()) ;
  Server.Observer.create (Observer.create ()) ;
  Server.Observer.destroy (Observer.destroy ()) ;
  Server.Observer.set_enabled (Observer.set_enabled ()) ;
  Server.Observer.set_attributes (Observer.set_attributes ()) ;
  Server.Observer.set_endpoints (Observer.set_endpoints ()) ;
  Server.Observer.init (Observer.init ()) ;
  Server.Observer.set_trace_log_dir (Observer.set_trace_log_dir ()) ;
  Server.Observer.set_export_interval (Observer.set_export_interval ()) ;
  Server.Observer.set_max_spans (Observer.set_max_spans ()) ;
  Server.Observer.set_max_traces (Observer.set_max_traces ()) ;
  Server.Observer.set_max_file_size (Observer.set_max_file_size ()) ;
  Server.Observer.set_host_id (Observer.set_host_id ()) ;
  Server.Observer.set_compress_tracing_files
    (Observer.set_compress_tracing_files ())
