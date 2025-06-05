(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

open Rpc
open Idl

module D = Debug.Make (struct let name = "storage_interface" end)

open D

type rpc_t = Rpc.t

let typ_of_rpc_t =
  let open Types in
  Abstract
    {
      aname= "Rpc.t"
    ; test_data= [Null]
    ; rpc_of= (fun x -> x)
    ; of_rpc= (fun x -> Ok x)
    }

let service_name = "storage"

let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"

let default_path = ref (Filename.concat default_sockets_dir service_name)

let set_sockets_dir x = default_path := Filename.concat x service_name

let uri () = "file:" ^ !default_path

let rpc_of t x = Rpcmarshal.marshal t.Rpc.Types.ty x

let of_rpc t x =
  match Rpcmarshal.unmarshal t.Rpc.Types.ty x with
  | Ok y ->
      y
  | Error (`Msg m) ->
      failwith
        (Printf.sprintf "Error unmarshalling type %s: %s" t.Rpc.Types.name m)

(** Primary key identifying the SR *)
module type WRAPPEDSTRING = sig
  type t

  val typ_of : t Rpc.Types.typ

  val t : t Rpc.Types.def

  val string_of : t -> string

  val of_string : string -> t
end

module Sr : WRAPPEDSTRING = struct
  type t = string [@@deriving rpcty]

  let string_of x = x

  let of_string x = x
end

type sr = Sr.t

let sr_pp : Format.formatter -> sr -> unit =
 fun ppf sr -> Format.fprintf ppf "%s" (Sr.string_of sr)

(** Primary key identifying a VDI within an SR *)
module Vdi : WRAPPEDSTRING = struct
  type t = string [@@deriving rpcty]

  let string_of x = x

  let of_string x = x
end

type vdi = Vdi.t

let vdi_pp : Format.formatter -> vdi -> unit =
 fun ppf vdi -> Format.fprintf ppf "%s" (Vdi.string_of vdi)

module Vm : WRAPPEDSTRING = struct
  (* This is the domid. *)
  type t = string [@@deriving rpcty]

  let string_of x = x

  let of_string x = x
end

type vm = Vm.t

let vm_pp : Format.formatter -> vm -> unit =
 fun ppf vm -> Format.fprintf ppf "%s" (Vm.string_of vm)

(** Opaque identifier used by the client to identify a particular operation *)
type debug_info = string [@@deriving rpcty]

(** The result of a successful call to the deprecated VDI.attach: this
    information (eg) can be used to connect a VBD backend to a VBD frontend *)
type attach_info = {
    params: string
  ; o_direct: bool
  ; o_direct_reason: string
  ; xenstore_data: (string * string) list
}
[@@deriving rpcty]

type xendisk = {
    params: string  (** Put into the "params" key in xenstore *)
  ; extra: (string * string) list
        (** Key-value pairs to be put into the "extra" subdirectory underneath
            the xenstore backend *)
  ; backend_type: string
}
[@@deriving rpcty]

type block_device = {path: string  (** Path to the block device *)}
[@@deriving rpcty]

type file = {path: string  (** Path to the raw file *)} [@@deriving rpcty]

type nbd = {
    uri: string
        (** NBD URI of the form nbd:unix:<domain-socket>:exportname=<NAME> (this
            format is used by qemu-system:
            https://manpages.debian.org/stretch/qemu-system-x86/qemu-system-x86_64.1.en.html) *)
}
[@@deriving rpcty]

type implementation =
  | XenDisk of xendisk
  | BlockDevice of block_device
  | File of file
  | Nbd of nbd
[@@deriving rpcty]

type backend = {implementations: implementation list} [@@deriving rpcty]

(** Extracts the UNIX domain socket path and the export name from the NBD URI in
    the NBD information returned from the VDI.attach2 SMAPIv2 call. This has the
    format nbd:unix:<domain-socket>:exportname=<name> *)
let parse_nbd_uri nbd =
  let {uri} = nbd in
  let fail () =
    failwith
      ("Could not parse NBD URI returned from the storage backend: " ^ uri)
  in
  match String.split_on_char ':' uri with
  | ["nbd"; "unix"; socket; exportname] -> (
      let prefix = "exportname=" in
      if not (Astring.String.is_prefix ~affix:prefix exportname) then fail () ;
      match Astring.String.cuts ~empty:false ~sep:prefix exportname with
      | [exportname] ->
          (socket, exportname)
      | _ ->
          fail ()
    )
  | _ ->
      fail ()

let parse_nbd_uri_opt nbd =
  try Some (parse_nbd_uri nbd) with Failure _e -> None

(** Separates the implementations of the given backend returned from the
    VDI.attach2 SMAPIv2 call based on their type *)
let implementations_of_backend backend =
  List.fold_left
    (fun (xendisks, blockdevices, files, nbds) implementation ->
      match implementation with
      | XenDisk xendisk ->
          (xendisk :: xendisks, blockdevices, files, nbds)
      | BlockDevice blockdevice ->
          (xendisks, blockdevice :: blockdevices, files, nbds)
      | File file ->
          (xendisks, blockdevices, file :: files, nbds)
      | Nbd nbd ->
          (xendisks, blockdevices, files, nbd :: nbds)
    )
    ([], [], [], []) backend.implementations

let nbd_export_of_attach_info (backend : backend) =
  let _, _, _, nbds = implementations_of_backend backend in
  match nbds with
  | [] ->
      debug "%s no nbd uri found" __FUNCTION__ ;
      None
  | uri :: _ ->
      debug "%s found nbd uri %s" __FUNCTION__ uri.uri ;
      parse_nbd_uri_opt uri |> Option.map snd

(** Uniquely identifies the contents of a VDI *)
type content_id = string [@@deriving rpcty]

(** The result of an operation which creates or examines a VDI *)
type vdi_info = {
    vdi: Vdi.t
  ; uuid: string option [@default None]
  ; content_id: content_id [@default ""]
  ; name_label: string
  ; name_description: string [@default ""]
  ; ty: string [@default "user"]
  ; (* sm_config: workaround via XenAPI *)
    metadata_of_pool: string [@default ""]
  ; is_a_snapshot: bool [@default false]
  ; snapshot_time: string [@default Clock.Date.(to_rfc3339 epoch)]
  ; snapshot_of: Vdi.t [@default Vdi.of_string ""]
  ; (* managed: workaround via XenAPI *)
    read_only: bool [@default false]
  ; cbt_enabled: bool [@default false]
  ; (* missing: workaround via XenAPI *)
    virtual_size: int64 [@default 0L]
  ; physical_utilisation: int64 [@default 0L]
  ; (* xenstore_data: workaround via XenAPI *)
    persistent: bool [@default true]
  ; sharable: bool [@default false]
  ; sm_config: (string * string) list [@default []]
}
[@@deriving rpcty]

let default_vdi_info =
  match
    Rpcmarshal.unmarshal vdi_info.Rpc.Types.ty
      Rpc.(Dict [("vdi", String ""); ("name_label", String "default")])
  with
  | Ok x ->
      x
  | Error (`Msg m) ->
      failwith (Printf.sprintf "Error creating default_vdi_info: %s" m)

type sr_health = Healthy | Recovering | Unreachable | Unavailable
[@@deriving rpcty, show {with_path= false}]

type sr_info = {
    sr_uuid: string option
  ; name_label: string
  ; name_description: string
  ; total_space: int64  (** total number of bytes on the storage substrate *)
  ; free_space: int64  (** current free space on the storage substrate *)
  ; clustered: bool
  ; health: sr_health
}
[@@deriving rpcty]

let string_of_vdi_info (x : vdi_info) = Jsonrpc.to_string (rpc_of vdi_info x)

(** Each VDI is associated with one or more "attached" or "activated"
    "datapaths". *)
type dp = string [@@deriving rpcty]

type sock_path = string [@@deriving rpcty]

type dp_stat_t = {
    superstate: Vdi_automaton.state
  ; dps: (string * Vdi_automaton.state) list
}
[@@deriving rpcty]

let string_of_dp_stat_t (x : dp_stat_t) = Jsonrpc.to_string (rpc_of dp_stat_t x)

type probe = {
    configuration: (string * string) list
  ; complete: bool
  ; sr: sr_info option
  ; extra_info: (string * string) list
}
[@@deriving rpcty]

type probe_result =
  | Raw of string (* SMAPIv1 adapters return arbitrary data *)
  | Probe of probe list
[@@deriving rpcty]

module Mirror = struct
  type id = string [@@deriving rpcty]

  type state = Receiving | Sending | Copying [@@deriving rpcty]

  type t = {source_vdi: Vdi.t; dest_vdi: Vdi.t; state: state list; failed: bool}
  [@@deriving rpcty]

  type mirror_receive_result_vhd_t = {
      mirror_vdi: vdi_info
    ; mirror_datapath: dp
    ; copy_diffs_from: content_id option
    ; copy_diffs_to: Vdi.t
    ; dummy_vdi: Vdi.t
  }
  [@@deriving rpcty]

  type mirror_receive_result_smapiv3_t = {
      mirror_vdi: vdi_info
    ; mirror_datapath: dp
    ; nbd_export: string
  }
  [@@deriving rpcty]

  (* The variant of the mirror receive result depends on the SMAPI version being used,
     rather than the VDI image type. We call the new variant SMAPIv3_mirror to reflect
     this, but keep the old one Vhd_mirror for backwards compatability reasons. *)
  type mirror_receive_result =
    | Vhd_mirror of mirror_receive_result_vhd_t
    | SMAPIv3_mirror of mirror_receive_result_smapiv3_t
  [@@deriving rpcty]

  type similars = content_id list [@@deriving rpcty]

  type copy_operation_v1 = string [@@deriving rpcty, show {with_path= false}]

  type mirror_operation_v1 = string [@@deriving rpcty, show {with_path= false}]

  (* SMAPIv3 mirror operation *)
  type operation =
    | CopyV1 of copy_operation_v1
    | MirrorV1 of mirror_operation_v1
  [@@deriving rpcty, show {with_path= false}]

  (* status of SMAPIv3 mirror *)
  type status = {failed: bool; complete: bool; progress: float option}
  [@@deriving rpcty]
end

type operation = Mirror.operation

type status = Mirror.status

type async_result_t = Vdi_info of vdi_info | Mirror_id of Mirror.id
[@@deriving rpcty]

module Task = struct
  type id = string [@@deriving rpcty]

  type async_result = async_result_t [@@deriving rpcty]

  type completion_t = {duration: float; result: async_result option}
  [@@deriving rpcty]

  type state = Pending of float | Completed of completion_t | Failed of rpc_t
  [@@deriving rpcty]

  type t = {
      id: id
    ; dbg: string
    ; ctime: float
    ; state: state
    ; subtasks: (string * state) list
    ; debug_info: (string * string) list
    ; backtrace: string
    ; cancellable: bool [@default false]
  }
  [@@deriving rpcty]
end

module Dynamic = struct
  type id = Task of Task.id | Vdi of Vdi.t | Dp of dp | Mirror of Mirror.id
  [@@deriving rpcty]

  type t =
    | Task_t of Task.id * Task.t
    | Vdi_t of Vdi.t * vdi_info
    | Dp_t of dp * dp_stat_t
    | Mirror_t of Mirror.id * Mirror.t
  [@@deriving rpcty]

  let rpc_of_id = Rpcmarshal.marshal id.Rpc.Types.ty
end

type uuid = string [@@deriving rpcty]

module Errors = struct
  type error =
    | Backend_error_with_backtrace of (string * string list)
        (** name * params *)
    | Sr_not_attached of string
        (** error: SR must be attached to access VDIs *)
    | Vdi_does_not_exist of string  (** error: the VDI is unknown *)
    | Illegal_transition of (Vdi_automaton.state * Vdi_automaton.state)
        (** This operation implies an illegal state transition *)
    | Backend_error of (string * string list)
        (** error: of the form SR_BACKEND_FAILURE *)
    | Does_not_exist of (string * string)
    | Cancelled of string
    | Redirect of string option
    | Sr_attached of string
    | Sr_unhealthy of string * sr_health
    | Unimplemented of string
    | Activated_on_another_host of uuid
    | Duplicated_key of string
    | No_storage_plugin_for_sr of string
    | Content_ids_do_not_match of (string * string)
    | Missing_configuration_parameter of string
    (* raised when preparing the environment for SXM, for example, when the dest
        host creates VDIs for data mirroring (SMAPIv1 and v3) *)
    | Migration_preparation_failure of string
    (* happens when passing fds to tapdisks for mirroring (SMAPIv1 only) *)
    | Migration_mirror_fd_failure of string
    (* raised when taking a snapshot as the base image before copying it over to
       the destination (SMAPIv1 only) *)
    | Migration_mirror_snapshot_failure of string
    (* mirror_copy_failure: raised when copying of the base image fails (SMAPIv1 only) *)
    | Migration_mirror_copy_failure of string
    (* mirror_failure: raised when there is any issues that causes the mirror to crash
       during SXM (SMAPIv1 and SMAPIv3 *)
    | Migration_mirror_failure of string
    | Internal_error of string
    | Unknown_error
  [@@default Unknown_error] [@@deriving rpcty]
end

exception Storage_error of Errors.error

let () =
  (* register printer *)
  let sprintf = Printf.sprintf in
  let string_of_error e =
    Rpcmarshal.marshal Errors.error.Rpc.Types.ty e |> Rpc.to_string
  in
  let printer = function
    | Storage_error e ->
        Some (sprintf "Storage_error (%s)" (string_of_error e))
    | _ ->
        None
  in
  Printexc.register_printer printer

let err =
  let open Idl.Error in
  {
    def= Errors.error
  ; raiser=
      (fun e ->
        let exn = Storage_error e in
        error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
        raise exn
      )
  ; matcher=
      (function
      | Storage_error e as exn ->
          error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
          Some e
      | exn ->
          error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
          Some (Internal_error (Printexc.to_string exn))
      )
  }

type smapi_version = SMAPIv1 | SMAPIv2 | SMAPIv3
[@@deriving rpcty, show {with_path= false}]

type query_result = {
    driver: string
  ; name: string
  ; description: string
  ; vendor: string
  ; copyright: string
  ; version: string
  ; required_api_version: string
  ; features: string list
  ; configuration: (string * string) list
  ; required_cluster_stack: string list
  ; smapi_version: smapi_version
}
[@@deriving rpcty]

module StorageAPI (R : RPC) = struct
  open R

  let description =
    let open Interface in
    {
      name= "Storage"
    ; namespace= None
    ; description=
        ["This interface is used by xapi to talk to the storage backends"]
    ; version= (1, 0, 0)
    }

  let implementation = implement description

  let dbg_p = Param.mk ~name:"dbg" debug_info

  let unit_p = Param.mk ~name:"unit" Types.unit

  let sr_p = Param.mk ~name:"sr" Sr.t

  let vdi_p = Param.mk ~name:"vdi" Vdi.t

  let vm_p = Param.mk ~name:"vm" Vm.t

  let dp_p = Param.mk ~name:"dp" dp

  let sock_path_p = Param.mk ~name:"sock_path" sock_path

  let device_config_p =
    Param.mk ~name:"device_config"
      ~description:["Backend-specific keys to specify the storage for the SR"]
      TypeCombinators.(list (pair (Types.string, Types.string)))

  let sm_config_p =
    Param.mk ~name:"sm_config"
      ~description:["Backend-private key-value pairs"]
      TypeCombinators.(list (pair (Types.string, Types.string)))

  let new_name_label_p = Param.mk ~name:"new_name_label" Types.string

  let new_name_description_p =
    Param.mk ~name:"new_name_description" Types.string

  module Query = struct
    (** [query ()] returns information about this storage driver *)
    let query =
      let query_result_p = Param.mk ~name:"query_result" query_result in
      declare "Query.query" ["Query the SM backend"]
        (dbg_p @-> returning query_result_p err)

    (** [diagnostics ()] returns diagnostic information about this storage
        driver *)
    let diagnostics =
      let result_p = Param.mk Types.string in
      declare "Query.diagnostics"
        ["Get diagnostic info from the SM backend"]
        (dbg_p @-> returning result_p err)
  end

  module DP = struct
    (** Functions which create/destroy (or register/unregister) dps *)

    let create =
      let id_p = Param.mk ~name:"id" Types.string in
      declare "DP.create"
        ["[DP.create dbg id]: creates and returns a dp"]
        (dbg_p @-> id_p @-> returning dp_p err)

    let destroy =
      let allow_leak_p = Param.mk ~name:"allow_leak" Types.bool in
      declare "DP.destroy"
        [
          "[DP.destroy dbg id allow_leak]: frees any resources associated with \
           [id] and destroys it."
        ; "This will typically do any needed VDI.detach, VDI.deactivate \
           cleanup."
        ]
        (dbg_p @-> dp_p @-> allow_leak_p @-> returning unit_p err)

    let destroy2 =
      let allow_leak_p = Param.mk ~name:"allow_leak" Types.bool in
      declare "DP.destroy2"
        [
          "[DP.destroy2 dbg id sr vdi vm allow_leak]: frees any resources \
           associated with [id] and destroys it."
        ; "This will typically do any needed VDI.detach, VDI.deactivate \
           cleanup."
        ]
        (dbg_p
        @-> dp_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> allow_leak_p
        @-> returning unit_p err
        )

    (** [attach_info context dbg sr vdi dp vm] returns the information as returned
    by the [attach3 dbg dp sr vdi vm _] call. Callers of this function should ensure
    that VDIs are already attached before calling this function. *)
    let attach_info =
      let backend_p = Param.mk ~name:"backend" backend in
      declare "DP.attach_info"
        [
          "[DP.attach_info  sr vdi dp]: returns the params of the dp (the \
           return value of VDI.attach2)"
        ]
        (dbg_p @-> sr_p @-> vdi_p @-> dp_p @-> vm_p @-> returning backend_p err)

    (**  *)
    let diagnostics =
      let diagnostics_p = Param.mk ~name:"diagnostics" Types.string in
      declare "DP.diagnostics"
        [
          "[DP.diagnostics ()]: returns a printable set of diagnostic \
           information,"
        ; "typically including lists of all registered datapaths and their \
           allocated"
        ; "resources."
        ]
        (unit_p @-> returning diagnostics_p err)

    let stat_vdi =
      let dp_stat_p = Param.mk ~name:"dp_stat" dp_stat_t in
      declare "DP.stat_vdi"
        [
          "[DP.stat_vdi task sr vdi ()] returns the state of the given VDI \
           from the point of view of each dp as well as the overall \
           superstate."
        ]
        (dbg_p @-> sr_p @-> vdi_p @-> unit_p @-> returning dp_stat_p err)
  end

  module SR = struct
    (** Functions which manipulate SRs *)
    let create =
      let name_label_p =
        Param.mk ~name:"name_label"
          ~description:["Human-readable name for the SR"]
          Types.string
      in
      let name_description_p =
        Param.mk ~name:"name_description"
          ~description:["Human-readable description for the SR"]
          Types.string
      in
      let size_p = Param.mk ~name:"physical_size" Types.int64 in
      declare "SR.create"
        [
          "[SR.create dbg sr name description device_config physical_size]: \
           creates a fresh SR"
        ]
        (dbg_p
        @-> sr_p
        @-> name_label_p
        @-> name_description_p
        @-> device_config_p
        @-> size_p
        @-> returning device_config_p err
        )

    (** [set_name_label sr new_name_label] updates the name_label of SR [sr]. *)
    let set_name_label =
      declare "SR.set_name_label" []
        (dbg_p @-> sr_p @-> new_name_label_p @-> returning unit_p err)

    (** [set_name_description sr new_name_description] updates the
        name_description of SR [sr]. *)
    let set_name_description =
      declare "SR.set_name_description" []
        (dbg_p @-> sr_p @-> new_name_description_p @-> returning unit_p err)

    (** [probe dbg queue device_config sm_config] searches on the storage device
        for SRs of queue [queue] *)
    let probe =
      let queue_p = Param.mk ~name:"queue" Types.string in
      let probe_result_p = Param.mk ~name:"result" probe_result in
      declare "SR.probe" []
        (dbg_p
        @-> queue_p
        @-> device_config_p
        @-> sm_config_p
        @-> returning probe_result_p err
        )

    (** [attach task sr]: attaches the SR *)
    let attach =
      declare "SR.attach" []
        (dbg_p @-> sr_p @-> device_config_p @-> returning unit_p err)

    (** [detach task sr]: detaches the SR, first detaching and/or deactivating
        any active VDIs. This may fail with Sr_not_attached, or any error from
        VDI.detach or VDI.deactivate. *)
    let detach = declare "SR.detach" [] (dbg_p @-> sr_p @-> returning unit_p err)

    (** [reset task sr]: declares that the SR has been completely reset, e.g. by
        rebooting the VM hosting the SR backend. *)
    let reset = declare "SR.reset" [] (dbg_p @-> sr_p @-> returning unit_p err)

    (** [destroy sr]: destroys (i.e. makes unattachable and unprobeable) the
        [sr], first detaching and/or deactivating any active VDIs. This may fail
        with Sr_not_attached, or any error from VDI.detach or VDI.deactivate. *)
    let destroy =
      declare "SR.destroy" [] (dbg_p @-> sr_p @-> returning unit_p err)

    (** [scan task sr] returns a list of VDIs contained within an attached SR.
    @deprecated This function is deprecated, and is only here to keep backward 
    compatibility with old xapis that call Remote.SR.scan during SXM. 
    Use the scan2 function instead. 
    *)
    let scan =
      let open TypeCombinators in
      let result = Param.mk ~name:"result" (list vdi_info) in
      declare "SR.scan" [] (dbg_p @-> sr_p @-> returning result err)

    (** [scan2 task sr] returns a list of VDIs contained within an attached SR,
    as well as the sr_info of the scanned [sr]. This operation is implemented as
    a combination of scan and stats. *)
    let scan2 =
      let open TypeCombinators in
      let result = Param.mk ~name:"result" (pair (list vdi_info, sr_info)) in
      declare "SR.scan2" [] (dbg_p @-> sr_p @-> returning result err)

    (** [update_snapshot_info_src sr vdi url dest dest_vdi snapshot_pairs] *
        updates the fields is_a_snapshot, snapshot_time and snapshot_of for a *
        list of snapshots on a remote SR. *)
    let update_snapshot_info_src =
      let url_p = Param.mk ~name:"url" Types.string in
      let dest_p = Param.mk ~name:"dest" Sr.t in
      let dest_vdi_p = Param.mk ~name:"dest_vdi" Vdi.t in
      let snapshot_pairs_p =
        Param.mk ~name:"snapshot_pairs"
          TypeCombinators.(list (pair (Vdi.t, Vdi.t)))
      in
      let verify_dest_p =
        Param.mk ~name:"verify_dest"
          ~description:["when true, verify remote server certificate"]
          Types.bool
      in
      declare "SR.update_snapshot_info_src" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> url_p
        @-> dest_p
        @-> dest_vdi_p
        @-> snapshot_pairs_p
        @-> verify_dest_p
        @-> returning unit_p err
        )

    (** [update_snapshot_info_dest sr vdi dest src_vdi snapshot_pairs] updates
        the fields is_a_snapshot, snapshot_time and snapshot_of for a list of
        snapshots on a local SR. Typically, vdi will be a mirror of src_vdi,
        and for each item in snapshot_pairs the first will be a copy of the
        second. *)
    let update_snapshot_info_dest =
      let src_vdi_p = Param.mk ~name:"src_vdi" vdi_info in
      let snapshot_pairs_p =
        Param.mk ~name:"snapshot_pairs"
          TypeCombinators.(list (pair (Vdi.t, vdi_info)))
      in
      declare "SR.update_snapshot_info_dest" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> src_vdi_p
        @-> snapshot_pairs_p
        @-> returning unit_p err
        )

    (** [stat task sr] returns instantaneous SR-level statistics *)
    let stat =
      let result_p = Param.mk ~name:"result" sr_info in
      declare "SR.stat" [] (dbg_p @-> sr_p @-> returning result_p err)

    (** [list task] returns the list of currently attached SRs *)
    let list =
      let result_p = Param.mk ~name:"result" TypeCombinators.(list Sr.t) in
      declare "SR.list" [] (dbg_p @-> returning result_p err)
  end

  module VDI = struct
    (** Functions which operate on particular VDIs. These functions are all
        idempotent from the point of view of a given [dp]. *)

    let vdi_info_p = Param.mk ~name:"vdi_info" vdi_info

    (** [create task sr vdi_info] creates a new VDI in [sr] using [vdi_info].
        Some fields in the [vdi_info] may be modified (e.g. rounded up), so the
        function returns the vdi_info which was used. *)
    let create =
      declare "VDI.create" []
        (dbg_p @-> sr_p @-> vdi_info_p @-> returning vdi_info_p err)

    (** [set_name_label sr vdi new_name_label] updates the name_label of VDI
        [vdi] in SR [sr]. *)
    let set_name_label =
      declare "VDI.set_name_label" []
        (dbg_p @-> sr_p @-> vdi_p @-> new_name_label_p @-> returning unit_p err)

    (** [set_name_description sr vdi new_name_description] updates the
        name_description of VDI [vdi] in SR [sr]. *)
    let set_name_description =
      declare "VDI.set_name_description" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> new_name_description_p
        @-> returning unit_p err
        )

    (** [snapshot task sr vdi_info] creates a new VDI which is a snapshot of
        [vdi_info] in [sr] *)
    let snapshot =
      declare "VDI.snapshot" []
        (dbg_p @-> sr_p @-> vdi_info_p @-> returning vdi_info_p err)

    (** [clone task sr vdi_info] creates a new VDI which is a clone of
        [vdi_info] in [sr] *)
    let clone =
      declare "VDI.clone" []
        (dbg_p @-> sr_p @-> vdi_info_p @-> returning vdi_info_p err)

    (** [resize task sr vdi new_size] makes a VDI's virtual_size at least
        [new_size] bytes. The function returns the new virtual_size which may be
        bigger (but not less than) requested. *)
    let resize =
      let new_size_p = Param.mk ~name:"new_size" Types.int64 in
      declare "VDI.resize" []
        (dbg_p @-> sr_p @-> vdi_p @-> new_size_p @-> returning new_size_p err)

    (** [destroy task sr vdi] removes [vdi] from [sr] *)
    let destroy =
      declare "VDI.destroy" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p err)

    (** [stat dbg sr vdi] returns information about VDI [vdi] in SR [sr] *)
    let stat =
      declare "VDI.stat" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning vdi_info_p err)

    (** [introduce dbg sr uuid sm_config location] checks that a VDI exists and
        returns info about it *)
    let introduce =
      let uuid_p = Param.mk ~name:"uuid" Types.string in
      let location_p = Param.mk ~name:"location" Types.string in
      declare "VDI.introduce" []
        (dbg_p
        @-> sr_p
        @-> uuid_p
        @-> sm_config_p
        @-> location_p
        @-> returning vdi_info_p err
        )

    let persistent_p = Param.mk ~name:"persistent" Types.bool

    (** [set_persistent dbg sr vdi persistent] sets [vdi]'s persistent flag to
        [persistent] *)
    let set_persistent =
      declare "VDI.set_persistent" []
        (dbg_p @-> sr_p @-> vdi_p @-> persistent_p @-> returning unit_p err)

    (** [epoch_begin sr vdi persistent] declares that [vdi] is about to be added
        to a starting/rebooting VM. This is not called over suspend/resume or
        migrate. If [persistent] is false, then changes to the disk will be
        erased when the VM shuts down. *)
    let epoch_begin =
      declare "VDI.epoch_begin" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> persistent_p
        @-> returning unit_p err
        )

    let read_write_p = Param.mk ~name:"read_write" Types.bool

    (** [attach task dp sr vdi read_write] returns the [params] for a given
        [vdi] in [sr] which can be written to if (but not necessarily only if)
        [read_write] is true. @deprecated This function is deprecated, and is
        only here to keep backward compatibility with old xapis that call
        Remote.VDI.attach during SXM. Use the attach3 function instead. *)
    let attach =
      let attach_info_p = Param.mk ~name:"attach_info" attach_info in
      declare "VDI.attach" []
        (dbg_p
        @-> dp_p
        @-> sr_p
        @-> vdi_p
        @-> read_write_p
        @-> returning attach_info_p err
        )

    (** [attach2 task dp sr vdi read_write] returns the [params] for a given
        [vdi] in [sr] which can be written to if (but not necessarily only if)
        [read_write] is true. @deprecated This function is deprecated, and is
        only here to keep backward compatibility with old xapis that call
        Remote.VDI.attach2 during SXM. Use the attach3 function instead. *)
    let attach2 =
      let backend_p = Param.mk ~name:"backend" backend in
      declare "VDI.attach2" []
        (dbg_p
        @-> dp_p
        @-> sr_p
        @-> vdi_p
        @-> read_write_p
        @-> returning backend_p err
        )

    (** [attach3 task dp sr vdi vm read_write] returns the [params] for a given
        [vdi] in [sr] which can be written to if (but not necessarily only if)
        [read_write] is true *)
    let attach3 =
      let backend_p = Param.mk ~name:"backend" backend in
      declare "VDI.attach3" []
        (dbg_p
        @-> dp_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> read_write_p
        @-> returning backend_p err
        )

    (** [activate task dp sr vdi] signals the desire to immediately use [vdi].
        This client must have called [attach] on the [vdi] first. @deprecated
        This function is deprecated, and is only here to keep backward
        compatibility with old xapis that call Remote.VDI.activate during SXM.
        Use the activate3 function instead. *)
    let activate =
      declare "VDI.activate" []
        (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> returning unit_p err)

    (** [activate3 task dp sr vdi] signals the desire to immediately use [vdi].
        This client must have called [attach] on the [vdi] first. *)
    let activate3 =
      declare "VDI.activate3" []
        (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> vm_p @-> returning unit_p err)

    (** [activate_readonly task dp sr vdi] signals the desire to immediately use [vdi].
        This client must have called [attach] on the [vdi] first. *)
    let activate_readonly =
      declare "VDI.activate_readonly" []
        (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> vm_p @-> returning unit_p err)

    (** [deactivate task dp sr vdi] signals that this client has stopped reading
        (and writing) [vdi]. *)
    let deactivate =
      declare "VDI.deactivate" []
        (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> vm_p @-> returning unit_p err)

    (** [detach task dp sr vdi] signals that this client no-longer needs the
        [attach_info] to be valid. *)
    let detach =
      declare "VDI.detach" []
        (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> vm_p @-> returning unit_p err)

    (** [epoch_end sr vdi] declares that [vdi] is about to be removed from a
        shutting down/rebooting VM. This is not called over suspend/resume or
        migrate. *)
    let epoch_end =
      declare "VDI.epoch_end" []
        (dbg_p @-> sr_p @-> vdi_p @-> vm_p @-> returning unit_p err)

    (** [get_url task sr vdi] returns a URL suitable for accessing disk data
        directly. *)
    let get_url =
      let result_p = Param.mk ~name:"url" Types.string in
      declare "VDI.get_url" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning result_p err)

    (** [similar_content task sr vdi] returns a list of VDIs which have similar
        content to [vdi] *)
    let similar_content =
      let result = Param.mk ~name:"vdis" TypeCombinators.(list vdi_info) in
      declare "VDI.similar_content" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning result err)

    (** [get_by_name task sr name] returns the vdi within [sr] with [name] *)
    let get_by_name =
      let name_p = Param.mk ~name:"name" Types.string in
      declare "VDI.get_by_name" []
        (dbg_p @-> sr_p @-> name_p @-> returning vdi_info_p err)

    (** [set_content_id task sr vdi content_id] tells the storage backend that a
        VDI has an updated [content_id] *)
    let set_content_id =
      let content_id_p = Param.mk ~name:"content_id" content_id in
      declare "VDI.set_content_id" []
        (dbg_p @-> sr_p @-> vdi_p @-> content_id_p @-> returning unit_p err)

    (** [compose task sr parent child] layers the updates from [child] onto [parent],
        modifying [child] *)
    let compose =
      let vdi1_p = Param.mk ~name:"vdi1" Vdi.t in
      let vdi2_p = Param.mk ~name:"vdi2" Vdi.t in
      declare "VDI.compose" []
        (dbg_p @-> sr_p @-> vdi1_p @-> vdi2_p @-> returning unit_p err)

    let key_p = Param.mk ~name:"key" Types.string

    let value_p = Param.mk ~name:"value" Types.string

    (** [add_to_sm_config dbg sr vdi key value] associates [value] to the [key]
        in [vdi] sm-config *)
    let add_to_sm_config =
      declare "VDI.add_to_sm_config" []
        (dbg_p @-> sr_p @-> vdi_p @-> key_p @-> value_p @-> returning unit_p err)

    (** [remove_from_sm_config dbg sr vdi key] remove [key] from [vdi] sm-config *)
    let remove_from_sm_config =
      declare "VDI.remove_from_sm_config" []
        (dbg_p @-> sr_p @-> vdi_p @-> key_p @-> returning unit_p err)

    (** [enable_cbt dbg sr vdi] enables changed block tracking for [vdi] *)
    let enable_cbt =
      declare "VDI.enable_cbt" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p err)

    (** [disable_cbt dbg sr vdi] disables changed block tracking for [vdi] *)
    let disable_cbt =
      declare "VDI.disable_cbt" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p err)

    (** [data_destroy dbg sr vdi] deletes the data of the snapshot [vdi] without
        deleting its changed block tracking metadata *)
    let data_destroy =
      declare "VDI.data_destroy" []
        (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p err)

    (** [list_changed_blocks dbg sr vdi_from vdi_to] returns the blocks that
        have changed between [vdi_from] and [vdi_to] as a base64-encoded bitmap
        string *)
    let list_changed_blocks =
      let vdi_from_p = Param.mk ~name:"vdi_from" Vdi.t in
      let vdi_to_p = Param.mk ~name:"vdi_tp" Vdi.t in
      let result_p = Param.mk ~name:"changed_blocks" Types.string in
      declare "VDI.list_changed_blocks" []
        (dbg_p @-> sr_p @-> vdi_from_p @-> vdi_to_p @-> returning result_p err)
  end

  (** [get_by_name task name] returns a vdi with [name] (which may be in any SR) *)
  let get_by_name =
    let name_p = Param.mk ~name:"name" Types.string in
    let result_p =
      Param.mk ~name:"result" TypeCombinators.(pair (Sr.t, vdi_info))
    in
    declare "get_by_name" [] (dbg_p @-> name_p @-> returning result_p err)

  module DATA = struct
    let url_p = Param.mk ~name:"url" Types.string

    let dest_p = Param.mk ~name:"dest" Sr.t

    let task_id_p = Param.mk ~name:"task_id" Task.id

    let verify_dest_p =
      Param.mk ~name:"verify_dest"
        ~description:["when true, verify remote server certificate"]
        Types.bool

    let copy =
      let result_p = Param.mk ~name:"task_id" Task.id in
      declare "DATA.copy" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> url_p
        @-> dest_p
        @-> verify_dest_p
        @-> returning result_p err
        )

    let operation_p = Param.mk ~name:"operation" Mirror.operation

    let mirror =
      declare "DATA.mirror" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> url_p
        @-> returning operation_p err
        )

    let stat =
      let status_p = Param.mk ~name:"status" Mirror.status in
      declare "DATA.stat" []
        (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> operation_p
        @-> returning status_p err
        )

    (** [import_activate dbg dp sr vdi vm] returns a server socket address to 
      which a fd can be passed via SCM_RIGHTS for mirroring purposes.*)
    let import_activate =
      declare "DATA.import_activate" []
        (dbg_p
        @-> dp_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> returning sock_path_p err
        )

    (** [get_nbd_server dbg dp sr vdi vm] returns the address of a generic nbd
      server that can be connected to. Depending on the backend, this will either
      be a nbd server backed by tapdisk or qemu-dp. Note this is different 
      from [import_activate] as the returned server does not accept fds. *)
    let get_nbd_server =
      declare "DATA.get_nbd_server" []
        (dbg_p
        @-> dp_p
        @-> sr_p
        @-> vdi_p
        @-> vm_p
        @-> returning sock_path_p err
        )

    module MIRROR = struct
      let mirror_vm_p = Param.mk ~name:"mirror_vm" Vm.t

      let copy_vm_p = Param.mk ~name:"copy_vm" Vm.t

      let live_vm_p = Param.mk ~name:"live_vm" Vm.t

      let id_p = Param.mk ~name:"id" Mirror.id

      (** [send_start dbg dp task src_sr vdi mirror_vm mirror_id local_vdi copy_vm 
      live_vm url remote_mirror dest_sr verify_dest]
      takes the remote mirror [remote_mirror] prepared by the destination host 
      and initiates the mirroring of [vdi] from the source *)
      let send_start =
        let recv_result_p =
          Param.mk ~name:"recv_result" Mirror.mirror_receive_result
        in
        let local_vdi_p = Param.mk ~name:"local_vdi" vdi_info in
        let src_sr_p = Param.mk ~name:"src_sr" Sr.t in
        let dest_sr_p = Param.mk ~name:"dest_sr" Sr.t in
        declare "DATA.MIRROR.send_start" []
          (dbg_p
          @-> dp_p
          @-> task_id_p
          @-> src_sr_p
          @-> vdi_p
          @-> mirror_vm_p
          @-> id_p
          @-> local_vdi_p
          @-> copy_vm_p
          @-> live_vm_p
          @-> url_p
          @-> recv_result_p
          @-> dest_sr_p
          @-> verify_dest_p
          @-> returning unit_p err
          )

      (** Called on the receiving end 
        @deprecated This function is deprecated, and is only here to keep backward 
        compatibility with old xapis that call Remote.DATA.MIRROR.receive_start during SXM. 
        Use the receive_start3 function instead. 
      *)
      let receive_start =
        let similar_p = Param.mk ~name:"similar" Mirror.similars in
        let result = Param.mk ~name:"result" Mirror.mirror_receive_result in
        declare "DATA.MIRROR.receive_start" []
          (dbg_p
          @-> sr_p
          @-> VDI.vdi_info_p
          @-> id_p
          @-> similar_p
          @-> returning result err
          )

      (** Called on the receiving end 
        @deprecated This function is deprecated, and is only here to keep backward 
        compatibility with old xapis that call Remote.DATA.MIRROR.receive_start2 during SXM. 
        Use the receive_start3 function instead. 
      *)
      let receive_start2 =
        let similar_p = Param.mk ~name:"similar" Mirror.similars in
        let result = Param.mk ~name:"result" Mirror.mirror_receive_result in
        declare "DATA.MIRROR.receive_start2" []
          (dbg_p
          @-> sr_p
          @-> VDI.vdi_info_p
          @-> id_p
          @-> similar_p
          @-> vm_p
          @-> returning result err
          )

      (** Called on the receiving end to prepare for receipt of the storage. This
      function should be used in conjunction with [receive_finalize3]*)
      let receive_start3 =
        let similar_p = Param.mk ~name:"similar" Mirror.similars in
        let result = Param.mk ~name:"result" Mirror.mirror_receive_result in
        declare "DATA.MIRROR.receive_start3" []
          (dbg_p
          @-> sr_p
          @-> VDI.vdi_info_p
          @-> id_p
          @-> similar_p
          @-> vm_p
          @-> url_p
          @-> verify_dest_p
          @-> returning result err
          )

      (** Called on the receiving end 
        @deprecated This function is deprecated, and is only here to keep backward 
        compatibility with old xapis that call Remote.DATA.MIRROR.receive_finalize
        during SXM.  Use the receive_finalize3 function instead. 
      *)
      let receive_finalize =
        declare "DATA.MIRROR.receive_finalize" []
          (dbg_p @-> id_p @-> returning unit_p err)

      (** Called on the receiving end 
        @deprecated This function is deprecated, and is only here to keep backward 
        compatibility with old xapis that call Remote.DATA.MIRROR.receive_finalize2
        during SXM.  Use the receive_finalize3 function instead. 
      *)
      let receive_finalize2 =
        declare "DATA.MIRROR.receive_finalize2" []
          (dbg_p @-> id_p @-> returning unit_p err)

      (** [receive_finalize3 dbg id] will stop the mirroring process and compose 
      the snapshot VDI with the mirror VDI. It also cleans up the storage resources 
      used by mirroring. It is called after the the source VM is paused. This fucntion
      should be used in conjunction with [receive_start3] *)
      let receive_finalize3 =
        declare "DATA.MIRROR.receive_finalize3" []
          (dbg_p
          @-> id_p
          @-> sr_p
          @-> url_p
          @-> verify_dest_p
          @-> returning unit_p err
          )

      (** [receive_cancel dbg id] is called in the case of migration failure to
      do the clean up.
        @deprecated This function is deprecated, and is only here to keep backward 
        compatibility with old xapis that call Remote.DATA.MIRROR.receive_cancel
        during SXM.  Use the receive_cancel2 function instead. 
      *)
      let receive_cancel =
        declare "DATA.MIRROR.receive_cancel" []
          (dbg_p @-> id_p @-> returning unit_p err)

      (** [receive_cancel2 dbg mirror_id url verify_dest] cleans up the side effects
      done by [receive_start3] on the destination host when the migration fails. *)
      let receive_cancel2 =
        declare "DATA.MIRROR.receive_cancel2" []
          (dbg_p @-> id_p @-> url_p @-> verify_dest_p @-> returning unit_p err)

      let pre_deactivate_hook =
        declare "DATA.MIRROR.pre_deactivate_hook" []
          (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> returning unit_p err)

      let has_mirror_failed =
        let mirror_failed_p =
          Param.mk ~name:"mirror_failed_p" ~description:[] Types.bool
        in
        declare "DATA.MIRROR.has_mirror_failed" []
          (dbg_p @-> id_p @-> sr_p @-> returning mirror_failed_p err)

      let list =
        let result_p =
          Param.mk ~name:"mirrors" TypeCombinators.(list (pair Mirror.(id, t)))
        in
        declare "DATA.MIRROR.list" [] (dbg_p @-> returning result_p err)

      let stat =
        let result_p = Param.mk ~name:"result" Mirror.t in
        declare "DATA.MIRROR.stat" [] (dbg_p @-> id_p @-> returning result_p err)
    end
  end

  module Policy = struct
    let get_backend_vm =
      let vm_p = Param.mk ~name:"vm" Types.string in
      let result_p = Param.mk ~name:"result" Types.string in
      declare "Policy.get_backend_vm" []
        (dbg_p @-> vm_p @-> sr_p @-> vdi_p @-> returning result_p err)
  end

  module TASK = struct
    let task_p = Param.mk ~name:"task" Task.id

    let stat =
      let result_p = Param.mk ~name:"result" Task.t in
      declare "TASK.stat" [] (dbg_p @-> task_p @-> returning result_p err)

    let cancel =
      declare "TASK.cancel" [] (dbg_p @-> task_p @-> returning unit_p err)

    let destroy =
      declare "TASK.destroy" [] (dbg_p @-> task_p @-> returning unit_p err)

    let list =
      let result_p = Param.mk ~name:"tasks" TypeCombinators.(list Task.t) in
      declare "TASK.list" [] (dbg_p @-> returning result_p err)
  end

  module UPDATES = struct
    let get =
      let from_p = Param.mk ~name:"from" Types.string in
      let timeout_p =
        Param.mk ~name:"timeout" TypeCombinators.(option Types.int)
      in
      let result_p =
        Param.mk ~name:"updates"
          TypeCombinators.(pair (list Dynamic.id, Types.string))
      in
      declare "UPDATES.get" []
        (dbg_p @-> from_p @-> timeout_p @-> returning result_p err)
  end
end

module type MIRROR = sig
  type context = unit

  val send_start :
       context
    -> dbg:debug_info
    -> task_id:Task.id
    -> dp:dp
    -> sr:sr
    -> vdi:vdi
    -> mirror_vm:vm
    -> mirror_id:Mirror.id
    -> local_vdi:vdi_info
    -> copy_vm:vm
    -> live_vm:vm
    -> url:string
    -> remote_mirror:Mirror.mirror_receive_result
    -> dest_sr:sr
    -> verify_dest:bool
    -> unit

  val receive_start :
       context
    -> dbg:debug_info
    -> sr:sr
    -> vdi_info:vdi_info
    -> id:Mirror.id
    -> similar:Mirror.similars
    -> Mirror.mirror_receive_result

  val receive_start2 :
       context
    -> dbg:debug_info
    -> sr:sr
    -> vdi_info:vdi_info
    -> id:Mirror.id
    -> similar:Mirror.similars
    -> vm:vm
    -> Mirror.mirror_receive_result

  val receive_start3 :
       context
    -> dbg:debug_info
    -> sr:sr
    -> vdi_info:vdi_info
    -> mirror_id:Mirror.id
    -> similar:Mirror.similars
    -> vm:vm
    -> url:string
    -> verify_dest:bool
    -> Mirror.mirror_receive_result

  val receive_finalize : context -> dbg:debug_info -> id:Mirror.id -> unit

  val receive_finalize2 : context -> dbg:debug_info -> id:Mirror.id -> unit

  val receive_finalize3 :
       context
    -> dbg:debug_info
    -> mirror_id:Mirror.id
    -> sr:sr
    -> url:string
    -> verify_dest:bool
    -> unit

  val receive_cancel : context -> dbg:debug_info -> id:Mirror.id -> unit

  val receive_cancel2 :
       context
    -> dbg:debug_info
    -> mirror_id:Mirror.id
    -> url:string
    -> verify_dest:bool
    -> unit

  val pre_deactivate_hook :
    context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit

  val has_mirror_failed :
    context -> dbg:debug_info -> mirror_id:Mirror.id -> sr:Sr.t -> bool

  val list : context -> dbg:debug_info -> (Mirror.id * Mirror.t) list

  val stat : context -> dbg:debug_info -> id:Mirror.id -> Mirror.t
end

module type Server_impl = sig
  type context = unit

  module Query : sig
    val query : context -> dbg:string -> query_result

    val diagnostics : context -> dbg:string -> string
  end

  module DP : sig
    val create : context -> dbg:debug_info -> id:string -> dp

    val destroy : context -> dbg:debug_info -> dp:dp -> allow_leak:bool -> unit

    val destroy2 :
         context
      -> dbg:debug_info
      -> dp:dp
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> allow_leak:bool
      -> unit

    val attach_info :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> vm:vm -> backend

    val diagnostics : context -> unit -> string

    val stat_vdi :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit -> dp_stat_t
  end

  module SR : sig
    val create :
         context
      -> dbg:debug_info
      -> sr:sr
      -> name_label:string
      -> name_description:string
      -> device_config:(string * string) list
      -> physical_size:int64
      -> (string * string) list

    val set_name_label :
      context -> dbg:debug_info -> sr:sr -> new_name_label:string -> unit

    val set_name_description :
      context -> dbg:debug_info -> sr:sr -> new_name_description:string -> unit

    val probe :
         context
      -> dbg:debug_info
      -> queue:string
      -> device_config:(string * string) list
      -> sm_config:(string * string) list
      -> probe_result

    val attach :
         context
      -> dbg:debug_info
      -> sr:sr
      -> device_config:(string * string) list
      -> unit

    val detach : context -> dbg:debug_info -> sr:sr -> unit

    val reset : context -> dbg:debug_info -> sr:sr -> unit

    val destroy : context -> dbg:debug_info -> sr:sr -> unit

    val scan : context -> dbg:debug_info -> sr:sr -> vdi_info list

    val scan2 : context -> dbg:debug_info -> sr:sr -> vdi_info list * sr_info

    val update_snapshot_info_src :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> url:string
      -> dest:sr
      -> dest_vdi:vdi
      -> snapshot_pairs:(vdi * vdi) list
      -> verify_dest:bool
      -> unit

    val update_snapshot_info_dest :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> src_vdi:vdi_info
      -> snapshot_pairs:(vdi * vdi_info) list
      -> unit

    val stat : context -> dbg:debug_info -> sr:sr -> sr_info

    val list : context -> dbg:debug_info -> sr list
  end

  module VDI : sig
    val create :
      context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info

    val set_name_label :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> new_name_label:string
      -> unit

    val set_name_description :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> new_name_description:string
      -> unit

    val snapshot :
      context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info

    val clone :
      context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info

    val resize :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> new_size:int64 -> int64

    val destroy : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit

    val stat : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info

    val introduce :
         context
      -> dbg:debug_info
      -> sr:sr
      -> uuid:string
      -> sm_config:(string * string) list
      -> location:string
      -> vdi_info

    val set_persistent :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit

    val epoch_begin :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> persistent:bool
      -> unit

    val attach :
         context
      -> dbg:debug_info
      -> dp:dp
      -> sr:sr
      -> vdi:vdi
      -> read_write:bool
      -> attach_info

    val attach2 :
         context
      -> dbg:debug_info
      -> dp:dp
      -> sr:sr
      -> vdi:vdi
      -> read_write:bool
      -> backend

    val attach3 :
         context
      -> dbg:debug_info
      -> dp:dp
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> read_write:bool
      -> backend

    val activate :
      context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit

    val activate3 :
      context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> vm:vm -> unit

    val activate_readonly :
      context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> vm:vm -> unit

    val deactivate :
      context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> vm:vm -> unit

    val detach :
      context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> vm:vm -> unit

    val epoch_end :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vm:vm -> unit

    val get_url : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> string

    val similar_content :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info list

    val get_by_name :
      context -> dbg:debug_info -> sr:sr -> name:string -> vdi_info

    val set_content_id :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> content_id:content_id
      -> unit

    val compose :
      context -> dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> unit

    val add_to_sm_config :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> key:string
      -> value:string
      -> unit

    val remove_from_sm_config :
      context -> dbg:debug_info -> sr:sr -> vdi:vdi -> key:string -> unit

    val enable_cbt : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit

    val disable_cbt : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit

    val data_destroy : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit

    val list_changed_blocks :
      context -> dbg:debug_info -> sr:sr -> vdi_from:vdi -> vdi_to:vdi -> string
  end

  val get_by_name : context -> dbg:debug_info -> name:string -> sr * vdi_info

  module DATA : sig
    val copy :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> url:string
      -> dest:sr
      -> verify_dest:bool
      -> Task.id

    val mirror :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> dest:string
      -> operation

    val stat :
         context
      -> dbg:debug_info
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> key:operation
      -> status

    val import_activate :
         context
      -> dbg:debug_info
      -> dp:dp
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> sock_path

    val get_nbd_server :
         context
      -> dbg:debug_info
      -> dp:dp
      -> sr:sr
      -> vdi:vdi
      -> vm:vm
      -> sock_path

    module MIRROR : MIRROR
  end

  module Policy : sig
    val get_backend_vm :
      context -> dbg:debug_info -> vm:string -> sr:sr -> vdi:vdi -> string
  end

  module TASK : sig
    val stat : context -> dbg:debug_info -> task:Task.id -> Task.t

    val cancel : context -> dbg:debug_info -> task:Task.id -> unit

    val destroy : context -> dbg:debug_info -> task:Task.id -> unit

    val list : context -> dbg:debug_info -> Task.t list
  end

  module UPDATES : sig
    val get :
         context
      -> dbg:debug_info
      -> from:string
      -> timeout:int option
      -> Dynamic.id list * string
  end
end

module Server (Impl : Server_impl) () = struct
  module S = StorageAPI (Idl.Exn.GenServer ())

  let _ =
    S.Query.query (fun dbg -> Impl.Query.query () ~dbg) ;
    S.Query.diagnostics (fun dbg -> Impl.Query.diagnostics () ~dbg) ;
    S.DP.create (fun dbg id -> Impl.DP.create () ~dbg ~id) ;
    S.DP.destroy (fun dbg dp allow_leak ->
        Impl.DP.destroy () ~dbg ~dp ~allow_leak
    ) ;
    S.DP.destroy2 (fun dbg dp sr vdi vm allow_leak ->
        Impl.DP.destroy2 () ~dbg ~dp ~sr ~vdi ~vm ~allow_leak
    ) ;
    S.DP.attach_info (fun dbg sr vdi dp vm ->
        Impl.DP.attach_info () ~dbg ~sr ~vdi ~dp ~vm
    ) ;
    S.DP.diagnostics (fun () -> Impl.DP.diagnostics () ()) ;
    S.DP.stat_vdi (fun dbg sr vdi () -> Impl.DP.stat_vdi () ~dbg ~sr ~vdi ()) ;
    S.SR.create
      (fun dbg sr name_label name_description device_config physical_size ->
        Impl.SR.create () ~dbg ~sr ~name_label ~name_description ~device_config
          ~physical_size
    ) ;
    S.SR.set_name_label (fun dbg sr new_name_label ->
        Impl.SR.set_name_label () ~dbg ~sr ~new_name_label
    ) ;
    S.SR.set_name_description (fun dbg sr new_name_description ->
        Impl.SR.set_name_description () ~dbg ~sr ~new_name_description
    ) ;
    S.SR.probe (fun dbg queue device_config sm_config ->
        Impl.SR.probe () ~dbg ~queue ~device_config ~sm_config
    ) ;
    S.SR.attach (fun dbg sr device_config ->
        Impl.SR.attach () ~dbg ~sr ~device_config
    ) ;
    S.SR.detach (fun dbg sr -> Impl.SR.detach () ~dbg ~sr) ;
    S.SR.reset (fun dbg sr -> Impl.SR.reset () ~dbg ~sr) ;
    S.SR.destroy (fun dbg sr -> Impl.SR.destroy () ~dbg ~sr) ;
    S.SR.scan (fun dbg sr -> Impl.SR.scan () ~dbg ~sr) ;
    S.SR.scan2 (fun dbg sr -> Impl.SR.scan2 () ~dbg ~sr) ;
    S.SR.update_snapshot_info_src
      (fun dbg sr vdi url dest dest_vdi snapshot_pairs verify_dest ->
        Impl.SR.update_snapshot_info_src () ~dbg ~sr ~vdi ~url ~dest ~dest_vdi
          ~snapshot_pairs ~verify_dest
    ) ;
    S.SR.update_snapshot_info_dest (fun dbg sr vdi src_vdi snapshot_pairs ->
        Impl.SR.update_snapshot_info_dest () ~dbg ~sr ~vdi ~src_vdi
          ~snapshot_pairs
    ) ;
    S.SR.stat (fun dbg sr -> Impl.SR.stat () ~dbg ~sr) ;
    S.SR.list (fun dbg -> Impl.SR.list () ~dbg) ;
    S.VDI.create (fun dbg sr vdi_info -> Impl.VDI.create () ~dbg ~sr ~vdi_info) ;
    S.VDI.set_name_label (fun dbg sr vdi new_name_label ->
        Impl.VDI.set_name_label () ~dbg ~sr ~vdi ~new_name_label
    ) ;
    S.VDI.set_name_description (fun dbg sr vdi new_name_description ->
        Impl.VDI.set_name_description () ~dbg ~sr ~vdi ~new_name_description
    ) ;
    S.VDI.snapshot (fun dbg sr vdi_info ->
        Impl.VDI.snapshot () ~dbg ~sr ~vdi_info
    ) ;
    S.VDI.clone (fun dbg sr vdi_info -> Impl.VDI.clone () ~dbg ~sr ~vdi_info) ;
    S.VDI.resize (fun dbg sr vdi new_size ->
        Impl.VDI.resize () ~dbg ~sr ~vdi ~new_size
    ) ;
    S.VDI.destroy (fun dbg sr vdi -> Impl.VDI.destroy () ~dbg ~sr ~vdi) ;
    S.VDI.stat (fun dbg sr vdi -> Impl.VDI.stat () ~dbg ~sr ~vdi) ;
    S.VDI.introduce (fun dbg sr uuid sm_config location ->
        Impl.VDI.introduce () ~dbg ~sr ~uuid ~sm_config ~location
    ) ;
    S.VDI.set_persistent (fun dbg sr vdi persistent ->
        Impl.VDI.set_persistent () ~dbg ~sr ~vdi ~persistent
    ) ;
    S.VDI.epoch_begin (fun dbg sr vdi vm persistent ->
        Impl.VDI.epoch_begin () ~dbg ~sr ~vdi ~vm ~persistent
    ) ;
    S.VDI.attach (fun dbg dp sr vdi read_write ->
        Impl.VDI.attach () ~dbg ~dp ~sr ~vdi ~read_write
    ) ;
    S.VDI.attach2 (fun dbg dp sr vdi read_write ->
        Impl.VDI.attach2 () ~dbg ~dp ~sr ~vdi ~read_write
    ) ;
    S.VDI.attach3 (fun dbg dp sr vdi vm read_write ->
        Impl.VDI.attach3 () ~dbg ~dp ~sr ~vdi ~vm ~read_write
    ) ;
    S.VDI.activate (fun dbg dp sr vdi -> Impl.VDI.activate () ~dbg ~dp ~sr ~vdi) ;
    S.VDI.activate3 (fun dbg dp sr vdi vm ->
        Impl.VDI.activate3 () ~dbg ~dp ~sr ~vdi ~vm
    ) ;
    S.VDI.activate_readonly (fun dbg dp sr vdi vm ->
        Impl.VDI.activate_readonly () ~dbg ~dp ~sr ~vdi ~vm
    ) ;
    S.VDI.deactivate (fun dbg dp sr vdi vm ->
        Impl.VDI.deactivate () ~dbg ~dp ~sr ~vdi ~vm
    ) ;
    S.VDI.detach (fun dbg dp sr vdi vm ->
        Impl.VDI.detach () ~dbg ~dp ~sr ~vdi ~vm
    ) ;
    S.VDI.epoch_end (fun dbg sr vdi vm ->
        Impl.VDI.epoch_end () ~dbg ~sr ~vdi ~vm
    ) ;
    S.VDI.get_url (fun dbg sr vdi -> Impl.VDI.get_url () ~dbg ~sr ~vdi) ;
    S.VDI.similar_content (fun dbg sr vdi ->
        Impl.VDI.similar_content () ~dbg ~sr ~vdi
    ) ;
    S.VDI.get_by_name (fun dbg sr name -> Impl.VDI.get_by_name () ~dbg ~sr ~name) ;
    S.VDI.set_content_id (fun dbg sr vdi content_id ->
        Impl.VDI.set_content_id () ~dbg ~sr ~vdi ~content_id
    ) ;
    S.VDI.compose (fun dbg sr vdi1 vdi2 ->
        Impl.VDI.compose () ~dbg ~sr ~vdi1 ~vdi2
    ) ;
    S.VDI.add_to_sm_config (fun dbg sr vdi key value ->
        Impl.VDI.add_to_sm_config () ~dbg ~sr ~vdi ~key ~value
    ) ;
    S.VDI.remove_from_sm_config (fun dbg sr vdi key ->
        Impl.VDI.remove_from_sm_config () ~dbg ~sr ~vdi ~key
    ) ;
    S.VDI.enable_cbt (fun dbg sr vdi -> Impl.VDI.enable_cbt () ~dbg ~sr ~vdi) ;
    S.VDI.disable_cbt (fun dbg sr vdi -> Impl.VDI.disable_cbt () ~dbg ~sr ~vdi) ;
    S.VDI.data_destroy (fun dbg sr vdi -> Impl.VDI.data_destroy () ~dbg ~sr ~vdi) ;
    S.VDI.list_changed_blocks (fun dbg sr vdi_from vdi_to ->
        Impl.VDI.list_changed_blocks () ~dbg ~sr ~vdi_from ~vdi_to
    ) ;
    S.get_by_name (fun dbg name -> Impl.get_by_name () ~dbg ~name) ;
    S.DATA.copy (fun dbg sr vdi vm url dest verify_dest ->
        Impl.DATA.copy () ~dbg ~sr ~vdi ~vm ~url ~dest ~verify_dest
    ) ;
    S.DATA.mirror (fun dbg sr vdi vm dest ->
        Impl.DATA.mirror () ~dbg ~sr ~vdi ~vm ~dest
    ) ;
    S.DATA.stat (fun dbg sr vdi vm key ->
        Impl.DATA.stat () ~dbg ~sr ~vdi ~vm ~key
    ) ;
    S.DATA.MIRROR.send_start
      (fun
        dbg
        task_id
        dp
        sr
        vdi
        mirror_vm
        mirror_id
        local_vdi
        copy_vm
        live_vm
        url
        remote_mirror
        dest_sr
        verify_dest
      ->
        Impl.DATA.MIRROR.send_start () ~dbg ~task_id ~dp ~sr ~vdi ~mirror_vm
          ~mirror_id ~local_vdi ~copy_vm ~live_vm ~url ~remote_mirror ~dest_sr
          ~verify_dest
    ) ;
    S.DATA.MIRROR.receive_start (fun dbg sr vdi_info id similar ->
        Impl.DATA.MIRROR.receive_start () ~dbg ~sr ~vdi_info ~id ~similar
    ) ;
    S.DATA.MIRROR.receive_start2 (fun dbg sr vdi_info id similar vm ->
        Impl.DATA.MIRROR.receive_start2 () ~dbg ~sr ~vdi_info ~id ~similar ~vm
    ) ;
    S.DATA.MIRROR.receive_start3
      (fun dbg sr vdi_info mirror_id similar vm url verify_dest ->
        Impl.DATA.MIRROR.receive_start3 () ~dbg ~sr ~vdi_info ~mirror_id
          ~similar ~vm ~url ~verify_dest
    ) ;
    S.DATA.MIRROR.receive_cancel (fun dbg id ->
        Impl.DATA.MIRROR.receive_cancel () ~dbg ~id
    ) ;
    S.DATA.MIRROR.receive_cancel2 (fun dbg mirror_id url verify_dest ->
        Impl.DATA.MIRROR.receive_cancel2 () ~dbg ~mirror_id ~url ~verify_dest
    ) ;
    S.DATA.MIRROR.receive_finalize (fun dbg id ->
        Impl.DATA.MIRROR.receive_finalize () ~dbg ~id
    ) ;
    S.DATA.MIRROR.receive_finalize2 (fun dbg id ->
        Impl.DATA.MIRROR.receive_finalize2 () ~dbg ~id
    ) ;
    S.DATA.MIRROR.receive_finalize3 (fun dbg mirror_id sr url verify_dest ->
        Impl.DATA.MIRROR.receive_finalize3 () ~dbg ~mirror_id ~sr ~url
          ~verify_dest
    ) ;
    S.DATA.MIRROR.pre_deactivate_hook (fun dbg dp sr vdi ->
        Impl.DATA.MIRROR.pre_deactivate_hook () ~dbg ~dp ~sr ~vdi
    ) ;
    S.DATA.MIRROR.has_mirror_failed (fun dbg mirror_id sr ->
        Impl.DATA.MIRROR.has_mirror_failed () ~dbg ~mirror_id ~sr
    ) ;
    S.DATA.MIRROR.list (fun dbg -> Impl.DATA.MIRROR.list () ~dbg) ;
    S.DATA.MIRROR.stat (fun dbg id -> Impl.DATA.MIRROR.stat () ~dbg ~id) ;
    S.DATA.import_activate (fun dbg dp sr vdi vm ->
        Impl.DATA.import_activate () ~dbg ~dp ~sr ~vdi ~vm
    ) ;
    S.DATA.get_nbd_server (fun dbg dp sr vdi vm ->
        Impl.DATA.get_nbd_server () ~dbg ~dp ~sr ~vdi ~vm
    ) ;
    S.Policy.get_backend_vm (fun dbg vm sr vdi ->
        Impl.Policy.get_backend_vm () ~dbg ~vm ~sr ~vdi
    ) ;
    S.TASK.stat (fun dbg task -> Impl.TASK.stat () ~dbg ~task) ;
    S.TASK.cancel (fun dbg task -> Impl.TASK.cancel () ~dbg ~task) ;
    S.TASK.destroy (fun dbg task -> Impl.TASK.destroy () ~dbg ~task) ;
    S.TASK.list (fun dbg -> Impl.TASK.list () ~dbg) ;
    S.UPDATES.get (fun dbg from timeout ->
        Impl.UPDATES.get () ~dbg ~from ~timeout
    )

  (* Bind all *)
  let process call = Idl.Exn.server S.implementation call
end
