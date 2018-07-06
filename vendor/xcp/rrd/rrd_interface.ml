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

(*
 * The interface of the RRD daemon is defined by the extern function
 * declarations in this file. Implemented by RRD server (separate
 * thread), used by RRD client (part of xapi).
 *)

open Rpc
open Idl

let service_name = "rrd"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let daemon_name = "xcp-rrdd"
let default_path = ref (Filename.concat default_sockets_dir daemon_name)
let forwarded_path = ref (Filename.concat default_sockets_dir daemon_name ^ ".forwarded")

let set_sockets_dir x =
  default_path := Filename.concat x daemon_name;
  forwarded_path := !default_path ^ ".forwarded"

let uri () = "file:" ^ !default_path

(** Version 1 or 2 of plugin protocol *)
type plugin_protocol =
  | V1  (** Plugin protocol 1 *)
  | V2  (** Plugin protocol 2 *)
[@@default V2]
[@@deriving rpcty]

(** Domain ID of VM *)
type interdomain_uid =
  { name           : string; (** VM domain name label *)
    frontend_domid : int     (** Front-end domain ID number *)
  } [@@deriving rpcty]

(* Note: for types such as rrd_req, which alias
 * user-defined types, it is usually not enough
 * to add [@@deriving rpcty] to derive RPC types,
 * and rpc functions must be declared in the definition.
 * However, types such as Rrd.sampling_frequency have
 * already been defined as RPC types in their original declarations
 * so we are able to derive these type aliases like so *)

type rrd_freq = Rrd.sampling_frequency = Five_Seconds
[@@deriving rpcty]

type statefile_latency = Rrd.Statefile_latency.t = { id: string; latency: float option }
[@@deriving rpcty]

type sflat_lst = statefile_latency list
[@@deriving rpcty]

(** Domain database sampling info *)
type interdomain_info =
  { frequency        : rrd_freq; (** interdomain rrd sampling frequency *)
    shared_page_refs : int list  (** list of shared page references *)
  } [@@deriving rpcty]

type string_opt = string option
[@@deriving rpcty]

type ds_list = Data_source.t list
[@@deriving rpcty]


(* -- error handling -- *)

(** Rrdd error type *)
type rrd_errors =
  | Archive_failed      of string    (** Archival failure *)
  | Invalid_protocol    of string    (** Thrown by protocol_of_string if
                                         string does not match plugin protocol *)
  | Rrdd_internal_error of string    (** Internal Rrdd error *)
[@@deriving rpcty]

exception Rrdd_error of rrd_errors

let () = (* register printer *)
  let string_of_error e =
    Rpcmarshal.marshal rrd_errors.Rpc.Types.ty e |> Rpc.to_string in
  let printer = function
    | Rrdd_error e ->
      Some (Printf.sprintf "Rrd_interface.Rrdd_error(%s)" (string_of_error e))
    | _ -> None in
  Printexc.register_printer printer

(** Error handler *)
module RrdErrHandler = Error.Make(struct
    type t = rrd_errors
    let t  = rrd_errors
    let internal_error_of e = Some (Rrdd_internal_error (Printexc.to_string e))
  end)
let rrd_err = RrdErrHandler.error

let string_of_protocol = function
  | V1 -> "V1"
  | V2 -> "V2"

let protocol_of_string = function
  | x when x="V1" -> V1
  | x when x="V2" -> V2
  | y -> raise (Rrdd_error (Invalid_protocol(y)))


(* -- RPC generation -- *)

module RPC_API(R : RPC) = struct
  open R

  let description =
    Interface.{ name = "Rrd"
              ; namespace = None
              ; description =
                  [ "This interface is used by Xapi and Rrdd to manage "
                  ; "round robin database sampling of dom0 databases. " ]
              ; version=(1,0,0)
              }

  let implementation = implement description

  (** Common API call parameter definitions *)

  let unit_p   = Param.mk Types.unit
  let string_p = Param.mk Types.string
  let float_p  = Param.mk Types.float

  let uuid_p    = Param.mk ~name:"uuid"           ~description:[ "User ID" ]            Types.string
  let vm_uuid_p = Param.mk ~name:"vm_uuid"        ~description:[ "VM ID" ]              Types.string
  let domid_p   = Param.mk ~name:"domid"          ~description:[ "Domain ID of VM" ]    Types.int
  let sr_uuid_p = Param.mk ~name:"sr_uuid"        ~description:[ "SR ID" ]              Types.string
  let ds_name_p = Param.mk ~name:"ds_name"        ~description:[ "Domain server name" ] Types.string
  let rem_add_p = Param.mk ~name:"remote_address" ~description:[ "Remote address" ]     Types.string

  let ds_list_p = Param.mk ~name:"data_source list"
      ~description: [ "Datasource list" ] ds_list

  (** API call definitions *)

  let has_vm_rrd =
    let bool_p = Param.mk Types.bool in
    declare "has_vm_rrd"
      [ "Returns `true` if xcp-rrdd has an RRD for the specified VM in memory" ]
      ( vm_uuid_p @-> returning bool_p rrd_err)

  let push_rrd_local = declare "push_rrd_local"
    [ "Loads a VM RRD from local storage, associates it with the specified domid, and";
      "starts recording all data sources related to the VM to that RRD" ]
    (vm_uuid_p @-> domid_p @-> returning unit_p rrd_err)

  let push_rrd_remote = declare "push_rrd_remote"
    [ "Loads a VM RRD from local storage and pushes it to a remote host" ]
    (vm_uuid_p @-> rem_add_p @-> returning unit_p rrd_err)

  let remove_rrd = declare "remove_rrd"
    [ "Removes a VM RRD from the local filesystem, if it exists." ]
    (uuid_p @-> returning unit_p rrd_err)

  let migrate_rrd =
    let host_uuid_p  = Param.mk ~name:"host_uuid"  ~description:[ "Unique ID of host" ] Types.string in
    let session_id_p = Param.mk ~name:"session_id" ~description:[ "ID of the session" ] string_opt in
    declare "migrate_rrd"
      [ "Migrate_push - used by the migrate code to push an RRD directly to"
      ; "a remote host without going via the master. If the host is on a"
      ; "different pool, you must pass both the remote_address and session_id parameters."]
      (session_id_p
       @-> rem_add_p
       @-> vm_uuid_p
       @-> host_uuid_p
       @-> returning unit_p rrd_err)


  let send_host_rrd_to_master =
    let mast_addr_str = Param.mk ~name:"master_address" ~description:[ "Address of remote" ] Types.string in
    declare "send_host_rrd_to_master"
      [ "Called on host shutdown/reboot to send the Host RRD to the master for"
      ; "backup." ]
      (mast_addr_str @-> returning unit_p rrd_err)

  let rem_addr_opt_p = Param.mk ~name:"remote_address" ~description:[ "Address of the remote" ] string_opt
  let backup_rrds = declare "backup_rrds"
      [ "Backs up RRD data to disk. This should be done periodically to ensure"
      ; "that if the host crashes we don't lose too much data." ]
      (rem_addr_opt_p @-> unit_p @-> returning unit_p rrd_err)

  let archive_rrd = declare "archive_rrd"
    [ "Sends the VM RRD either to local disk or the remote address if specified,"
    ; "and removes it from memory. Called on VM shutdown/suspend." ]
    (vm_uuid_p @-> rem_addr_opt_p @-> returning unit_p rrd_err)

  let archive_sr_rrd = declare "archive_sr_rrd"
    [ "Saves the SR RRD to the local disk. Returns the path to the saved RRD so"
    ; "it can be copied onto the SR before it is detached." ]
    (sr_uuid_p @-> returning string_p rrd_err)

  let push_sr_rrd =
    let path_p = Param.mk ~name:"path" ~description:[ "Filepath" ] Types.string in
    declare "push_sr_rrd"
      [ "Loads the RRD from the path specified on the local disk. Overwrites any"
      ; "RRD already in memory for the SR. Data sources will subsequently be "
      ; "recorded to this RRD." ]
      (sr_uuid_p @-> path_p @-> returning unit_p rrd_err)

  let add_host_ds = declare "add_host_ds"
    [ "Adds a host data source to the host RRD. This causes the data source to be "
    ; "recorded if it wasn't a default data source." ]
    (ds_name_p @-> returning unit_p rrd_err)

  let forget_host_ds = declare "forget_host_ds"
    [ "Forgets the recorded archives for the named data source. Note that if the "
    ; "data source is marked as default, new data coming in will cause the archive"
    ; "to be recreated." ]
    (ds_name_p @-> returning unit_p rrd_err)

  let query_possible_host_dss =
    declare "query_possible_host_dss"
      [ "Returns list of possible host DSs. This will include data sources not "
      ; "currently being recorded into archives." ]
      (unit_p @-> returning ds_list_p rrd_err)

  let query_host_ds = declare "query_host_ds"
      [ "Returns the current value of the named host data source. Note this returns"
      ; " the raw data source value, not the smoothed last value of the RRA." ]
      (ds_name_p @-> returning float_p rrd_err)


  let add_vm_ds = declare "add_vm_ds"
    [ "Adds a VM data source to the VM RRD. This causes the data source to be"
    ; " recorded if it wasn't a default data source." ]
    (vm_uuid_p @-> domid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let forget_vm_ds = declare "forget_vm_ds"
    [ "Forgets the recorded archives for the named VM data source. Note that if the"
    ; "data source is marked as default, new data coming in will cause the archive"
    ; "to be recreated." ]
    (vm_uuid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let query_possible_vm_dss = declare "query_possible_vm_dss"
    [ "Returns list of possible VM DSs. This will include data sources not"
    ; "currently being recorded into archives." ]
    (vm_uuid_p @-> returning ds_list_p rrd_err)

  let query_vm_ds = declare "query_vm_ds"
    [ "Returns the current value of the named VM data source. Note this returns"
    ; "the raw data source value, not the smoothed last value of the RRA." ]
    (vm_uuid_p @-> ds_name_p @-> returning float_p rrd_err)

  let add_sr_ds = declare "add_sr_ds"
    [ "Adds an SR data source to the SR RRD. This causes the data source to be"
    ; "recorded if it wasn't a default data source." ]
    (sr_uuid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let forget_sr_ds = declare "forget_sr_ds"
    [ "Forgets the recorded archives for the named SR data source. Note that if the"
    ; "data source is marked as default, new data coming in will cause the archive"
    ; "to be recreated." ]
    (sr_uuid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let query_possible_sr_dss = declare "query_possible_sr_dss"
    [ "Returns list of possible SR DSs. This will include data sources not"
    ; "currently being recorded into archives." ]
    (sr_uuid_p @-> returning ds_list_p rrd_err)

  let query_sr_ds = declare "query_sr_ds"
    [ "Returns the current value of the named VM data source. Note this returns"
    ; "the raw data source value, not the smoothed last value of the RRA." ]
    (sr_uuid_p @-> ds_name_p @-> returning float_p rrd_err)

  let update_use_min_max =
    let value_p = Param.mk ~name:"value" ~description:[ "Value dictating whether to use min_max" ] Types.bool in
    declare "update_use_min_max"
      [ "Set the value of the `use_min_max` variable. If this is `true`, when creating"
      ; "a new RRD, archives for the minimum and maximum observed values will be created"
      ; "alongside the standard archive of average values" ]
      (value_p @-> returning unit_p rrd_err)

  let update_vm_memory_target =
    let target_p = Param.mk ~name:"target" ~description:[ "VM memory target" ] Types.int64 in
    declare "update_vm_memory_target"
      [ "Sets the `memory_target` value for a VM. This is called by xapi when it is told by"
      ; "xenopsd that squeezed has changed the target for a VM." ]
      (domid_p @-> target_p @-> returning unit_p rrd_err)

  let set_cache_sr = declare "set_cache_sr"
    [ "Sets the uuid of the cache SR. If this is set, statistics about the usage of the cache"
    ; "will be recorded into the host SR." ]
    (sr_uuid_p @-> returning unit_p rrd_err)

  let unset_cache_sr = declare "unset_cache_sr"
    [ "Unsets the cache_sr. No futher data will be gathered about cache usage, but existing"
    ; "archive data will not be deleted." ]
    (unit_p @-> returning unit_p rrd_err)


  module Plugin = struct

    let uid_p      = Param.mk ~name:"uid"      ~description:[ "Plugin UID" ]              Types.string
    let info_p     = Param.mk ~name:"info"     ~description:[ "Interdomain info" ]        interdomain_info
    let protocol_p = Param.mk ~name:"protocol" ~description:[ "Plugin protocol version" ] plugin_protocol

    let get_header = declare "Plugin.get_header"
        [ "Returns header string. This string should be copied exactly to the start"
        ; "of the shared memory containing the data source" ]
        (unit_p @-> returning string_p rrd_err)

    let get_path = declare "Plugin.get_path"
        [ "Returns path in the local filesystem to place the data"
        ; "source file" ]
        (uid_p @-> returning string_p rrd_err)

    module Local = struct
      let register =
        let info_p = Param.mk ~name:"info" ~description:[ "Local rrd info" ] rrd_freq in
        declare "Plugin.Local.register"
          [ "[Plugin.Local.register uid info protocol] registers a plugin"
          ; "as a source of a set of data-sources. [uid] is a unique identifier"
          ; "for the plugin, often the name of the plugin. [info] is the RRD"
          ; "frequency, and [protocol] specifies whether the plugin will be"
          ; "using V1 or V2 of the protocol."]
          (uid_p @-> info_p @-> protocol_p @-> returning float_p rrd_err)

      let deregister = declare "Plugin.Local.deregister"
        [ "Deregisters a plugin by uid" ]
        (uid_p @-> returning unit_p rrd_err)

      let next_reading = declare "Plugin.Local.next_reading"
        [ "Returns the number of seconds until the next reading will be taken." ]
        (uid_p @-> returning float_p rrd_err)
    end

    module Interdomain = struct

      let iduid_p = Param.mk ~name:"uid" ~description:[ "Interdomain ID" ] interdomain_uid

      let register =
        declare "Plugin.Interdomain.register"
          [ "[Plugin.Interdomain.register uid info protocol] registers an"
          ; "interdomain plugin. [uid] is the unique identifier of the"
          ; "plugin, containing both a name and the frontend domain id."
          ; "[info] contains both the desired sampling frequency and"
          ; "a list of the grant references of the shared pages. [protocol]"
          ; "is V1 or V2, and the return value is the time until the next"
          ; "reading" ]
          (iduid_p @-> info_p @-> protocol_p @-> returning float_p rrd_err)

      let deregister = declare "Plugin.Interdomain.deregister"
        [ "Deregisters a plugin by uid." ]
        (iduid_p @-> returning unit_p rrd_err)

      let next_reading = declare "Plugin.Interdomain.next_reading"
        [ "Returns the number of seconds before the next reading." ]
        (iduid_p @-> returning float_p rrd_err)
    end

    let register =
      let freq_p = Param.mk ~name:"frequency" ~description:[ "Rrd database sampling frequency" ] rrd_freq in
      declare "Plugin.register"
        [ "Preserved for backwards compatibility. Equivalent to a Local"
        ; "plugin registration with V1 protocol." ]
        (uid_p @-> freq_p @-> returning float_p rrd_err)

    let deregister = declare "Plugin.deregister"
      [ "Preserved for backwards compatibility. Deregesters a local plugin." ]
      (uid_p @-> returning unit_p rrd_err)

    let next_reading = declare "Plugin.next_reading"
      [ "Returns the time until the next reading." ]
      (uid_p @-> returning float_p rrd_err)
  end

  (** High availability module *)
  module HA = struct

    let enable_and_update =
      let heartb_lat_p  = Param.mk ~name:"heartbeat_latency"   ~description:[ "Time taken for heartbeat signal to travel" ] Types.float in
      let xapi_lat_p    = Param.mk ~name:"xapi_latency"        ~description:[ "Time taken for Xapi to respond" ]            Types.float in
      let stfile_lats_p = Param.mk ~name:"statefile_latencies" ~description:[ "Time taken for statefiles to update" ]       sflat_lst in
      declare "HA.enable_and_update"
        [ "Enables the gathering of HA metrics, a built-in function of xcp-rrdd." ]
        (stfile_lats_p @-> heartb_lat_p @-> xapi_lat_p @-> returning unit_p rrd_err)

    let disable = declare "HA.disable"
      [ "Disables the HA metrics." ]
      (unit_p @-> returning unit_p rrd_err)
  end

  module Deprecated = struct

    let load_rrd =
      let timescale_int_p = Param.mk ~name:"timescale"      ~description:[ "Speed of round-robin database loading" ] Types.int in
      let mast_addr_opt_p = Param.mk ~name:"master address" ~description:[ "Master address to load rrd from" ]       string_opt in
      declare "Deprecated.load_rrd"
        [ "Deprecated call." ]
        (uuid_p
         @-> timescale_int_p
         @-> mast_addr_opt_p
         @-> returning unit_p rrd_err)
  end
end
