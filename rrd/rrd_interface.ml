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

(** Error handler *)
module RrdErrHandler = Error.Make(struct
    type t = rrd_errors
    let t  = rrd_errors
    let internal_error_of e = Some (Rrdd_internal_error (Printexc.to_string e))
  end)
let rrd_err = Error.{ def     = rrd_errors
                    ; raiser  = (fun e -> raise (Rrdd_error e))
                    ; matcher = (function
                      | Rrdd_error e -> Some e
                      | e            -> Some (Rrdd_internal_error (Printexc.to_string e)))
                    }

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
      [ "docstring" ]
      ( vm_uuid_p @-> returning bool_p rrd_err)

  let push_rrd_local = declare "push_rrd_local"
    [ "docstring" ]
    (vm_uuid_p @-> domid_p @-> returning unit_p rrd_err)

  let push_rrd_remote = declare "push_rrd_remote"
    [ "docstring" ]
    (vm_uuid_p @-> rem_add_p @-> returning unit_p rrd_err)

  let remove_rrd = declare "remove_rrd"
    [ "docstring" ]
    (uuid_p @-> returning unit_p rrd_err)

  let migrate_rrd =
    let host_uuid_p  = Param.mk ~name:"host_uuid"  ~description:[ "Unique ID of host" ] Types.string in
    let session_id_p = Param.mk ~name:"session_id" ~description:[ "ID of the session" ] string_opt in
    declare "migrate_rrd"
      [ "docstring" ]
      (session_id_p
       @-> rem_add_p
       @-> vm_uuid_p
       @-> host_uuid_p
       @-> returning unit_p rrd_err)


  let send_host_rrd_to_master =
    let mast_addr_str = Param.mk ~name:"master address" ~description:[ "Address of remote" ] Types.string in
    declare "send_host_rrd_to_master"
      [ "Sends host rrd data to master" ]
      (mast_addr_str @-> returning unit_p rrd_err)

  let rem_addr_opt_p = Param.mk ~name:"remote address" ~description:[ "Address of the remote" ] string_opt
  let backup_rrds = declare "backup_rrds"
      [ "Backs up rrd data" ]
      (rem_addr_opt_p @-> unit_p @-> returning unit_p rrd_err)

  let archive_rrd = declare "archive_rrd"
    [ "Archives rrd, optionally at the remote destination provided" ]
    (vm_uuid_p @-> rem_addr_opt_p @-> returning unit_p rrd_err)

  let archive_sr_rrd = declare "archive_sr_rrd"
    [ "Archives SR rrd" ]
    (sr_uuid_p @-> returning string_p rrd_err)

  let push_sr_rrd =
    let path_p = Param.mk ~name:"path" ~description:[ "Filepath" ] Types.string in
    declare "push_sr_rrd"
      [ "docstring" ]
      (sr_uuid_p @-> path_p @-> returning unit_p rrd_err)

  let add_host_ds = declare "add_host_ds"
    [ "docstring" ]
    (ds_name_p @-> returning unit_p rrd_err)

  let forget_host_ds = declare "forget_host_ds"
    [ "docstring" ]
    (ds_name_p @-> returning unit_p rrd_err)

  let query_possible_host_dss =
    declare "query_possible_host_dss"
      [ "Returns list of possible host DSS, if any" ]
      (unit_p @-> returning ds_list_p rrd_err)

  let query_host_ds = declare "query_host_ds"
      [ "Queries DSS of provided DS" ]
      (ds_name_p @-> returning float_p rrd_err)


  let add_vm_ds = declare "add_vm_ds"
    [ "docstring" ]
    (vm_uuid_p @-> domid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let forget_vm_ds = declare "forget_vm_ds"
    [ "docstring" ]
    (vm_uuid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let query_possible_vm_dss = declare "query_possible_vm_dss"
    [ "docstring" ]
    (vm_uuid_p @-> returning ds_list_p rrd_err)

  let query_vm_ds = declare "query_vm_ds"
    [ "docstring" ]
    (vm_uuid_p @-> ds_name_p @-> returning float_p rrd_err)

  let add_sr_ds = declare "add_sr_ds"
    [ "docstring" ]
    (sr_uuid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let forget_sr_ds = declare "forget_sr_ds"
    [ "docstring" ]
    (sr_uuid_p @-> ds_name_p @-> returning unit_p rrd_err)

  let query_possible_sr_dss = declare "query_possible_sr_dss"
    [ "docstring" ]
    (sr_uuid_p @-> returning ds_list_p rrd_err)

  let query_sr_ds = declare "query_sr_ds"
    [ "docstring" ]
    (sr_uuid_p @-> ds_name_p @-> returning float_p rrd_err)

  let update_use_min_max =
    let value_p = Param.mk ~name:"value" ~description:[ "Value dictating whether to use min_max" ] Types.bool in
    declare "update_use_min_max"
      [ "docstring" ]
      (value_p @-> returning unit_p rrd_err)

  let update_vm_memory_target =
    let target_p = Param.mk ~name:"target" ~description:[ "VM memory target" ] Types.int64 in
    declare "update_vm_memory_target"
      [ "docstring" ]
      (domid_p @-> target_p @-> returning unit_p rrd_err)

  let set_cache_sr = declare "set_cache_sr"
    [ "docstring" ]
    (sr_uuid_p @-> returning unit_p rrd_err)

  let unset_cache_sr = declare "unset_cache_sr"
    [ "docstring" ]
    (unit_p @-> returning unit_p rrd_err)


  module Plugin = struct

    let uid_p      = Param.mk ~name:"uid"      ~description:[ "ID" ]                      Types.string
    let info_p     = Param.mk ~name:"info"     ~description:[ "Interdomain info" ]        interdomain_info
    let protocol_p = Param.mk ~name:"protocol" ~description:[ "Plugin protocol version" ] plugin_protocol

    let get_header = declare "get_header"
        [ "Returns header string." ]
        (unit_p @-> returning string_p rrd_err)

    let get_path = declare "get_path"
        [ "Returns path of protocol." ]
        (uid_p @-> returning string_p rrd_err)

    module Local = struct
      let register =
        let info_p = Param.mk ~name:"info" ~description:[ "Local rrd info" ] rrd_freq in
        declare "register"
          [ "docstring" ]
          (uid_p @-> info_p @-> protocol_p @-> returning float_p rrd_err)

      let deregister = declare "deregister"
        [ "docstring" ]
        (uid_p @-> returning unit_p rrd_err)

      let next_reading = declare "next_reading"
        [ "docstring" ]
        (uid_p @-> returning float_p rrd_err)
    end

    module Interdomain = struct

      let iduid_p = Param.mk ~name:"uid" ~description:[ "Interdomain ID" ] interdomain_uid

      let register =
        declare "register"
          [ "docstring" ]
          (iduid_p @-> info_p @-> protocol_p @-> returning float_p rrd_err)

      let deregister = declare "deregister"
        [ "docstring" ]
        (iduid_p @-> returning unit_p rrd_err)

      let next_reading = declare "next_reading"
        [ "docstring" ]
        (iduid_p @-> returning float_p rrd_err)
    end

    let register =
      let freq_p = Param.mk ~name:"frequency" ~description:[ "Rrd database sampling frequency" ] rrd_freq in
      declare "register"
        [ "docstring" ]
        (uid_p @-> freq_p @-> returning float_p rrd_err)

    let deregister = declare "deregister"
      [ "docstring" ]
      (uid_p @-> returning unit_p rrd_err)

    let next_reading = declare "next_reading"
      [ "docstring" ]
      (uid_p @-> returning float_p rrd_err)
  end

  (** High availability module *)
  module HA = struct

    let enable_and_update =
      let heartb_lat_p  = Param.mk ~name:"heartbeat_latency"   ~description:[ "Time taken for heartbeat signal to travel" ] Types.float in
      let xapi_lat_p    = Param.mk ~name:"xapi_latency"        ~description:[ "Time taken for Xapi to respond" ]            Types.float in
      let stfile_lats_p = Param.mk ~name:"statefile_latencies" ~description:[ "Time taken for statefiles to update" ]       sflat_lst in
      declare "enable_and_update"
        [ "docstring" ]
        (stfile_lats_p @-> heartb_lat_p @-> xapi_lat_p @-> returning unit_p rrd_err)

    let disable = declare "disable"
      [ "docstring" ]
      (unit_p @-> returning unit_p rrd_err)
  end

  module Deprecated = struct

    let load_rrd = (* TODO: there can only be one *)
      let timescale_int_p = Param.mk ~name:"timescale"      ~description:[ "Speed of round-robin database loading" ] Types.int in
      let mast_addr_opt_p = Param.mk ~name:"master address" ~description:[ "Address of master" ]                     string_opt in
      declare "load_rrd"
        [ "docstring" ]
        (uuid_p
         @-> timescale_int_p
         @-> mast_addr_opt_p
         @-> returning unit_p rrd_err)
  end
end
