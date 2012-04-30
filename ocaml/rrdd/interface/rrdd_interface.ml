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

let name = "xcp-rrdd"

(* The interface is defined by extern function declarations. *)

external load_rrd : uuid:string -> ?is_host:bool -> unit -> unit = ""
external push_rrd : vm_uuid:string -> unit = ""
external remove_rrd : vm_uuid:string -> unit = ""
external migrate_rrd : ?session_id:string -> remote_address:string ->
	vm_uuid:string -> host_uuid:string -> unit -> unit = ""
external send_host_rrd_to_master : unit -> unit = ""
external backup_rrds : ?save_stats_locally:bool -> unit -> unit = ""

external add_host_ds : ds_name:string -> unit = ""
external forget_host_ds : ds_name:string -> unit = ""
external query_possible_host_dss : unit -> Data_source.t list = ""
external query_host_ds : ds_name:string -> float = ""

external add_vm_ds : vm_uuid:string -> ds_name:string -> unit = ""
external forget_vm_ds : vm_uuid:string -> ds_name:string -> unit = ""
external query_possible_vm_dss : vm_uuid:string -> Data_source.t list = ""
external query_vm_ds : vm_uuid:string -> ds_name:string -> float = ""

external update_use_min_max : value:bool -> unit = ""

external add_to_uncooperative_domains : domid:int -> unit = ""
external remove_from_uncooperative_domains : domid:int -> unit = ""
external get_uncooperative_domains : unit -> string list = ""
external is_domain_cooperative : domid:int -> bool = ""

external update_vm_memory_target : domid:int -> target:int64 -> unit = ""

external set_cache_sr : sr_uuid:string -> unit = ""
external unset_cache_sr : unit -> unit = ""

module Deprecated = struct
	external get_full_update_avg_rra_idx : unit -> int = ""
	external get_full_update_last_rra_idx : unit -> int = ""
end
