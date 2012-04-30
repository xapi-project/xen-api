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

(* The framework requires type 'context' to be defined. *)
type context = unit

open Threadext

let backup_rrds _ ?(save_stats_locally : bool option) () : unit = ()
(* Monitor_rrds.backup *)

let load_rrd _ ~(uuid : string) ?(is_host : bool option) () : unit = ()
(* Monitor_rrds.load_rrd *)

let push_rrd _ ~(vm_uuid : string) : unit = ()
(* Monitor_rrds.push_rrd *)

let remove_rrd _ ~(vm_uuid : string) : unit = ()
(* Monitor_rrds.maybe_remove_rrd *)

let migrate_rrd _ ?(remote_address : string option) ?(session_id : string option) ~(vm_uuid : string) ~(host_uuid : string) () : unit = ()
(* Monitor_rrds.migrate_push *)

let send_host_rrd_to_master _ () = ()
(* Monitor_rrds.send_host_rrd_to_master *)


let add_host_ds _ ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.add_host_ds *)

let forget_host_ds _ ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.forget_host_ds *)

let query_possible_host_dss _ () : Data_source.t list =
	[]
	(* Monitor_rrds.query_possible_host_dss *)

let query_host_ds _ ~(ds_name : string) : float =
	-1.0
	(* Monitor_rrds.query_host_dss *)


let add_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.add_vm_ds *)

let forget_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.forget_vm_ds *)

let query_possible_vm_dss _ ~(vm_uuid : string) : Data_source.t list =
	[]
	(* Monitor_rrds.query_possible_vm_dss *)

let query_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : float =
	-1.0
	(* Monitor_rrds.query_vm_dss *)


let update_use_min_max _ ~(value : bool) : unit =
	()
(*
    debug "Updating use_min_max: New value=%b" new_use_min_max;
    use_min_max := new_use_min_max;
*)

(* Handle uncooperative domains. *)
let uncooperative_domains: (int, unit) Hashtbl.t = Hashtbl.create 20
let uncooperative_domains_m = Mutex.create ()

let string_of_domain_handle dh =
  Uuid.string_of_uuid (Uuid.uuid_of_int_array dh.Xenctrl.handle)

let add_to_uncooperative_domains _ ~(domid : int) : unit =
	Mutex.execute uncooperative_domains_m
		(fun _ -> Hashtbl.replace uncooperative_domains domid ())

let remove_from_uncooperative_domains _ ~(domid : int) : unit =
	Mutex.execute uncooperative_domains_m
		(fun _ -> Hashtbl.remove uncooperative_domains domid)

let is_domain_cooperative _ ~(domid : int) : bool =
	Mutex.execute uncooperative_domains_m
		(fun _ -> not (Hashtbl.mem uncooperative_domains domid))

let get_uncooperative_domains _ () : string list =
	let domids = Mutex.execute uncooperative_domains_m
		(fun () -> Hashtbl.fold (fun domid _ acc -> domid::acc) uncooperative_domains []) in
	let dis = Xenctrl.with_intf (fun xc -> Xenctrl.domain_getinfolist xc 0) in
	let dis_uncoop = List.filter (fun di -> List.mem di.Xenctrl.domid domids) dis in
	List.map string_of_domain_handle dis_uncoop

(** Cache memory/target values *)
let memory_targets : (int, int64) Hashtbl.t = Hashtbl.create 20
let memory_targets_m = Mutex.create ()

let update_vm_memory_target _ ~(domid : int) ~(target : int64) : unit =
	Mutex.execute memory_targets_m
		(fun _ -> Hashtbl.replace memory_targets domid target)

let cache_sr_uuid = ref None
let cache_sr_lock = Mutex.create ()

let set_cache_sr _ ~(sr_uuid : string) : unit =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := Some sr_uuid)

let unset_cache_sr _ () =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := None)
