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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

open Stdext.Threadext
module StringSet = Set.Make(String)

(* A cache mapping PIF names to PIF structures. *)
let pifs_cached_m : Mutex.t = Mutex.create ()
let pifs_cached : (string, Monitor_types.pif) Hashtbl.t = Hashtbl.create 10
let pifs_tmp    : (string, Monitor_types.pif) Hashtbl.t = Hashtbl.create 10
(* A cache mapping PIF names to bond.links_up. *)
let bonds_links_up_cached_m : Mutex.t = Mutex.create ()
let bonds_links_up_cached : (string, int) Hashtbl.t = Hashtbl.create 10
let bonds_links_up_tmp    : (string, int) Hashtbl.t = Hashtbl.create 10
(* A cache mapping vm_uuids to actual memory. *)
let vm_memory_cached_m : Mutex.t = Mutex.create ()
let vm_memory_cached : (string, Int64.t) Hashtbl.t = Hashtbl.create 100
let vm_memory_tmp    : (string, Int64.t) Hashtbl.t = Hashtbl.create 100
(* A cache for host's free/total memory. *)
let host_memory_m : Mutex.t = Mutex.create ()
let host_memory_free_cached : Int64.t ref = ref Int64.zero
let host_memory_total_cached : Int64.t ref = ref Int64.zero
(* A cache mapping VM uuids to PVS_proxy status. *)
let pvs_proxy_cached_m : Mutex.t = Mutex.create ()
let pvs_proxy_cached : (string, int) Hashtbl.t = Hashtbl.create 100
let pvs_proxy_tmp    : (string, int) Hashtbl.t = Hashtbl.create 100
(* A cache for ignoring errors from files that cannot be loaded *)
let ignore_errors = ref StringSet.empty

(** [clear_cache_for_pif] removes any current cache for PIF with [pif_name],
 * which forces fresh properties for the PIF into xapi's database. *)
let clear_cache_for_pif ~pif_name =
  Mutex.execute pifs_cached_m (fun _ ->
      Hashtbl.remove pifs_cached pif_name;
      Hashtbl.remove pifs_tmp pif_name
    )

(** [clear_cache_for_vm] removes any current cache for VM with [vm_uuid],
 * which forces fresh properties for the VM into xapi's database. *)
let clear_cache_for_vm ~vm_uuid =
  Mutex.execute vm_memory_cached_m (fun _ ->
      Hashtbl.remove vm_memory_cached vm_uuid;
      Hashtbl.remove vm_memory_tmp vm_uuid
    )

(** [clear_pvs_status_cache] removes the cache entry for [vm_uuid] *)
let clear_pvs_status_cache ~vm_uuid =
  Mutex.execute pvs_proxy_cached_m (fun _ ->
      Hashtbl.remove pvs_proxy_cached vm_uuid;
      Hashtbl.remove pvs_proxy_tmp vm_uuid
    )

(** Clear the whole cache. This forces fresh properties to be written into
 * xapi's database. *)
let clear_cache () =
  let safe_clear ~cache ~tmp ~lock =
    Mutex.execute lock (fun _ -> Hashtbl.clear cache; Hashtbl.clear tmp) in
  safe_clear ~cache:pifs_cached ~tmp:pifs_tmp ~lock:pifs_cached_m;
  safe_clear ~cache:bonds_links_up_cached ~tmp:bonds_links_up_tmp ~lock:bonds_links_up_cached_m;
  safe_clear ~cache:vm_memory_cached ~tmp:vm_memory_tmp ~lock:vm_memory_cached_m;
  Mutex.execute host_memory_m (fun _ ->
      host_memory_free_cached := Int64.zero;
      host_memory_total_cached := Int64.zero;
    )

(* Helper map functions. *)
let transfer_map ?(except=[]) ~source ~target =
  List.iter (fun ex ->
      try Hashtbl.replace source ex (Hashtbl.find target ex)
      with Not_found -> Hashtbl.remove source ex
    ) except;
  Hashtbl.clear target;
  Hashtbl.iter (fun k v -> Hashtbl.add target k v) source;
  Hashtbl.clear source

let get_updates ~before ~after ~f =
  Hashtbl.fold (fun k v acc ->
      if (try v <> Hashtbl.find before k with Not_found -> true)
      then (f k v acc)
      else acc
    ) after []
let get_updates_map = get_updates ~f:(fun k v acc -> (k, v)::acc)
let get_updates_values = get_updates ~f:(fun _ v acc -> v::acc)

(* Helper for filename errors set *)
let is_ignored filename =
  StringSet.mem filename !ignore_errors

let ignore_errors_from filename =
  ignore_errors := StringSet.add filename !ignore_errors

let log_errors_from filename =
        ignore_errors := StringSet.remove filename !ignore_errors;
