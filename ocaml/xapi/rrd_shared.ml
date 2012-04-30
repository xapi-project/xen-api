(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Shared RRD data
 * @group Performance Monitoring
 *)

(* This module contains all of the data that is shared between 
 * the monitor_rrds module and the monitor_dbcalls module
 * 
 * Also this is used by the xapi_pif code to signal a force-updated
 * when a pif is introduced to ensure that any stats that this
 * module has cached get stored in the database.
 * 
 *)

module StringSet = Set.Make(String)

(* strings are uuids *)
(*type ds_type = VM of string | Host | SR of string*)

(* Mutex to protect the shared data.
 *
 * Warning: Do not do any DB calls with this mutex held! A possible side effect
 * of a DB call will be resetting the 'dirty' status below, requiring acquisition
 * of this lock, resulting in a deadlock! 
 *) 
(*
let mutex = Mutex.create ()
*)

(** A cache of the PIF stats to ensure that PIF updates send the correct info *)
(*let pif_stats : Monitor_types.pif list ref = ref []*)

(** Dirty memory *)
(*let dirty_memory = ref StringSet.empty*)

(** Dirty PIFs *)
(*let dirty_pifs = ref StringSet.empty*)

(** Full update required - this is the one that happens every 60 secs / 5 secs / never *)
(*
let full_update = ref false
let full_update_avg_rra_idx = ref (-1)
let full_update_last_rra_idx = ref (-1)
*)

(** Dirty host memory - host memory has changed *)
(*let dirty_host_memory = ref false*)

(** Condition variable - used to signal that a db update is required *)
(*let condition = Condition.create ()*)
