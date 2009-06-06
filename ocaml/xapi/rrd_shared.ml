(** Shared RRD data *)

(* This module contains all of the data that is shared between 
 * the monitor_rrds module and the monitor_dbcalls module
 * 
 * Also this is used by the xapi_pif code to signal a force-updated
 * when a pif is introduced to ensure that any stats that this
 * module has cached get stored in the database.
 * 
 *)

module StringSet = Set.Make(String)

type rrd_info = { 
  rrd: Rrd.rrd;
  mutable dss: Ds.ds list; 
}

(* strings are uuids *)
type ds_type = VM of string | Host | SR of string

let mutex = Mutex.create ()

(* RRDs *)		  
let vm_rrds : (string, rrd_info) Hashtbl.t = Hashtbl.create 32
let host_rrd : rrd_info option ref = ref None

(** A cache of the PIF stats to ensure that PIF updates send the correct info *)
let pif_stats : Monitor_types.pif list ref = ref [] 

(** Dirty memory *)
let dirty_memory = ref StringSet.empty

(** Dirty PIFs *)
let dirty_pifs = ref StringSet.empty

(** Full update required - this is the one that happens every 60 secs / 5 secs / never *)
let full_update = ref false
let full_update_avg_rra_idx = ref (-1)
let full_update_last_rra_idx = ref (-1)

(** Dirty host memory - host memory has changed *)
let dirty_host_memory = ref false

(** Condition variable - used to signal that a db update is required *)
let condition = Condition.create ()



