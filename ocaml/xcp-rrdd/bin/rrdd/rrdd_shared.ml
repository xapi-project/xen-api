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

module D = Debug.Make (struct let name = "rrdd_shared" end)

open D
module StringSet = Set.Make (String)

(* Whether to enable all non-default datasources *)
let enable_all_dss = ref false

(* The expected time span between each monitoring loop. *)
let timeslice : Mtime.span ref = ref Mtime.Span.(5 * s)

(* A timer that expires at the start of the next iteration *)
let next_iteration_start : Clock.Timer.t ref =
  ref (Clock.Timer.start ~duration:!timeslice)

(* The mutex that protects the next_iteration_start against data corruption. *)
let next_iteration_start_m : Mutex.t = Mutex.create ()

(** Cache memory/target values *)
let memory_targets : (int, int64) Hashtbl.t = Hashtbl.create 20

let memory_targets_m = Mutex.create ()

let cache_sr_uuid : string option ref = ref None

let cache_sr_lock = Mutex.create ()

let default_ssl_port = 443

let https_port = ref default_ssl_port

(** Pool secret. *)
let get_pool_secret () =
  try
    Unix.access Rrdd_libs.Constants.pool_secret_path [Unix.F_OK] ;
    Xapi_stdext_unix.Unixext.string_of_file Rrdd_libs.Constants.pool_secret_path
  with _ -> failwith "Unable to read the pool secret."

(* Here is the only place where RRDs are created. The timescales are fixed. If
   other timescales are required, this could be done externally. The types of
   archives created are also fixed. Currently, we're making 4 timescales of 3
   types of archive. This adds up to a total of (120+120+168+366)*3 doubles per
   field, and at 8 bytes per double this is a grand total of 18k per field. For
   a VM with 2 VBDs, 2 VCPUs and 1 VIF, this adds up to 130k of data per VM.
   This is the function where tuning could be done to change this. *)
let timescales =
  (* These are purely for xenrt testing. *)
  if Rrdd_fist.reduce_rra_times then
    [(120, 1); (20, 12); (15, 24); (10, 36)]
  else
    [
      (120, 1)
    ; (* 120 values of interval 1 step (5 secs) = 10 mins *)
      (120, 12)
    ; (* 120 values of interval 12 steps (1 min) = 2 hours *)
      (168, 720)
    ; (* 168 values of interval 720 steps (1 hr) = 1 week *)
      (366, 17280) (* 366 values of interval 17280 steps (1 day) = 1 yr *)
    ]

let use_min_max = ref false

let mutex = Mutex.create ()

type rrd_info = {
    rrd: Rrd.rrd
        (** Contains the live metrics, i.e. The datasources that are enabled
            and being collected .*)
  ; mutable dss: (float * Ds.ds) Rrd.StringMap.t
        (** Important: this must contain the entire list of datasources
           associated with the RRD, even the ones disabled by default, because
           functions like rrd_add_ds or rrd_remove_ds expect disabled
           datasources to be present. e.g. to enable DSs at runtime *)
  ; mutable domid: int
}

(* RRDs *)
let vm_rrds : (string, rrd_info) Hashtbl.t = Hashtbl.create 32

let sr_rrds : (string, rrd_info) Hashtbl.t = Hashtbl.create 32

let host_rrd : rrd_info option ref = ref None

let rrd_of_fd fd =
  let ic = Unix.in_channel_of_descr fd in
  let input = Xmlm.make_input ~strip:true (`Channel ic) in
  Rrd.from_xml input

(** Helper function - path is the path to the file _without the extension .gz_ *)
let rrd_of_gzip path =
  let gz_path = path ^ ".gz" in
  let gz_exists =
    try
      let (_ : Unix.stats) = Unix.stat gz_path in
      true
    with _ -> false
  in
  if gz_exists then
    Some
      (Xapi_stdext_unix.Unixext.with_file gz_path [Unix.O_RDONLY] 0o0 (fun fd ->
           Gzip.Default.decompress_passive fd rrd_of_fd
       )
      )
  else
    None

(* Send rrds to a remote host. If the host is on another pool, you must pass the
   session_id parameter, and optionally the __context. *)
let send_rrd ?(session_id : string option)
    ~(transport : Xmlrpc_client.transport) ~(to_archive : bool) ~(uuid : string)
    ~(rrd : Rrd.rrd) () =
  debug "Sending RRD for object uuid=%s archiving=%b to address: %s" uuid
    to_archive
    (Xmlrpc_client.string_of_transport transport) ;
  let arch_query = if to_archive then [("archive", "true")] else [] in
  let sid_query =
    match session_id with None -> [] | Some id -> [("session_id", id)]
  in
  let query = sid_query @ arch_query @ [("uuid", uuid)] in
  let cookie =
    if sid_query = [] then [("pool_secret", get_pool_secret ())] else []
  in
  let request =
    Http.Request.make ~user_agent:Rrdd_libs.Constants.rrdd_user_agent ~query
      ~cookie Http.Put Rrdd_libs.Constants.put_rrd_uri
  in
  let open Xmlrpc_client in
  with_transport transport
    (with_http request (fun (_response, fd) ->
         try Rrd_unix.to_fd ~internal:true rrd fd with e -> log_backtrace e
     )
    ) ;
  debug "Sending RRD complete."

let archive_rrd_internal ?(transport = None) ~uuid ~rrd () =
  match transport with
  | None -> (
      debug "Archiving RRD for object uuid=%s to local disk" uuid ;
      try
        (* Stash away the rrd onto disk. *)
        let exists =
          try
            let (_ : Unix.stats) =
              Unix.stat Rrdd_libs.Constants.blob_location
            in
            true
          with _ -> false
        in
        if exists then (
          Xapi_stdext_unix.Unixext.mkdir_safe Rrdd_libs.Constants.rrd_location
            0o755 ;
          let base_filename = Rrdd_libs.Constants.rrd_location ^ "/" ^ uuid in
          Xapi_stdext_unix.Unixext.atomic_write_to_file (base_filename ^ ".gz")
            0o644 (fun fd ->
              Gzip.Default.compress fd (Rrd_unix.to_fd ~internal:true rrd)
          ) ;
          (* If there's an uncompressed one hanging around, remove it. *)
          Xapi_stdext_unix.Unixext.unlink_safe base_filename
        ) else
          debug "No local storage: not persisting RRDs"
      with e -> log_backtrace e
    )
  | Some transport ->
      (* Stream it to the master to store, or maybe to a host in the migrate
         case *)
      debug "Archiving RRD for object uuid=%s to %s" uuid
        (Xmlrpc_client.string_of_transport transport) ;
      send_rrd ~transport ~to_archive:true ~uuid ~rrd ()
