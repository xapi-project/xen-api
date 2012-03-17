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
open Squeezed_rpc 
open Xenstore

(** Path in xenstore where the daemon stores state, specifically reservations *)
let state_path service = path [ ""; service; "state" ]

(** Path in xenstore where the deamon puts the amount of host memory it needs to keep eg for lowmem_emergency_pool *)
let reserved_host_memory_path service = path [ ""; service; "reserved-host-memory" ]

(** Path in the filesystem where the deamon puts the amount of host memory it needs to keep eg for lowmem_emergency_pool *)
let reserved_host_memory_filename service = Printf.sprintf "/var/run/%s/reserved-host-memory" service

(** Path where a specific reservation is stored *)
let reservation_path service session_id reservation_id = path [ ""; service; "state"; session_id; reservation_id ]

let add_reservation xs service session_id reservation_id kib = 
  xs.Xs.write (reservation_path service session_id reservation_id) kib

let del_reservation xs service session_id reservation_id = 
  xs.Xs.rm (reservation_path service session_id reservation_id)

(** Return the total amount of memory reserved *)
let total_reservations xs service = 
  let session_ids = listdir xs (path [ ""; service; "state" ]) in
  let session_total session_id = 
    let rids = listdir xs (path [ ""; service; "state"; session_id ]) in
    List.fold_left Int64.add 0L (List.map (fun r -> Int64.of_string (xs_read xs (path [ ""; service; "state"; session_id; r]))) rids) in
  List.fold_left Int64.add 0L (List.map session_total session_ids)
