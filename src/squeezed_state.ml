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
open Squeezed_xenstore

module D = Debug.Make(struct let name = Memory_interface.service_name end)
open D

let ( |> ) a b = b a

let _service = "squeezed"

let listdir path = try List.filter (fun x -> x <> "") (Client.with_xs client (fun xs -> Client.directory xs path)) with Xs_protocol.Enoent _ -> []
let xs_read path = try Client.with_xs client (fun xs -> Client.read xs path) with Xs_protocol.Enoent _ as e -> begin debug "xenstsore-read %s returned ENOENT" path; raise e end

let path = String.concat "/"

(** Path in xenstore where the daemon stores state, specifically reservations *)
let state_path service = path [ ""; service; "state" ]

(** Path in xenstore where the deamon puts the amount of host memory it needs to keep eg for lowmem_emergency_pool *)
let reserved_host_memory_path service = path [ ""; service; "reserved-host-memory" ]

(** Path where a specific reservation is stored *)
let reservation_path service session_id reservation_id = path [ ""; service; "state"; session_id; reservation_id ]

let add_reservation service session_id reservation_id kib = 
  Client.with_xs client (fun xs -> Client.write xs (reservation_path service session_id reservation_id) kib)

let del_reservation service session_id reservation_id = 
  Client.with_xs client (fun xs -> Client.rm xs (reservation_path service session_id reservation_id))

(** Return the total amount of memory reserved *)
let total_reservations service = 
  let session_ids = listdir (path [ ""; service; "state" ]) in
  let session_total session_id = 
    let rids = listdir (path [ ""; service; "state"; session_id ]) in
    List.fold_left Int64.add 0L (List.map (fun r -> Int64.of_string (xs_read (path [ ""; service; "state"; session_id; r]))) rids) in
  List.fold_left Int64.add 0L (List.map session_total session_ids)
