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

module D = Debug.Make (struct let name = Memory_interface.service_name end)

open D

let _service = "squeezed"

let initial_host_free_memory_file =
  "/var/run/nonpersistent/xapi/boot_time_memory"

let listdir path =
  try
    List.filter
      (fun x -> x <> "")
      (Client.immediate (get_client ()) (fun xs -> Client.directory xs path))
  with Xs_protocol.Enoent _ -> []

let xs_read path =
  try Client.immediate (get_client ()) (fun xs -> Client.read xs path)
  with Xs_protocol.Enoent _ as e ->
    debug "xenstsore-read %s returned ENOENT" path ;
    raise e

let xs_read_option path =
  try Some (Client.immediate (get_client ()) (fun xs -> Client.read xs path))
  with Xs_protocol.Enoent _ -> None

let path = String.concat "/"

(** Path in xenstore where the daemon stores state, specifically reservations *)
let state_path service = path [""; service; "state"]

(** Path in xenstore where the deamon puts the amount of host memory it needs to
    keep eg for lowmem_emergency_pool *)
let reserved_host_memory_path service =
  path [""; service; "reserved-host-memory"]

(** Path where a specific reservation is stored *)
let reservation_path service session_id reservation_id =
  path [""; service; "state"; session_id; reservation_id]

let add_reservation service session_id reservation_id kib =
  Client.immediate (get_client ()) (fun xs ->
      Client.write xs (reservation_path service session_id reservation_id) kib
  )

let del_reservation service session_id reservation_id =
  Client.immediate (get_client ()) (fun xs ->
      Client.rm xs (reservation_path service session_id reservation_id)
  )

(** Return the total amount of memory reserved *)
let total_reservations service domain_infolist =
  let dom_list = List.map (fun di -> di.Xenctrl.domid) domain_infolist in
  let session_ids = listdir (path [""; service; "state"]) in
  let already_counted sid rid =
    match
      xs_read_option (path [""; service; "state"; sid; rid; "in-transfer"])
    with
    | Some domid when List.mem (int_of_string domid) dom_list ->
        true
    | _ ->
        false
  in
  let session_total sid =
    let rids = listdir (path [""; service; "state"; sid]) in
    List.fold_left Int64.add 0L
      (List.map
         (fun rid ->
           if already_counted sid rid then
             0L
           else
             Int64.of_string (xs_read (path [""; service; "state"; sid; rid]))
           )
         rids
      )
  in
  List.fold_left Int64.add 0L (List.map session_total session_ids)
