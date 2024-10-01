(*
 * Copyright (C) Cloud Software Group
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

module D = Debug.Make (struct let name = "xcp-rrdp-xenctrl-lib" end)

let uuid_blacklist = ["00000000-0000-0000"; "deadbeef-dead-beef"]

module IntSet = Set.Make (Int)

let domain_snapshot xc =
  let metadata_of_domain dom =
    let ( let* ) = Option.bind in
    let* uuid_raw = Uuidx.of_int_array dom.Xenctrl.handle in
    let uuid = Uuidx.to_string uuid_raw in
    let domid = dom.Xenctrl.domid in
    let start = String.sub uuid 0 18 in
    (* Actively hide migrating VM uuids, these are temporary and xenops writes
       the original and the final uuid to xenstore *)
    let uuid_from_key key =
      let path = Printf.sprintf "/vm/%s/%s" uuid key in
      try Ezxenstore_core.Xenstore.(with_xs (fun xs -> xs.read path))
      with Xs_protocol.Enoent _hint ->
        D.info "Couldn't read path %s; falling back to actual uuid" path ;
        uuid
    in
    let stable_uuid = Option.fold ~none:uuid ~some:uuid_from_key in
    if List.mem start uuid_blacklist then
      None
    else
      let key =
        if Astring.String.is_suffix ~affix:"000000000000" uuid then
          Some "origin-uuid"
        else if Astring.String.is_suffix ~affix:"000000000001" uuid then
          Some "final-uuid"
        else
          None
      in
      Some (dom, stable_uuid key, domid)
  in
  let domains =
    Xenctrl.domain_getinfolist xc 0 |> List.filter_map metadata_of_domain
  in
  let timestamp = Unix.gettimeofday () in
  let domain_paused (d, uuid, _) =
    if d.Xenctrl.paused then Some uuid else None
  in
  let paused_uuids = List.filter_map domain_paused domains in
  (timestamp, domains, paused_uuids)
