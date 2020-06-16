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
(** Some records for easy passing around of monitor types.
 * @group Performance Monitoring
*)

type pif = {
    pif_name: string
  ; pif_carrier: bool
  ; pif_speed: int
  ; pif_duplex: Network_interface.duplex
  ; pif_pci_bus_path: string
  ; pif_vendor_id: string
  ; pif_device_id: string
}

let vif_device_of_string x =
  try
    let ty, params = Astring.String.span ~max:3 x in
    let domid, devid = Scanf.sscanf params "%d.%d" (fun x y -> (x, y)) in
    match ty with "vif" -> Some (domid, devid) | _ -> None
  with _ -> None

let find_rrd_files () = Sys.readdir Xapi_globs.metrics_root |> Array.to_list

let datasources_from_filename filename =
  let path = Filename.concat Xapi_globs.metrics_root filename in
  let reader = Rrd_file_reader.create path Rrd_protocol_v2.protocol in
  let payload = reader.Rrd_reader_functor.read_payload () in
  payload.Rrd_protocol.datasources
