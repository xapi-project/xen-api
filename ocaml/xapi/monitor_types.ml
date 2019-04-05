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
  pif_name: string;
  pif_carrier: bool;
  pif_speed: int;
  pif_duplex: Network_interface.duplex;
  pif_pci_bus_path: string;
  pif_vendor_id: string;
  pif_device_id: string;
}

module Vif_device = struct
  type t = {
    pv: bool;
    vif: Xenops_interface.Vif.id;
    domid: int;
    devid: int;
  }
end

let vif_device_of_string x =
  let open Vif_device in
  try
    let ty, params = Astring.String.span ~max:3 x in
    let domid, devid = Scanf.sscanf params "%d.%d" (fun x y -> x,y) in
    let di = Xenctrl.with_intf (fun xc -> Xenctrl.domain_getinfo xc domid) in
    let uuid = Uuid.uuid_of_int_array di.Xenctrl.handle |> Uuid.to_string in
    let vif = (uuid, string_of_int devid) in
    match ty with
    | "vif" -> Some { pv = true; vif = vif; domid = domid; devid = devid }
    | "tap" -> Some { pv = false; vif = vif; domid = domid; devid = devid }
    | _ -> failwith "bad device"
  with _ -> None

let find_rrd_files prefix =
  Sys.readdir Xapi_globs.metrics_root
  |> Array.to_list
  |> List.filter (Astring.String.is_prefix ~affix:prefix)

let datasources_from_filename filename =
  let path = Filename.concat Xapi_globs.metrics_root filename in
  let reader = Rrd_reader.FileReader.create path Rrd_protocol_v2.protocol in
  let payload = reader.Rrd_reader.read_payload () in
  payload.Rrd_protocol.datasources
