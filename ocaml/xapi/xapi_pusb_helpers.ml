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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module D=Debug.Make(struct let name="xapi" end)
open D

open Stdext
open Yojson.Basic.Util

module USB = struct
  type t = {
    path         : string;
    vendor_id    : string;
    vendor_desc  : string;
    product_id   : string;
    product_desc : string;
    serial       : string;
    version      : string;
    description  : string;
  }
  let compare x y =
    if x.path = y.path then
      if x.vendor_id = y.vendor_id then
        if x.product_id = y.product_id then
          compare x.serial y.serial
        else
          compare x.product_id y.product_id
      else
        compare x.vendor_id y.vendor_id
    else
      compare x.path y.path
end

module USBSet = Set.Make(USB)

let extract_member json member =
  let safe_hd = function
    | x::_ -> x
    | _ -> failwith (Printf.sprintf "Internal error: Json from scan script missing element: %s" member)
  in
  [json]
  |> filter_member member
  |> filter_string
  |> safe_hd

let extract_local_usb_info usb =
  let open USB in
  {
    path = extract_member usb "path";
    vendor_id = extract_member usb "vendor-id";
    vendor_desc = extract_member usb "vendor-desc";
    product_id = extract_member usb "product-id";
    product_desc = extract_member usb "product-desc";
    serial = extract_member usb "serial";
    version = extract_member usb "version";
    description = extract_member usb "description";
  }

let extract_known_usb_info usb =
  let open USB in
  {
    path = usb.API.pUSB_path;
    vendor_id = usb.API.pUSB_vendor_id;
    vendor_desc = usb.API.pUSB_vendor_desc;
    product_id = usb.API.pUSB_product_id;
    product_desc = usb.API.pUSB_product_desc;
    serial = usb.API.pUSB_serial;
    version = usb.API.pUSB_version;
    description = usb.API.pUSB_description;
  }

let get_known_usb usbs =
  List.map (fun (_, usb) -> extract_known_usb_info usb) usbs
  |> USBSet.of_list

let get_local_usb usbs =
  List.map (fun usb -> extract_local_usb_info usb) usbs
  |> USBSet.of_list

let get_script_stdout () =
  (** usb_scan is a script that will get all the usb details in current host, which will return json format data *)
  let usb_scan_script = "/opt/xensource/libexec/usb_scan.py" in
  try
    let stdout, stderr = Forkhelpers.execute_command_get_output usb_scan_script [] in
    stdout
  with Forkhelpers.Spawn_internal_error(stdout, stderr, Unix.WEXITED n) ->
    raise Api_errors.(Server_error(internal_error, [Printf.sprintf "%s exitted with %d" usb_scan_script n]))

let get_usbs stdout =
  let extract_devices json =
    [json]
    |> flatten
  in
  let usbs =
    try
      let json = Yojson.Basic.from_string stdout in
      extract_devices json
    with
    | Type_error (msg, js) ->
      failwith (Printf.sprintf "%s %s in %s" msg (to_string js) stdout)
    | e ->
      failwith (Printf.sprintf "%s in %s" (Printexc.to_string e)  stdout)
  in
  usbs

let find_or_create ~__context pusb =
  let pusb_rec = Db.PUSB.get_record_internal ~__context ~self:pusb in
  let name_label = "Group of " ^ pusb_rec.Db_actions.pUSB_vendor_id ^ " " ^ pusb_rec.Db_actions.pUSB_product_id ^ " USBs" in
  let group = Xapi_usb_group.create ~__context ~name_label ~name_description:"" ~other_config:[] in
  group

let destroy_pusb ~__context pusb =
  let usb_group = Db.PUSB.get_USB_group ~__context ~self:pusb in
  let vusbs = Db.USB_group.get_VUSBs ~__context ~self:usb_group in
  List.iter (fun vusb ->
      let currently_attached = Db.VUSB.get_currently_attached ~__context ~self:vusb in
      if currently_attached then
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Client.VUSB.unplug rpc session_id vusb);
      Db.VUSB.destroy ~__context ~self:vusb
    ) vusbs;
  Db.PUSB.destroy ~__context ~self:pusb;
  Db.USB_group.destroy ~__context ~self:usb_group
