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

let create ~__context ~name_label ~name_description ~other_config =
  let group = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.USB_group.create ~__context ~ref:group ~uuid ~name_label ~name_description ~other_config;
  group

let destroy ~__context ~self =
  let vusbs = Db.USB_group.get_VUSBs ~__context ~self in
  let connected = List.filter (fun self ->
      Db.VUSB.get_currently_attached ~__context ~self
    ) vusbs in
  if connected <> [] then
    raise (Api_errors.Server_error (Api_errors.usb_group_contains_vusb, List.map Ref.string_of connected));
  let pusbs = Db.USB_group.get_PUSBs ~__context ~self in
  if pusbs <> [] then
    raise (Api_errors.Server_error (Api_errors.usb_group_contains_pusb, List.map Ref.string_of pusbs));

  (* Destroy all vUSBs *)
  List.iter (fun vusb ->
      Helpers.log_exn_continue (Printf.sprintf "destroying VUSB: %s" (Ref.string_of vusb))
        (fun vusb -> Db.VUSB.destroy ~__context ~self:vusb) vusb) vusbs;

  Db.USB_group.destroy ~__context ~self
