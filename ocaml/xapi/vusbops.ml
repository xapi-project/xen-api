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
module D=Debug.Make(struct let name="vusbops" end)
open D

open Stdext
open Listext

type vusb = {
  vusb_ref: API.ref_VUSB;
  usb_group_ref: API.ref_USB_group;
  other_config: (string * string) list;
}

let vusb_of_vusb ~__context vm_r vusb =
  let vusb_r = Db.VUSB.get_record ~__context ~self:vusb in
  {
    vusb_ref = vusb;
    usb_group_ref = vusb_r.API.vUSB_USB_group;
    other_config = vusb_r.API.vUSB_other_config;
  }

let vusbs_of_vm ~__context vm_r =
  List.map (vusb_of_vusb ~__context vm_r) vm_r.API.vM_VUSBs

let allocate_vusb_to_usb ~__context vm host vusb =
  let available_pusbs = Db.Host.get_PUSBs ~__context ~self:host in
  let compatible_pusbs = Db.USB_group.get_PUSBs ~__context ~self:vusb.usb_group_ref in
  let pusbs = List.intersect compatible_pusbs available_pusbs in

  (** We currently only have one pusb in usb_group. So compatible_pusbs will only have one PUSB,
    and pusbs will have no more than one PUSB. *)
  match pusbs with
  | [] -> raise (Api_errors.Server_error (Api_errors.vm_requires_vusb, [ Ref.string_of vm; Ref.string_of vusb.usb_group_ref;]))
  | pusb :: _ ->
    Db.VUSB.set_attached ~__context ~self:vusb.vusb_ref ~value:pusb;
    Db.PUSB.set_attached ~__context ~self:pusb ~value:vusb.vusb_ref

let add_vusbs_to_vm ~__context host vm vusbs =
  match vusbs with
  | [] -> ()
  | (v :: vs) as vusbs' -> List.iter (fun vusb -> allocate_vusb_to_usb ~__context vm host vusb) vusbs'

(* Note that this function is called from Message_forwarding.allocate_vm_to_host,
 * only on the pool master, and with the global lock held. We therefore do not
 * need any further locking in any of the functions above, where resources are
 * reserved. *)
let create_vusbs ~__context host (vm, vm_r) hvm =
  let vusbs = vusbs_of_vm ~__context vm_r in
  if vusbs <> [] && not hvm then
      raise (Api_errors.Server_error (Api_errors.feature_requires_hvm, ["VUSB- and USB-passthrough needs HVM"]));
  add_vusbs_to_vm ~__context host vm vusbs
