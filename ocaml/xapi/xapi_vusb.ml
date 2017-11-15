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

open Stdext
open Listext
open Threadext
module D = Debug.Make(struct let name="xapi" end)
open D

let m = Mutex.create ()

let create ~__context ~vM ~uSB_group ~other_config =
  let vusb = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Pool_features.assert_enabled ~__context ~f:Features.USB_passthrough;
  Mutex.execute m (fun () ->
    let attached_vusbs = Db.VM.get_VUSBs ~__context ~self:vM in
    (* At most 6 VUSBS can be attached to one vm *)
    if List.length attached_vusbs > 5 then
      raise (Api_errors.Server_error (Api_errors.too_many_vusbs, ["6"]));
    let vusbs = Db.USB_group.get_VUSBs ~__context ~self:uSB_group in
    (* Currently USB_group only have one PUSB. So when vusb is created with a USB_group,
        another vusb can not create with the same USB_group. *)
    if vusbs <> [] then
      raise (Api_errors.Server_error(Api_errors.usb_group_conflict, [Ref.string_of uSB_group]));

    (* All USBs that will be attached to the VM must be on the same host, as USB from host A can
      not attach to the vm resident on host B. *)
    let possible_hosts = Xapi_vm.get_possible_hosts ~__context ~vm:vM in
    let pusb = Helpers.get_first_pusb ~__context uSB_group in
    let host = Db.PUSB.get_host ~__context ~self:pusb in
    if not (List.mem host possible_hosts) then
      raise (Api_errors.Server_error (Api_errors.pusb_not_in_possible_hosts, [Ref.string_of pusb; String.concat ";" (List.map Ref.string_of possible_hosts)]));
    (* We won't attach VUSB when VM ha_restart_priority is set to 'restart'  *)
    let ha_restart_priority = Db.VM.get_ha_restart_priority ~__context ~self:vM in
    match ha_restart_priority with
    | hp when hp = Constants.ha_restart -> raise (Api_errors.Server_error(Api_errors.operation_not_allowed,
      [Printf.sprintf "VM %s ha_restart_priority has been set to 'restart', can not create VUSB for it. " (Ref.string_of vM)]))
    | _ ->
      Db.VUSB.create ~__context ~ref:vusb ~uuid ~current_operations:[] ~allowed_operations:[] ~vM ~uSB_group
        ~other_config ~currently_attached:false;
      debug "VUSB ref='%s' created VM = '%s'" (Ref.string_of vusb) (Ref.string_of vM);
      vusb
  )

let unplug ~__context ~self =
  Xapi_xenops.vusb_unplug ~__context ~self

let destroy ~__context ~self =
  debug "VUSB.destroy (uuid = %s; ref = %s)" (Db.VUSB.get_uuid ~__context ~self) (Ref.string_of self);
  let r = Db.VUSB.get_record_internal ~__context ~self in
  let vm = r.Db_actions.vUSB_VM in
  (* Force the user to unplug first *)
  if r.Db_actions.vUSB_currently_attached then
    raise (Api_errors.Server_error(Api_errors.operation_not_allowed,
                                      [Printf.sprintf "VUSB '%s' still attached to '%s'" r.Db_actions.vUSB_uuid (Db.VM.get_uuid __context vm)]));
  Db.VUSB.destroy ~__context ~self
