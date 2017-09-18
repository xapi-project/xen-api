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
  Mutex.execute m (fun () ->
      Db.VUSB.create ~__context ~ref:vusb ~uuid ~current_operations:[] ~allowed_operations:[] ~vM ~uSB_group
      ~other_config ~attached:Ref.null;
  );
  debug "VUSB ref='%s' created VM = '%s'" (Ref.string_of vusb) (Ref.string_of vM);
  vusb

let unplug ~__context ~self =
  debug "unplug vusb to do"

let destroy ~__context ~self =
  debug "VUSB.destroy (uuid = %s; ref = %s)" (Db.VUSB.get_uuid ~__context ~self) (Ref.string_of self);
  let r = Db.VUSB.get_record_internal ~__context ~self in
  let vm = r.Db_actions.vUSB_VM in
  (* Force the user to unplug first *)
  if Helpers.is_running ~__context ~self:vm  && r.Db_actions.vUSB_attached <> Ref.null then
   raise (Api_errors.Server_error(Api_errors.operation_not_allowed,
                                      [Printf.sprintf "VUSB '%s' still attached to '%s'" r.Db_actions.vUSB_uuid (Db.VM.get_uuid __context vm)]));
  Db.VUSB.destroy ~__context ~self
