(*
 * Copyright (c) Cloud Software Group
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

module D = Debug.Make (struct let name = "storage_smapiv1_migrate" end)

module Unixext = Xapi_stdext_unix.Unixext
module State = Storage_migrate_helper.State
module SXM = Storage_migrate_helper.SXM

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

module MIRROR : SMAPIv2_MIRROR = struct
  type context = unit

  let u x = raise Storage_interface.(Storage_error (Errors.Unimplemented x))

  let send_start _ctx = u __FUNCTION__

  let receive_start _ctx = u __FUNCTION__

  let receive_start2 _ctx = u __FUNCTION__

  let receive_finalize _ctx = u __FUNCTION__

  let receive_finalize2 _ctx = u __FUNCTION__

  let receive_cancel _ctx = u __FUNCTION__
end
