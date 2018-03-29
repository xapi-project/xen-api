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
(**
 * @group Storage
*)

module D = Debug.Make(struct let name="xapi" end)
open D

module L = Debug.Make(struct let name="license" end)


(** Thrown if an empty VBD which isn't a CDROM is attached to an HVM guest *)
exception Only_CD_VBDs_may_be_empty


let translate_vbd_device vbd_ref name is_hvm =
  try
    let i = Device_number.of_string is_hvm name in
    debug "VBD device name %s interpreted as %s (hvm = %b)" name (Device_number.to_debug_string i) is_hvm;
    i
  with _ ->
    raise (Api_errors.Server_error(Api_errors.illegal_vbd_device, [ Ref.string_of vbd_ref; name ]))

