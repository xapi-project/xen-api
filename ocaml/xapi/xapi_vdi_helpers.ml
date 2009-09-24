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

(* We only support .iso files (from an iso SR) and block devices from
   a local magic SR (eg /dev/hda) but NOT phantom_vbd block attach isos *)
let assert_vdi_is_valid_iso ~__context ~vdi = 
	let sr = Db.VDI.get_SR ~__context ~self:vdi in
	let ct = Db.SR.get_content_type ~__context ~self:sr in
	if ct <> "iso"
	then raise (Api_errors.Server_error(Api_errors.vdi_is_not_iso, [ Ref.string_of vdi; ct ]))

(* CA-26514: Block operations on 'unmanaged' VDIs *)
let assert_managed ~__context ~vdi = 
  if not (Db.VDI.get_managed ~__context ~self:vdi)
  then raise (Api_errors.Server_error(Api_errors.vdi_not_managed, [ Ref.string_of vdi ]))

