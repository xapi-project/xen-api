(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
module D=Debug.Debugger(struct let name="xapi" end)
open D

let create ~__context  ~vM ~gPU_group ~device ~other_config =
	let vgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.VGPU.create ~__context ~ref:vgpu ~uuid ~vM ~gPU_group ~device ~currently_attached:false ~other_config;
	vgpu

let destroy ~__context ~self =
	Db.VGPU.destroy ~__context ~self

