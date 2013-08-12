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

let assert_VGPU_type_enabled ~__context ~self ~value =
	let enabled_VGPU_types =
		Db.PGPU.get_enabled_VGPU_types ~__context ~self
	in
	if not (List.mem value enabled_VGPU_types)
	then raise (Api_errors.Server_error
		(Api_errors.vgpu_type_not_enabled,
			List.map Ref.string_of (value :: enabled_VGPU_types)))

let assert_VGPU_type_supported ~__context ~self ~value =
	let supported_VGPU_types =
		Db.PGPU.get_supported_VGPU_types ~__context ~self
	in
	if not (List.mem value supported_VGPU_types)
	then raise (Api_errors.Server_error
		(Api_errors.vgpu_type_not_supported,
			List.map Ref.string_of (value :: supported_VGPU_types)))

let assert_VGPU_type_allowed ~__context ~self ~value =
	let resident_VGPUs = Db.PGPU.get_resident_VGPUs ~__context ~self in
	(match resident_VGPUs with
	| [] -> ()
	| resident_VGPU :: _ ->
		let running_type =
			Db.VGPU.get_type ~__context ~self:resident_VGPU
		in
		if running_type <> value
		then raise (Api_errors.Server_error (
			Api_errors.vgpu_type_not_compatible_with_running_type,
			[
				Ref.string_of self;
				Ref.string_of value;
				Ref.string_of running_type;
			])))

let assert_no_resident_VGPUs_of_type ~__context ~self ~vgpu_type =
	let open Db_filter_types in
	match Db.VGPU.get_records_where ~__context
		~expr:(And
			(Eq (Field "resident_on", Literal (Ref.string_of self)),
			Eq (Field "vgpu_type", Literal (Ref.string_of vgpu_type))))
	with
	| [] -> ()
	| vgpus_and_records ->
		let vms =
			List.map
				(fun (vgpu, _) -> Db.VGPU.get_VM ~__context ~self:vgpu)
				vgpus_and_records
		in
		raise (Api_errors.Server_error
			(Api_errors.pgpu_in_use_by_vm, List.map Ref.string_of vms))
