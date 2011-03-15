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

open Listext

let create ~__context ~pCI ~host =
	let pgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.PGPU.create ~__context ~ref:pgpu ~uuid ~pCI ~gPU_group:(Ref.null) ~host ~other_config:[];
	pgpu

let update_gpus ~__context ~host =
	let existing_pgpus = List.filter (fun (rf, rc) -> rc.API.pGPU_host = host) (Db.PGPU.get_all_records ~__context) in
	let class_id = Xapi_pci.find_class_id Xapi_pci.Display_controller in
	let pcis = List.filter (fun self ->
		Db.PCI.get_class_id ~__context ~self = class_id &&
		Db.PCI.get_host ~__context ~self = host) (Db.PCI.get_all ~__context)
	in
	let rec find_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let pgpu =
				try
					List.find (fun (rf, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus
				with Not_found ->
					let self = create ~__context ~pCI:pci ~host in
					self, Db.PGPU.get_record ~__context ~self
			in
			find_or_create (pgpu :: cur) remaining_pcis
	in
	let current_pgpus = find_or_create [] pcis in
	let obsolete_pgpus = List.set_difference existing_pgpus current_pgpus in
	List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus

