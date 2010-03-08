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
open Stringext

let xen_cmdline_script = "/opt/xensource/libexec/xen-cmdline"

let call_script args = 
	try
		let out, _ = Forkhelpers.execute_command_get_output xen_cmdline_script args in
		out
	with 
	| Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) ->
		failwith "Error while calling xen-cmdline script"
		
let list_cpuid_masks () =
	let masks = call_script ["--list-cpuid-masks"] in
	let masks = String.split '\n' masks in
	let masks = List.filter (fun m -> String.contains m '=') masks in
	List.map (fun m -> match String.split '=' m with k :: [v] -> k, v | _ -> failwith ("parse error: " ^ m)) masks

let set_cpuid_masks masks =
	call_script ("--set-cpuid-masks" :: (List.map (fun (k,v) -> k ^ "=" ^ v) masks))
	
let delete_cpuid_masks masks =
	List.map (fun mask -> call_script ("--delete-cpuid-masks" :: [mask])) masks
