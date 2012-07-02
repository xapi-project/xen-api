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

module D=Debug.Debugger(struct let name="v6api" end)
open D

let supported_editions = [Edition.Free]

let apply_edition dbg edition additional_params =
	let edition' = Edition.of_string edition in
	if List.mem edition' supported_editions then
		edition, Edition.to_features edition', []
	else
		failwith "unknown edition"
		
let get_editions dbg =
	List.map (fun e -> Edition.to_string e, Edition.to_marketing_name e,
		Edition.to_short_string e, Edition.to_int e) supported_editions

let get_version dbg =
	""

let reopen_logs () = true
