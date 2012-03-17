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

(* Note: this used to be in Helpers; moved due to cyclic dependencies relating to License *)

open Xapi_globs
open Printf
open Stringext

module D=Debug.Debugger(struct let name="xapi" end)
open D

let read_config filename =
	let configargs = [
		"use-xenopsd", Config.Set_bool Xapi_globs.use_xenopsd;
		"disable-logging-for", Config.String
			(fun x ->
				try
					let modules = String.split_f String.isspace x in
					List.iter
						(fun x ->
							debug "Disabling logging for: %s" x;
							Debug.disable x
						) modules
				with e ->
					error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
			)
	] in
	try
		Config.read filename configargs (fun _ _ -> ())
	with Config.Error ls ->
		List.iter (fun (p,s) ->
								 eprintf "config file error: %s: %s\n" p s) ls;
		exit 2

let log_if_not_empty format_string value =
	if value <> "" then debug format_string value

let dump_config () =
	debug "Server configuration:";
	log_if_not_empty "product_version: %s" Version.product_version;
	log_if_not_empty "product_brand: %s" Version.product_brand;
	debug "platform_version: %s" Version.platform_version;
	debug "platform_name: %s" Version.platform_name;
	debug "build_number: %s" Version.build_number;
	debug "git changeset: %s" Version.git_id;
	debug "version: %d.%d" version_major version_minor;
	debug "use-xenopsd: %b" !Xapi_globs.use_xenopsd
	(* debug "License filename: %s" !License_file.filename *)
