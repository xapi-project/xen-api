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

let clear_log level key =
	let clear_f =
		if key = "" then
			Logs.clear_default
		else
			Logs.clear key in
	if level = "" then (
		List.iter (fun level -> clear_f level)
			[ Log.Error; Log.Warn; Log.Info; Log.Debug ]
	) else (
		let loglevel = match level with
			| "debug" -> Log.Debug
			| "info"  -> Log.Info
			| "warn"  -> Log.Warn
			| "error" -> Log.Error
			| s       -> failwith (sprintf "Unknown log level: %s" s) in
		clear_f loglevel
	)

let append_log level key logger =
	(* if key is empty, append to the default logger *)
	let append =
		if key = "" then
			Logs.append_default
		else
			Logs.append key in
	(* if level is empty, append to all level *)
	if level = "" then (
		List.iter (fun level -> append level logger)
			[ Log.Error; Log.Warn; Log.Info; Log.Debug ]
	) else (
		let loglevel = match level with
			| "debug" -> Log.Debug
			| "info"  -> Log.Info
			| "warn"  -> Log.Warn
			| "error" -> Log.Error
			| s       -> failwith (sprintf "Unknown log level: %s" s) in
		append loglevel logger
	)

let read_log_config filename =
	let trim_end lc s =
		let i = ref (String.length s - 1) in
		while !i > 0 && (List.mem s.[!i] lc)
		do
			decr i
		done;
		if !i >= 0 then String.sub s 0 (!i + 1) else ""
	in
	Unixext.readfile_line
		(fun line ->
			 let line = trim_end [ ' '; '\t' ] line in
			 if String.startswith "#" line then
				 ()
			 else
				 let ls = String.split ~limit:3 ';' line in
				 match ls with
					 | [ "reset" ] ->
						 Logs.reset_all []
					 | [ level; key; "clear" ] ->
						 clear_log level key
					 | [ level; key; logger ] ->
						 append_log level key logger
					 | _ ->
						 ()
		) filename

let read_config filename =
	let configargs = [
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
	(* debug "License filename: %s" !License_file.filename *)
