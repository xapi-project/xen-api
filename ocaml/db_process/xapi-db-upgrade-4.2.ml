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
(* 
 * Helpers for upgrading from a normal XML database to a protected XML database, where multiple spaces, '\t', '\n', '\r' and '\'
 * are protected inside attributes.
 *)

(* We assume here that the upgrade done by host-installer creates a XML file where only double quotes are used around attributes. *)
(* This is normally done internally by the miami version of xapi-db-process, so it should be OK. *)
let perform_inside_quotes_fn f inside_quotes = function
	| ('"', _)              -> inside_quotes := not (!inside_quotes); 
	                           Xml_spaces.No_change
	| c when !inside_quotes -> f c
	| _                     -> Xml_spaces.No_change

let (_:unit) =
	(* By default, we upgrade the database, ie. we protect multiple spaces and so on.                    *)
	(* However, in case something went wrong, we can downgrade the database using '--downgrade' argument *)
	let fn_to_apply = 
		let inside_quotes = ref false in
		if Array.length Sys.argv = 2 && Sys.argv.(1) = "--downgrade"
		then begin
			Printf.printf "Warning: unprotecting characters of the XML database\n%!";
			perform_inside_quotes_fn Xml_spaces.unprotect_fn inside_quotes;
		end else 
			perform_inside_quotes_fn Xml_spaces.protect_fn inside_quotes
	in
	let paths = Parse_db_conf.parse_db_conf !Xapi_globs.db_conf_path in
	List.iter 
		(fun dbconn ->
			let path = dbconn.Parse_db_conf.path in
			(* first, fit the database into memory *)
			let state_db = Unixext.string_of_file path in
			(* second, save the original database in a backup file, using a timestamp *)
			Unixext.write_string_to_file (path ^ ".prev_version." ^ (string_of_float (Unix.gettimeofday()))) state_db;
			(* finally, transform the database and replace the database file by a new one *)
			let result = Xml_spaces.map2_unlikely fn_to_apply state_db in
			(* Remark: Unixext.write_string_to_file is atomic *)
			Unixext.write_string_to_file path result)
		paths
