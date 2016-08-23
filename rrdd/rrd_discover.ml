(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

(* This is a small stand-alone utility to test the RRD plugin mechanism.
 * It watches a directory and recognises new plugins that are writing
 * data there. The implementation is based on the inotify family of
 * system calls.
 *
 * rrd_discover /some/dir
 *
 * This could be turned into a daemon that registers plugins using
 * xcp-idl.
 *)

open Rrd_protocol

let sprintf     = Printf.sprintf

let printf fmt =
	Printf.kprintf (fun msg -> print_endline msg; flush stdout) fmt

let finally opn cls =
	let res = try opn () with exn -> cls (); raise exn
	in
		cls ();
		res

let events_as_string
	: Inotify.event_kind list -> string
	= fun es ->
	es
	|> List.map Inotify.string_of_event_kind
	|> String.concat ","

let v2 = Rrd_protocol_v2.protocol

let owner_as_string = function
	| Rrd.VM vm -> vm
	| Rrd.Host  -> "host"
	| Rrd.SR sr -> sr

let print_ds (owner, src) =
	( printf "Owner:      %s" (owner_as_string owner)
	; printf "Datasource: %s" src.Ds.ds_name
	; printf "--"
	)

let print file payload =
	( printf "File:       %s" file
	; printf "Time:       %Ld" payload.Rrd_protocol.timestamp
	; List.iter print_ds payload.Rrd_protocol.datasources
	)

(** [read file] attempts to read an rrd file written by a plugin at
 * [path]. It returns an option with None if this does not succeed.
 *)
let read
	: string -> Rrd_protocol.payload option
	= fun file ->
	try
		let reader = Rrd_reader.FileReader.create file v2 in
		finally
			(fun () ->
				try
					Some (reader.Rrd_reader.read_payload ())
				with _ -> None)
			(fun () ->
				reader.Rrd_reader.cleanup ())
	with
		_ -> None

(* [register file] is called when we found a new file in the watched
 * directory *)
let register file =
	match read file with
	| None        -> printf "%-12s is not an RRD plugin" file
	| Some plugin -> print file plugin

(* [deregister file] is called when a file is removed from the watched
 * directory *)
let deregister file =
	printf "deregister %s" file

(* [discover] scans a directory for plugins *)
let discover dir =
	Sys.readdir dir
	|> Array.to_list
	|> List.map (Filename.concat dir)
	|> List.iter register


(** [watch dir] monitors a directory [dir] for plugins *)
let watch dir =
	let (//)        = Filename.concat in
	let fd          = Inotify.create () in
	let selectors   = [Inotify.S_Create; Inotify.S_Delete] in
	let rec loop = function
		| []  -> Inotify.read fd |> loop
		| (_, [Inotify.Create], _, Some file)::es ->
			( register (dir//file)
			; loop es
			)
		| (_, [Inotify.Delete], _, Some file)::es ->
			( deregister (dir//file)
			; loop es
			)
		| (_, events, _, None)::es ->
			( printf "Ignoring: %s" (events_as_string events)
			; loop es
			)
		| (_, events, _, Some file)::es ->
			( printf "Ignoring: %s: %s" file (events_as_string events)
			; loop es
			)
	in
		( Inotify.add_watch fd dir selectors |> ignore
		; loop []
		)

let stop signal =
	printf "caught signal %d" signal;
	exit 1

(* main function *)
let main () =
	Sys.set_signal Sys.sigterm (Sys.Signal_handle stop);
	Coverage.init "rrd-discover";
	let args = Array.to_list Sys.argv |> List.tl in
	let this = Sys.executable_name in
	match args with
	| [dir]   -> discover dir; watch dir
	| _       ->
			( List.iter print_endline
				[ Printf.sprintf "Usage: %s /some/dir" this
				; ""
				; "This command watches a directory for the addition and removal"
				; "of RRD plugins. It reports all files it finds at startup and"
				; "any additions and removals while it is running."
				]
			; exit 1
			)

let () = if !Sys.interactive then () else main ()
