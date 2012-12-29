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

(* TODO:
   1. Modify pygrub to extract all possible boot options
   2. Parse the results into some kind of option list
   3. Ensure all our guests have complete grub menu.lst (no hacks please!)
   4. Add support to control a slave screen process, to make a 'bios'
*)

open String
open Xenops_utils
open Xenops_task

module D=Debug.Make(struct let name="bootloader" end)
open D

let pygrub_path = "/usr/bin/pygrub"
let eliloader_path = "/usr/bin/eliloader"
let supported_bootloader_paths = [
	"pygrub", pygrub_path;
	"eliloader", eliloader_path
]
let supported_bootloaders = List.map fst supported_bootloader_paths

exception Bad_sexpr of string

exception Bad_error of string

exception Unknown_bootloader of string

exception Error_from_bootloader of string

type t = {
  kernel_path: string;
  initrd_path: string option;
  kernel_args: string;
}

(** Helper function to generate a bootloader commandline *)
let bootloader_args q extra_args legacy_args pv_bootloader_args image vm_uuid = 
  (* Let's not do anything fancy while parsing the pv_bootloader_args string:
     no escaping of spaces or quotes for now *)
  let pv_bootloader_args = if pv_bootloader_args = "" then [] else Re_str.split (Re_str.regexp "[ ]") pv_bootloader_args in

  let rules = [ '"', "\\\""; '\\', "\\\\" ] in
  [ if q then "-q" else "";
    Printf.sprintf "--default_args=%s" (String.escaped ~rules legacy_args);
    Printf.sprintf "--extra_args=%s" (String.escaped ~rules extra_args);
    Printf.sprintf "--vm=%s" vm_uuid;
  ] @ pv_bootloader_args @ [
    image ]

let parse_output x = 
  let sexpr = "(" ^ x ^ ")" in
  let sexpr' = SExpr_TS.of_string sexpr in
  match sexpr' with
    (* linux (kernel /var/lib/xen/vmlinuz.y1Wmrp)(args 'root=/dev/sda1 ro') *)
    (* linux (kernel /var/lib/xen/vmlinuz.SFO5fb)(ramdisk /var/lib/xen/initrd.MUitgP)(args 'root=/dev/sda1 ro') *)
    | SExpr.Node (SExpr.Symbol "linux" :: list) ->
	let l = List.map (function
	   | SExpr.Node [ SExpr.Symbol x; SExpr.Symbol y | SExpr.String y ] -> (x,y)
	   | _ -> raise (Bad_sexpr sexpr)) list in
	{
		kernel_path = List.assoc "kernel" l;
		initrd_path = (try Some (List.assoc "ramdisk" l) with _ -> None);
		kernel_args = (try List.assoc "args" l with _ -> "") }
    | _ -> 
		debug "Failed to parse: %s" sexpr;
		raise (Bad_sexpr sexpr)

let parse_exception x =
	debug "Bootloader failed: %s" x;
	let lines = Re_str.split (Re_str.regexp "[\n]") x in
	(* Since the error is a python stack trace, the last-but-one line tends to be the most useful. *)
	let err = try List.nth (List.rev lines) 1 with _ -> raise (Bad_error x) in
	raise (Error_from_bootloader err)

(** Extract the default kernel using the -q option *)
let extract (task: Xenops_task.t) ~bootloader ~disk ?(legacy_args="") ?(extra_args="") ?(pv_bootloader_args="") ~vm:vm_uuid () =
	if not(List.mem_assoc bootloader supported_bootloader_paths)
	then raise (Unknown_bootloader bootloader);
	let bootloader_path = List.assoc bootloader supported_bootloader_paths in
	let cmdline = bootloader_args true extra_args legacy_args pv_bootloader_args disk vm_uuid in
	debug "Bootloader commandline: %s %s\n" bootloader_path (String.concat " " cmdline);
	try
		let output, _ = Cancel_utils.cancellable_subprocess task bootloader_path cmdline in
		parse_output output
	with Forkhelpers.Spawn_internal_error(stderr, stdout, _) ->
		parse_exception stderr

let delete x =
  Unix.unlink x.kernel_path;
  match x.initrd_path with
  | None -> ()
  | Some x -> Unix.unlink x
