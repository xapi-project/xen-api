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

type supported_bootloader =
  | Pygrub
  | Eliloader

let string_of_bootloader = function
  | Pygrub -> "pygrub" | Eliloader -> "eliloader"
let bootloader_of_string = function
  | "pygrub" -> Some Pygrub | "eliloader" -> Some Eliloader | _ -> None

let path_of_bootloader = function
  | Pygrub -> !Xc_path.pygrub
  | Eliloader -> !Xc_path.eliloader

let supported_bootloaders =  List.map string_of_bootloader [ Pygrub; Eliloader ]

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
let command bootloader q pv_bootloader_args image vm_uuid = 
  (* Let's not do anything fancy while parsing the pv_bootloader_args string:
     no escaping of spaces or quotes for now *)
  let pv_bootloader_args = if pv_bootloader_args = "" then [] else Re_str.split (Re_str.regexp "[ ]") pv_bootloader_args in
  let q = if q then [ "-q" ] else [] in
  let vm = [ "--vm"; vm_uuid ] in
  let image = [ image ] in
  match bootloader_of_string bootloader with
  | Some Pygrub ->
    let args = [
      q;
      (* --vm is unnecessary for pygrub and not supported upstream *)
      pv_bootloader_args;
      image;
    ] in
    path_of_bootloader Pygrub, List.concat args
  | Some Eliloader ->
    let args = [
      q;
      vm;
      pv_bootloader_args;
      image;
    ] in    
    path_of_bootloader Eliloader, List.concat args
  | None -> raise (Unknown_bootloader bootloader)

(* linux (kernel /var/lib/xen/vmlinuz.y1Wmrp)(args 'root=/dev/sda1 ro') *)
(* linux (kernel /var/lib/xen/vmlinuz.SFO5fb)(ramdisk /var/lib/xen/initrd.MUitgP)(args 'root=/dev/sda1 ro') *)
let kvpairs_of_string x = Sexplib.Sexp.(match scan_sexps (Lexing.from_string x) with
  | Atom "linux" :: list ->
    let rec to_list = function
    | List (Atom key :: values) :: rest ->
      let atoms = List.map (function Atom x -> x | _ -> raise (Bad_sexpr x)) values in
      let v = String.concat " " atoms in
      let v =
        if v.[0] = '\'' && v.[String.length v - 1] = '\''
        then String.sub v 1 (String.length v - 2)
        else v in
      (key, v) :: (to_list rest)
    | [] -> []
    | _ -> raise (Bad_sexpr x) in
		to_list list
	| _ -> raise (Bad_sexpr x)
)

let parse_output x =
  let l = kvpairs_of_string x in
	{
		kernel_path = List.assoc "kernel" l;
		initrd_path = (try Some (List.assoc "ramdisk" l) with _ -> None);
		kernel_args = (try List.assoc "args" l with _ -> "")
	}

let parse_exception x =
	debug "Bootloader failed: %s" x;
	let lines = Re_str.split (Re_str.regexp "[\n]") x in
	(* Since the error is a python stack trace, the last-but-one line tends to be the most useful. *)
	let err = try List.nth (List.rev lines) 1 with _ -> raise (Bad_error x) in
	raise (Error_from_bootloader err)

(** Extract the default kernel using the -q option *)
let extract (task: Xenops_task.t) ~bootloader ~disk ?(legacy_args="") ?(extra_args="") ?(pv_bootloader_args="") ~vm:vm_uuid () =
	(* Without this path, pygrub will fail: *)
	Unixext.mkdir_rec "/var/run/xend/boot" 0o0755;
	let bootloader_path, cmdline = command bootloader true pv_bootloader_args disk vm_uuid in
	debug "Bootloader commandline: %s %s\n" bootloader_path (String.concat " " cmdline);
	try
		let output, _ = Cancel_utils.cancellable_subprocess task bootloader_path cmdline in
		let result = parse_output output in
		{ result with kernel_args = Printf.sprintf "%s %s %s" result.kernel_args legacy_args extra_args }
	with Forkhelpers.Spawn_internal_error(stderr, stdout, _) ->
		parse_exception stderr

let delete x =
  Unix.unlink x.kernel_path;
  match x.initrd_path with
  | None -> ()
  | Some x -> Unix.unlink x
