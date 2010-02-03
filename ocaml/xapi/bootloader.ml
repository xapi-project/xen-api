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

open Stringext
open Pervasiveext
open Forkhelpers

module D=Debug.Debugger(struct let name="bootloader" end)
open D


exception Error of string

(** Helper function to generate a bootloader commandline *)
let bootloader_args q extra_args legacy_args pv_bootloader_args image vm_uuid = 
  (* Let's not do anything fancy while parsing the pv_bootloader_args string:
     no escaping of spaces or quotes for now *)
  let pv_bootloader_args = if pv_bootloader_args = "" then [] else String.split ' ' pv_bootloader_args in

  let rules = [ '"', "\\\""; '\\', "\\\\" ] in
  [ if q then "-q" else "";
    Printf.sprintf "--default_args=%s" (String.escaped ~rules legacy_args);
    Printf.sprintf "--extra_args=%s" (String.escaped ~rules extra_args);
    Printf.sprintf "--vm=%s" vm_uuid;
  ] @ pv_bootloader_args @ [
    image ]

(** Parsed representation of bootloader's stdout, as used by xend (XXX: need HVM) *)
type extracted_kernel = {
  kernel_path: string;
  initrd_path: string option;
  kernel_args: string;
}

let parse_output x = 
  let sexpr = "(" ^ x ^ ")" in
  let parse_failed = Error(Printf.sprintf "Expecting an s-expression; received: %s" sexpr) in
  let sexpr' = SExpr_TS.of_string sexpr in
  match sexpr' with
    (* linux (kernel /var/lib/xen/vmlinuz.y1Wmrp)(args 'root=/dev/sda1 ro') *)
    (* linux (kernel /var/lib/xen/vmlinuz.SFO5fb)(ramdisk /var/lib/xen/initrd.MUitgP)(args 'root=/dev/sda1 ro') *)
    | SExpr.Node (SExpr.Symbol "linux" :: list) ->
	let l = List.map (function
	   | SExpr.Node [ SExpr.Symbol x; SExpr.Symbol y | SExpr.String y ] -> (x,y)
	   | _                                                              -> raise parse_failed) list in
	{ kernel_path = List.assoc "kernel" l;
	  initrd_path = (try Some (List.assoc "ramdisk" l) with _ -> None);
	  kernel_args = (try List.assoc "args" l with _ -> "") }
    | _ -> 
	debug "Failed to parse: %s" sexpr;
	raise parse_failed

let parse_exception x = 
  match Stringext.String.split '\n' x with
  | code :: params -> raise (Api_errors.Server_error(code, params))
  | _ -> failwith (Printf.sprintf "Failed to parse stderr output of bootloader: %s" x)

(** Extract the default kernel using the -q option *)
let extract_default_kernel bootloader disks legacy_args extra_args pv_bootloader_args vm_uuid =
  let bootloader_path = List.assoc bootloader Xapi_globs.supported_bootloaders in
  if List.length disks = 0 then
    raise (Error("no bootable disk"));
  if List.length disks > 1 then
    raise (Error(Printf.sprintf "too many bootable disks (%d disks)" (List.length disks)));
  let disk = List.hd disks in
  let cmdline = bootloader_args true extra_args legacy_args pv_bootloader_args disk vm_uuid in
  debug "Bootloader commandline: %s %s\n" bootloader_path (String.concat " " cmdline);
  try
	parse_output (Helpers.call_script ~log_successful_output:false bootloader_path cmdline)
  with Forkhelpers.Spawn_internal_error(stderr, stdout, _) ->
	  parse_exception stderr

let delete_extracted_kernel x = 
  Unix.unlink x.kernel_path;
  match x.initrd_path with
  | None -> ()
  | Some x -> Unix.unlink x
