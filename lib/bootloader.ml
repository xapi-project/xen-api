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
  | Pygrub -> !Resources.pygrub
  | Eliloader -> !Resources.eliloader

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
  let pv_bootloader_args = if pv_bootloader_args = "" then [] else Stdext.Xstringext.String.split ' ' pv_bootloader_args in
  let q = if q then [ "-q" ] else [] in
  let vm = [ "--vm"; vm_uuid ] in
  let image = [ image ] in
  match bootloader_of_string bootloader with
  | Some Pygrub ->
    let args = [
      ["--output-format=simple"];
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

(* The string to parse comes from eliloader or pygrub, which builds it based on
 * reading and processing the grub configuration from the guest's disc.
 * Therefore it may contain malicious content from the guest if pygrub has not
 * cleaned it up sufficiently. *)
(* Example of a valid three-line string to parse, with blank third line:
 * kernel <kernel:/vmlinuz-2.6.18-412.el5xen>
 * args ro root=/dev/VolGroup00/LogVol00 console=ttyS0,115200n8
 *
*)
type acc_t = {kernel: string option; ramdisk: string option; args: string option}
let parse_output_simple x =
  let parse_line_optimistic acc l =
    (* String.index will raise Not_found on the empty line that pygrub includes
     * at the end of its simple-format output. *)
    let space_pos = String.index l ' ' in
    let first_word = String.sub l 0 space_pos in
    let pos = space_pos + 1 in
    match first_word with
    | "kernel" -> (
        match acc.kernel with
        | Some _ -> raise (Bad_error ("More than one kernel line when parsing bootloader result: "^x))
        | None ->
          debug "Using kernel line from bootloader output: %s" l;
          {acc with kernel = Some (String.sub l pos (String.length l - pos))} )
    | "ramdisk" -> (
        match acc.ramdisk with
        | Some _ -> raise (Bad_error ("More than one ramdisk line when parsing bootloader result: "^x))
        | None ->
          debug "Using ramdisk line from bootloader output: %s" l;
          {acc with ramdisk = Some (String.sub l pos (String.length l - pos))} )
    | "args" -> (
        match acc.args with
        | Some _ -> raise (Bad_error ("More than one args line when parsing bootloader result: "^x))
        | None ->
          debug "Using args line from bootloader output: %s" l;
          {acc with args = Some (String.sub l pos (String.length l - pos))} )
    | "" -> acc
    | _ -> raise (Bad_error ("Unrecognised start of line when parsing bootloader result: line="^l))
  in
  let parse_line acc l =
    try parse_line_optimistic acc l
    with Not_found -> acc
  in
  let linelist = Stdext.Xstringext.String.split '\n' x in
  let content = List.fold_left parse_line {kernel=None; ramdisk=None; args=None} linelist in
  {
    kernel_path = (match content.kernel with
        | None -> raise (Bad_error ("No kernel found in "^x))
        | Some p -> p);
    initrd_path = content.ramdisk;
    kernel_args = (match content.args with
        | None -> ""
        | Some a -> a)
  }

let runtimeError = "RuntimeError: "

let parse_exception x =
  debug "Bootloader failed: %s" x;
  (* Look through the error for the prefix "RuntimeError: " - raise an exception with a message
     	 * containing the error from the end of this prefix onwards. *)
  let l = String.length runtimeError in
  match Stdext.Xstringext.String.find_all runtimeError x with
  | i::_ ->
    raise (Error_from_bootloader (String.sub x (i+l) (String.length x - i - l)))
  | _ -> (* no expected prefix *)
    raise (Bad_error x)

(* A layer of defence against the chance of a malicious guest grub config tricking
 * pygrub or eliloader into giving the guest access to an inappropriate file in dom0 *)
let sanity_check_path p = match p with
  | "" -> p
  | p when Filename.is_relative p ->
    raise (Bad_error ("Bootloader returned a relative path for kernel or ramdisk: "^p))
  | p ->
    let canonical_path = Stdext.Unixext.resolve_dot_and_dotdot p in
    match Filename.dirname canonical_path with
    | "/var/run/xen/pygrub" (* From pygrub, including when called by eliloader *)
    | "/var/run/xend/boot" (* From eliloader *)
      -> canonical_path
    | _ -> raise (Bad_error ("Malicious guest? Bootloader returned a kernel or ramdisk path outside the allowed directories: "^p))

(** Extract the default kernel using the -q option *)
let extract (task: Xenops_task.task_handle) ~bootloader ~disk ?(legacy_args="") ?(extra_args="") ?(pv_bootloader_args="") ~vm:vm_uuid () =
  (* Without this path, pygrub will fail: *)
  Unixext.mkdir_rec "/var/run/xend/boot" 0o0755;
  let bootloader_path, cmdline = command bootloader true pv_bootloader_args disk vm_uuid in
  debug "Bootloader commandline: %s %s\n" bootloader_path (String.concat " " cmdline);
  try
    let output, _ = Cancellable_subprocess.run task [] bootloader_path cmdline in
    debug "Bootloader output: %s" output;
    let result = parse_output_simple output in
    {
      kernel_path = sanity_check_path result.kernel_path;
      initrd_path = (match result.initrd_path with
          | None -> None
          | Some p -> Some (sanity_check_path p));
      kernel_args = Printf.sprintf "%s %s %s" result.kernel_args legacy_args extra_args
    }
  with Forkhelpers.Spawn_internal_error(stderr, stdout, _) ->
    parse_exception stderr

let delete x =
  Unix.unlink x.kernel_path;
  match x.initrd_path with
  | None -> ()
  | Some x -> Unix.unlink x
