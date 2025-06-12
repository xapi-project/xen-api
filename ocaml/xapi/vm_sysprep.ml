(*
 * Copyright (c) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
open Xapi_stdext_unix

let ( // ) = Filename.concat

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let temp_dir = Filename.get_temp_dir_name ()

let sr_dir = "/var/opt/iso"

let genisoimage = "/usr/bin/genisoimage"

let failwith_fmt fmt = Printf.ksprintf failwith fmt

let prng = Random.State.make_self_init ()

let temp_name prefix suffix =
  let rnd = Random.State.bits prng land 0xFFFFFF in
  Printf.sprintf "%s%06x%s" prefix rnd suffix

(** [mkdtmp] creates a directory in [dir] and returns its path. If [dir]
    does not yet exist it is created. It is a an error if [dir] exists
    and is not a directory. *)
let mkdtemp ?(dir = temp_dir) ?(perms = 0o700) prefix suffix =
  ( match Sys.file_exists dir with
  | true when not (Sys.is_directory dir) ->
      failwith_fmt "s: %s is not a directory" __FUNCTION__ dir
  | true ->
      ()
  | false ->
      Unixext.mkdir_rec dir perms
  ) ;
  let rec loop = function
    | n when n >= 20 ->
        failwith_fmt "s: can't create directory in %s" __FUNCTION__ dir
    | n -> (
        let path = Filename.concat dir (temp_name prefix suffix) in
        try Sys.mkdir path perms ; path with Sys_error _ -> loop (n + 1)
      )
  in
  loop 0

(** Crteate a temporary directory, and pass its path to [f]. Once [f]
    returns the directory is removed again *)
let with_temp_dir ?(dir = temp_dir) ?(perms = 0o700) prefix suffix f =
  let dir = mkdtemp ~dir ~perms prefix suffix in
  finally (fun () -> f dir) (fun () -> Unixext.rm_rec dir)

(** name of the ISO we will use for a VMi; this is not a path *)
let iso_name ~vm_uuid =
  let now = Ptime_clock.now () |> Ptime.to_rfc3339 in
  Printf.sprintf "config-%s-%s.iso" vm_uuid now

(** Create an ISO in [sr_dir] with content [unattend]. [sr_dir] is
    created if it not already exists. Returns the path of the ISO image *)
let make_iso ~vm_uuid ~unattend =
  try
    let iso = sr_dir // iso_name ~vm_uuid in
    Xapi_stdext_unix.Unixext.mkdir_rec sr_dir 0o755 ;
    with_temp_dir ~dir:"/var/tmp/xapi" "sysprep-" "-iso" (fun temp_dir ->
        let path = temp_dir // "unattend.xml" in
        Unixext.write_string_to_file path unattend ;
        debug "%s: written to %s" __FUNCTION__ path ;
        let args = ["-r"; "-J"; "-o"; iso; temp_dir] in
        Forkhelpers.execute_command_get_output genisoimage args |> ignore ;
        iso
    )
  with e ->
    let msg = Printexc.to_string e in
    Helpers.internal_error "%s failed: %s" __FUNCTION__ msg

(* This function is executed on the host where [vm] is running *)
let sysprep ~__context ~vm ~unattend =
  debug "%s" __FUNCTION__ ;
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let iso = make_iso ~vm_uuid ~unattend in
  debug "%s: created %s" __FUNCTION__ iso
