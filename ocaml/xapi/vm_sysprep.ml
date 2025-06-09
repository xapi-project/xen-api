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

let tmp_dir = Filename.get_temp_dir_name ()

let sr_dir = "/opt/opt/iso"

let genisoimage = "/usr/bin/genisoimage"

(** name of the ISO we will use for a VMi; this is not a path *)
let iso_name ~vm_uuid =
  let now = Ptime_clock.now () |> Ptime.to_rfc3339 in
  Printf.sprintf "config-%s-%s.iso" vm_uuid now

(** taken from OCaml 5 stdlib *)
let temp_dir ?(dir = tmp_dir) ?(perms = 0o700) prefix suffix =
  let rec try_name counter =
    let name = Filename.temp_file ~temp_dir:dir prefix suffix in
    try Sys.mkdir name perms ; name
    with Sys_error _ as e ->
      if counter >= 20 then raise e else try_name (counter + 1)
  in
  try_name 0

(** Crteate a temporary directory, and pass its path to [f]. Once [f]
    returns the directory is removed again *)
let with_temp_dir ?(dir = tmp_dir) ?(perms = 0o700) prefix suffix f =
  let dir = temp_dir ~dir ~perms prefix suffix in
  finally (fun () -> f dir) (fun () -> Unixext.rm_rec dir)

let make_iso ~vm_uuid ~unattend =
  try
    let _iso = sr_dir // iso_name ~vm_uuid in
    Xapi_stdext_unix.Unixext.mkdir_rec sr_dir 0o755
    (* Unixext.write_string_to_file path unattend *)
  with e ->
    let msg = Printexc.to_string e in
    Helpers.internal_error "%s failed: %s" __FUNCTION__ msg

(* This function is executed on the host where [vm] is running *)
let sysprep ~__context ~vm ~unattend = debug "%s" __FUNCTION__
