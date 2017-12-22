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
(**
 * @group Storage
*)

open Xapi_stdext_unix

(** Represents the configuration of a static (ie attached on boot) vdi *)
type vdi = {
  uuid:               string;        (* VDI.uuid *)
  reason:             string;        (* describes why this disk was attached for debugging *)
  delete_next_boot:   bool;          (* indicates this disk configuration will be forgotten on reboot *)
  currently_attached: bool;          (* indicates this disk is attached now in dom0 *)
  path:               string option; (* path in dom0 *)
}

(** Returns a list of vdi records, one for each VDI statically configured on this host *)
let list () =
  (* Read the filesystem structure directly *)
  let main_dir = !Db_globs.static_vdis_dir in
  let all = try Array.to_list (Sys.readdir main_dir) with Sys_error _ -> [] in
  List.map (fun x ->
      let path = Filename.concat main_dir x in
      let uuid = Unixext.string_of_file (Filename.concat path "vdi-uuid") in
      let reason = Unixext.string_of_file (Filename.concat path "reason") in
      (* let bool_of_string x = String.lowercase_ascii x = "true" in *)
      let delete_next_boot =
        try ignore(Unix.stat (Filename.concat path "delete-next-boot")); true
        with _ -> false in
      let currently_attached =
        try ignore(Unix.stat (Filename.concat path "disk")); true
        with _ -> false in
      let path =
        try Some (Unix.readlink (Filename.concat path "disk"))
        with _ -> None in
      { uuid = uuid; reason = reason; delete_next_boot = delete_next_boot;
        currently_attached = currently_attached; path = path }) all

