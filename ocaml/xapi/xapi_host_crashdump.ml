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
   * @group Host Management
*)

open Stdext
open Listext
open Pervasiveext
open Xstringext
open Xapi_support

module D = Debug.Make(struct let name="xapi" end)
open D


let rm = "/bin/rm"
let du = "/usr/bin/du"

(* Host crashes are directories placed into /var/crash by the crash kernel *)
let crash_dir = "/var/crash"

let delete_crashdump_dir filename =
  let path = Filename.concat crash_dir filename in
  try
    let stat = Unix.stat path in
    match stat.Unix.st_kind with
    | Unix.S_DIR ->
      (* crash dumps are directories *)
      let cmd = Printf.sprintf "%s -rf %s" rm path in
      let output = Helpers.get_process_output cmd in
      if output <> "" then warn "Output from %s: %s" cmd output
    | _ ->
      error "Crashdump path %s refers to something other than a directory!" path;
  with e ->
    error "Caught exception while deleting crashdump at path %s (%s)" filename (ExnHelper.string_of_exn e);
    raise e

(* Called once on host boot to resync the crash directory with the database *)
let resynchronise ~__context ~host =
  debug "Xapi_host_crashdump.resynchronise";
  let all_refs = Db.Host.get_crashdumps ~__context ~self:host in
  let db_filenames = List.map
      (fun self ->
         Db.Host_crashdump.get_filename ~__context ~self) all_refs in

  let real_filenames =
    List.filter (fun filename ->
        let stat = Unix.stat (Filename.concat crash_dir filename) in
        stat.Unix.st_kind = Unix.S_DIR (*only directories are marked as crashdumps*)
      )
      (try Array.to_list (Sys.readdir crash_dir) with _ -> []) in
  let gone_away = List.set_difference db_filenames real_filenames
  and arrived = List.set_difference real_filenames db_filenames in

  let was_shutdown_cleanly = try bool_of_string (Localdb.get Constants.host_restarted_cleanly) with _ -> false in
  Localdb.put Constants.host_restarted_cleanly "false";
  (* If HA is enabled AND no crashdump appeared AND we weren't shutdown cleanly then assume it was a fence. *)
  let ha_is_enabled =
    try Db.Pool.get_ha_enabled ~__context ~self:(Helpers.get_pool ~__context)
    with _ -> false (* on first boot no-pool=>exn, but on first boot HA is never enabled *) in
  begin
    if ha_is_enabled && (arrived = []) && not was_shutdown_cleanly && !Xapi_globs.on_system_boot
    then Xapi_alert.add ~msg:Api_messages.ha_host_was_fenced ~cls:`Host ~obj_uuid:(Db.Host.get_uuid ~__context ~self:host) ~body:""
  end;

  let table = List.combine db_filenames all_refs in
  List.iter (fun filename ->
      debug "Deleting record corresponding to old crashdump %s" filename;
      let r = List.assoc filename table in
      Db.Host_crashdump.destroy ~__context ~self:r) gone_away;
  List.iter (fun filename ->
      debug "Adding record corresponding to new crashdump %s" filename;
      let cmd = Printf.sprintf "%s --bytes -s %s/%s" du crash_dir filename in
      let size = match String.split_f String.isspace (Helpers.get_process_output cmd) with
        | size :: _ -> Int64.of_string size
        | _ -> (-1L) in
      let timestamp =
        let open Unix in
        try Scanf.sscanf filename "%04d%02d%02d-%02d%02d%02d-UTC"
              (fun year mon tm_mday tm_hour tm_min tm_sec ->
                 fst ( mktime {tm_year=year-1900; tm_mon=mon-1; tm_mday; tm_hour; tm_min; tm_sec; tm_wday=0; tm_yday=0; tm_isdst=false}))
        with _ ->
          (Unix.stat (Filename.concat crash_dir filename)).Unix.st_ctime in
      let timestamp = Date.of_float timestamp in
      let r = Ref.make () and uuid = Uuid.to_string (Uuid.make_uuid ()) in
      Db.Host_crashdump.create ~__context ~ref:r ~uuid ~other_config:[]
        ~host ~timestamp ~size ~filename) arrived


let destroy ~__context ~self =
  let filename = Db.Host_crashdump.get_filename ~__context ~self in
  finally
    (fun () -> delete_crashdump_dir filename)
    (fun () -> Db.Host_crashdump.destroy ~__context ~self)


let upload ~__context ~self ~url ~options =
  let filename = Db.Host_crashdump.get_filename ~__context ~self in
  let url = if url = "" then (upload_url filename) else url in
  do_upload "host-crash-upload" (crash_dir ^ "/" ^ filename) url options

