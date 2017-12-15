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
let oldnotify = ref false

let disc_inserted name =
  let args = [| "xe" ; "host-notify"; "type=cdrom"; "params=inserted:" ^ name |] in
  let ret = Stdext.Unixext.spawnvp args.(0) args in
  (* check if we got an error, and record the fact *)
  begin match ret with
    | Unix.WEXITED 0 -> oldnotify := false
    | Unix.WEXITED n -> oldnotify := true
    | _              -> oldnotify := true
  end

let disc_removed name =
  (* we don't need to do anything *)
  oldnotify := false

let check interval name =
  let has_disc = ref false in

  let check_has_disc status =
    if !has_disc then (
      begin match status with
        | Cdrom.NO_INFO | Cdrom.NO_DISC | Cdrom.TRAY_OPEN ->
          has_disc := false; disc_removed name
        | _             -> ()
      end;
      if !oldnotify then
        disc_inserted name
    ) else (
      match status with
      | Cdrom.DISC_OK -> has_disc := true; disc_inserted name
      | _             -> ()
    )
  in

  let status = Cdrom.query_cdrom_drive_status name in
  has_disc := status = Cdrom.DISC_OK;

  while Sys.file_exists name
  do
    let drive_status = Cdrom.query_cdrom_drive_status name in
    check_has_disc drive_status;
    Unix.sleep interval
  done

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "usage: %s <cdrompath>\n" Sys.argv.(0);
    exit 1
  );
  Xapi_stdext_unix.Unixext.daemonize ();
  (* check every 2 seconds *)
  check 2 Sys.argv.(1)
