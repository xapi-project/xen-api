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
(** HTTP handler for importing a raw VDI.
 * @group Import and Export
 *)

module D=Debug.Make(struct let name="vdi_tool_wrapper" end)
open D

let vhd_tool = Filename.concat Fhs.bindir "vhd-tool"

let receive protocol (s: Unix.file_descr) (path: string) =
  let s' = Uuidm.to_string (Uuidm.create `V4) in
  let args = [ "serve";
               "--direct";
               "--source-protocol"; protocol;
               "--source-fd"; s';
               "--destination"; "file://" ^ path;
               "--destination-format"; "raw" ] in
  info "Executing %s %s" vhd_tool (String.concat " " args);
  let open Forkhelpers in
  match with_logfile_fd "vhd-tool"
    (fun log_fd ->
      let pid = safe_close_and_exec None (Some log_fd) (Some log_fd) [ s', s ] vhd_tool args in
      let (_, status) = waitpid pid in
      if status <> Unix.WEXITED 0 then begin
        error "vhd-tool failed, returning VDI_IO_ERROR";
        raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O errors"])) 
      end
    ) with
  | Success(out, _) -> debug "%s" out
  | Failure(out, e) -> error "vhd-tool output: %s" out; raise e


