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

open Pervasiveext

let vhd_tool = Filename.concat Fhs.bindir "vhd-tool"

let ignore_progress _ = ()

let run_vhd_tool progress_cb args s s' path =
  info "Executing %s %s" vhd_tool (String.concat " " args);
  let open Forkhelpers in
  let pipe_read, pipe_write = Unix.pipe () in
  let to_close = ref [ pipe_read; pipe_write ] in
  let close x = if List.mem x !to_close then (Unix.close x; to_close := List.filter (fun y -> y <> x) !to_close) in
  finally
  (fun () ->
    match with_logfile_fd "vhd-tool"
      (fun log_fd ->
        let pid = safe_close_and_exec None (Some pipe_write) (Some log_fd) [ s', s ] vhd_tool args in
        close pipe_write;
        begin
          try
            let buf = String.make 3 '\000' in
            while true do
              Unixext.really_read pipe_read buf 0 (String.length buf);
              progress_cb (int_of_string buf)
            done
          with End_of_file -> ()
             | e ->
               warn "unexpected error reading progress from vhd-tool: %s" (Printexc.to_string e)
        end;
        let (_, status) = waitpid pid in
        if status <> Unix.WEXITED 0 then begin
          error "vhd-tool failed, returning VDI_IO_ERROR";
          raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O errors"])) 
        end
      ) with
    | Success(out, _) -> debug "%s" out
    | Failure(out, e) -> error "vhd-tool output: %s" out; raise e
  ) (fun () -> close pipe_read; close pipe_write)

let receive progress_cb protocol (s: Unix.file_descr) (path: string) (prefix: string) (prezeroed: bool) =
  let s' = Uuidm.to_string (Uuidm.create `V4) in
  let args = [ "serve";
               "--source-protocol"; protocol;
               "--source-fd"; s';
               "--tar-filename-prefix"; prefix;
               "--destination"; "file://" ^ path;
               "--destination-format"; "raw";
               "--progress";
               "--machine";
               "--direct";
             ] @ (if prezeroed then [ "--prezeroed" ] else []) in
  run_vhd_tool progress_cb args s s' path

open Fun

let startswith prefix x =
        let prefix' = String.length prefix
        and x' = String.length x in
        prefix' <= x' && (String.sub x 0 prefix' = prefix)

(** [find_backend_device path] returns [Some path'] where [path'] is the backend path in
    the driver domain corresponding to the frontend device [path] in this domain. *)
let find_backend_device path =
        try 
                let open Xenstore in
                (* If we're looking at a xen frontend device, see if the backend
                   is in the same domain. If so check if it looks like a .vhd *)
                let rdev = (Unix.stat path).Unix.st_rdev in
                let major = rdev / 256 and minor = rdev mod 256 in
                let link = Unix.readlink (Printf.sprintf "/sys/dev/block/%d:%d/device" major minor) in
                match List.rev (Re_str.split (Re_str.regexp_string "/") link) with
                | id :: "xen" :: "devices" :: _ when startswith "vbd-" id ->
                        let id = int_of_string (String.sub id 4 (String.length id - 4)) in
                        with_xs (fun xs -> 
                                let self = xs.Xs.read "domid" in
                                let backend = xs.Xs.read (Printf.sprintf "device/vbd/%d/backend" id) in
                                let params = xs.Xs.read (Printf.sprintf "%s/params" backend) in
                                match Re_str.split (Re_str.regexp_string "/") backend with
                                | "local" :: "domain" :: bedomid :: _ ->
                                        assert (self = bedomid);
                                        Some params
                                | _ -> raise Not_found
                        )
                | _ -> raise Not_found
        with _ -> None
(** [vhd_of_device path] returns (Some vhd) where 'vhd' is the vhd leaf backing a particular device [path] or None.
    [path] may either be a blktap2 device *or* a blkfront device backed by a blktap2 device. If the latter then
    the script must be run in the same domain as blkback. *)
let vhd_of_device path =
        let tapdisk_of_path path =
                try 
                        match Tapctl.of_device (Tapctl.create ()) path with
                        | _, _, (Some ("vhd", vhd)) -> Some vhd
                        | _, _, _ -> raise Not_found
                with Tapctl.Not_blktap ->
                        debug "Device %s is not controlled by blktap" path;
                        None
                | Tapctl.Not_a_device ->
                        debug "%s is not a device" path;
                        None
                | _ -> 
                        debug "Device %s has an unknown driver" path;
                        None in
        find_backend_device path |> Opt.default path |> tapdisk_of_path

let send progress_cb (s: Unix.file_descr) (path: string) (prefix: string) =
  let s' = Uuidm.to_string (Uuidm.create `V4) in
  let source_format, source = match vhd_of_device path with
    | Some vhd -> "hybrid", path ^ ":" ^ vhd
    | None -> "raw", path in
  let args = [ "stream";
               "--source-protocol"; "none";
               "--source-format"; source_format;
               "--source"; source;
               "--destination-protocol"; "tar";
               "--destination-format"; "raw";
               "--destination-fd"; s';
               "--tar-filename-prefix"; prefix;
               "--progress";
               "--machine";
               "--direct";
             ] in
  run_vhd_tool progress_cb args s s' path

