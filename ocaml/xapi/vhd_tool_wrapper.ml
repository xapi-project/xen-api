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

module D = Debug.Make (struct let name = "vhd_tool_wrapper" end)

open D
open Xapi_stdext_std.Xstringext

(* .vhds on XenServer are sometimes found via /dev/mapper *)
let vhd_search_path = "/dev/mapper:."

let update_task_progress __context x =
  TaskHelper.set_progress ~__context (float_of_int x /. 100.)

let run_vhd_tool progress_cb args s s' _path =
  let vhd_tool = !Xapi_globs.vhd_tool in
  info "Executing %s %s" vhd_tool (String.concat " " args) ;
  let open Forkhelpers in
  let pipe_read, pipe_write = Unix.pipe () in
  let to_close = ref [pipe_read; pipe_write] in
  let close x =
    if List.mem x !to_close then (
      Unix.close x ;
      to_close := List.filter (fun y -> y <> x) !to_close
    )
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      match
        with_logfile_fd "vhd-tool" (fun log_fd ->
            let pid =
              safe_close_and_exec None (Some pipe_write) (Some log_fd)
                [(s', s)]
                vhd_tool args
            in
            close pipe_write ;
            ( try
                let buf = Bytes.make 3 '\000' in
                while true do
                  Xapi_stdext_unix.Unixext.really_read pipe_read buf 0
                    (Bytes.length buf) ;
                  progress_cb (int_of_string (Bytes.to_string buf))
                done
              with
            | End_of_file ->
                ()
            | e ->
                warn "unexpected error reading progress from vhd-tool: %s"
                  (Printexc.to_string e)
            ) ;
            let _, status = waitpid pid in
            if status <> Unix.WEXITED 0 then (
              error "vhd-tool failed, returning VDI_IO_ERROR" ;
              raise
                (Api_errors.Server_error
                   (Api_errors.vdi_io_error, ["Device I/O errors"])
                )
            )
        )
      with
      | Success (out, _) ->
          debug "%s" out
      | Failure (out, e) ->
          error "vhd-tool output: %s" out ;
          raise e
    )
    (fun () -> close pipe_read ; close pipe_write)

let receive progress_cb format protocol (s : Unix.file_descr)
    (length : int64 option) (path : string) (prefix : string) (prezeroed : bool)
    =
  let s' = Uuidx.(to_string (make ())) in
  let args =
    [
      "serve"
    ; "--source-format"
    ; format
    ; "--source-protocol"
    ; protocol
    ; "--source-fd"
    ; s'
    ; "--tar-filename-prefix"
    ; prefix
    ; "--destination"
    ; "file://" ^ path
    ; "--destination-format"
    ; "raw"
    ; "--progress"
    ; "--machine"
    ; "--direct"
    ]
    @ ( match length with
      | Some x ->
          ["--destination-size"; Int64.to_string x]
      | None ->
          []
      )
    @ if prezeroed then ["--prezeroed"] else []
  in
  run_vhd_tool progress_cb args s s' path

(** [vhd_of_device path] returns (Some vhd) where 'vhd' is the vhd leaf backing a particular device [path] or None.
    [path] may either be a blktap2 device *or* a blkfront device backed by a blktap2 device. If the latter then
    the script must be run in the same domain as blkback. *)
let vhd_of_device path =
  let tapdisk_of_path path =
    try
      match Tapctl.of_device (Tapctl.create ()) path with
      | _, _, Some ("vhd", vhd) ->
          Some vhd
      | _, _, _ ->
          raise Not_found
    with
    | Tapctl.Not_blktap -> (
        debug "Device %s is not controlled by blktap" path ;
        (* Check if it is a VHD behind a NBD deivce *)
        Stream_vdi.(get_nbd_device path |> image_behind_nbd_device) |> function
        | Some ("vhd", vhd) ->
            debug "%s is a VHD behind NBD device %s" vhd path ;
            Some vhd
        | _ ->
            None
      )
    | Tapctl.Not_a_device ->
        debug "%s is not a device" path ;
        None
    | _ ->
        debug "Device %s has an unknown driver" path ;
        None
  in
  Common_tool_wrapper.find_backend_device path
  |> Option.value ~default:path
  |> tapdisk_of_path

let send progress_cb ?relative_to (protocol : string) (dest_format : string)
    (s : Unix.file_descr) (path : string) (size : Int64.t) (prefix : string) =
  let s' = Uuidx.(to_string (make ())) in
  debug "GTNDEBUG: path is %s" path ;
  debug "GTNDEBUG: prefix is %s" prefix ;
  let source_format, source =
    match (Stream_vdi.get_nbd_device path, vhd_of_device path, relative_to) with
    | Some (nbd_server, exportname), _, None ->
        debug "GTNDEBUG: nbdhybrid %s:%s:%s:%Ld" path nbd_server exportname size ;
        ( "nbdhybrid"
        , Printf.sprintf "%s:%s:%s:%Ld" path nbd_server exportname size
        )
    | Some _, Some vhd, Some _ | None, Some vhd, _ ->
        debug "GTNDEBUG: hybrid %s" (path ^ ":" ^ vhd) ;
        ("hybrid", path ^ ":" ^ vhd)
    | None, None, None ->
        debug "GTNDEBUG: raw %s" path ;
        ("raw", path)
    | _, None, Some _ ->
        let msg = "Cannot compute differences on non-VHD images" in
        error "%s" msg ; failwith msg
  in
  let relative_to =
    match relative_to with
    | Some path -> (
      match vhd_of_device path with
      | Some vhd ->
          Some vhd
      | None ->
          error "base VDI is not a vhd; cannot compute differences" ;
          failwith "base VDI is not a vhd; cannot compute differences"
    )
    | None ->
        None
  in
  let args =
    [
      "stream"
    ; "--source-protocol"
    ; "none"
    ; "--source-format"
    ; source_format
    ; "--source"
    ; source
    ; "--destination-protocol"
    ; protocol
    ; "--destination-format"
    ; dest_format
    ; "--destination-fd"
    ; s'
    ; "--tar-filename-prefix"
    ; prefix
    ; "--progress"
    ; "--machine"
    ; "--direct"
    ; "--path"
    ; vhd_search_path
    ]
    @ match relative_to with None -> [] | Some x -> ["--relative-to"; x]
  in
  run_vhd_tool progress_cb args s s' path
