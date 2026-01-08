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

let read_vhd_header path =
  let vhd_tool = !Xapi_globs.vhd_tool in
  let args = ["read_headers"; path] in
  let pipe_reader, pipe_writer = Unix.pipe ~cloexec:true () in

  let progress_cb _ = () in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Vhd_qcow_parsing.run_tool vhd_tool progress_cb args ~output_fd:pipe_writer
    )
    (fun () -> Unix.close pipe_writer) ;
  pipe_reader

let parse_header vhd_path =
  let pipe_reader = read_vhd_header vhd_path in
  Vhd_qcow_parsing.parse_header pipe_reader

let send progress_cb ?relative_to (protocol : string) (dest_format : string)
    (s : Unix.file_descr) (path : string) (size : Int64.t) (prefix : string) =
  let __FUN = __FUNCTION__ in
  let vhd_of_device =
    Xapi_vdi_helpers.backing_file_of_device_with_driver ~driver:"vhd"
  in
  let s' = Uuidx.(to_string (make ())) in
  let source_format, source =
    match
      (Xapi_vdi_helpers.get_nbd_device path, vhd_of_device path, relative_to)
    with
    | Some (nbd_server, exportname), _, None ->
        ( "nbdhybrid"
        , Printf.sprintf "%s:%s:%s:%Ld" path nbd_server exportname size
        )
    | Some _, Ok vhd, Some _ | None, Ok vhd, _ ->
        ("hybrid", path ^ ":" ^ vhd)
    | None, Error _, None ->
        ("raw", path)
    | _, Error _, Some _ ->
        let msg = "Cannot compute differences on non-VHD images" in
        error "%s" msg ; failwith msg
  in
  let relative_to =
    let maybe_device path =
      match vhd_of_device path with
      | Ok vhd ->
          Some vhd
      | Error e ->
          let explanation = Xapi_vdi_helpers.backing_file_error_to_string e in
          let msg =
            Printf.sprintf
              "%s: base VDI is not a vhd; cannot compute differences: %s" __FUN
              explanation
          in
          error "%s" msg ; failwith msg
    in
    Option.bind relative_to maybe_device
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
