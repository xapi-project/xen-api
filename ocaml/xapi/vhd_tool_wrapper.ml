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
    @
    if prezeroed then
      ["--prezeroed"]
    else
      []
  in
  Vhd_qcow_parsing.run_tool !Xapi_globs.vhd_tool args ~progress_cb
    ~replace_fds:[(s', s)]

let read_vhd_header path ~legacy =
  let vhd_tool = !Xapi_globs.vhd_tool in
  let args =
    if legacy then
      ["read_headers"; path]
    else
      ["read_headers_interval"; path]
  in
  let pipe_reader, pipe_writer = Unix.pipe ~cloexec:true () in

  let progress_cb _ = () in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        Xapi_stdext_pervasives.Pervasiveext.finally
          (fun () ->
            Vhd_qcow_parsing.run_tool vhd_tool args ~progress_cb
              ~output_fd:pipe_writer
          )
          (fun () -> Unix.close pipe_writer)
      )
      ()
  in
  pipe_reader

let parse_header vhd_path =
  let pipe_reader = read_vhd_header vhd_path ~legacy:true in
  Vhd_qcow_parsing.parse_header pipe_reader

let parse_header_interval vhd_path =
  let pipe_reader = read_vhd_header vhd_path ~legacy:false in
  Vhd_qcow_parsing.parse_header_interval pipe_reader

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
  Vhd_qcow_parsing.run_tool !Xapi_globs.vhd_tool args ~progress_cb
    ~replace_fds:[(s', s)]
