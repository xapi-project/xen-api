(*
 * Copyright (C) 2025 Vates.
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

let update_task_progress (__context : Context.t) (x : int) =
  TaskHelper.set_progress ~__context (float_of_int x /. 100.)

let receive (progress_cb : int -> unit) (unix_fd : Unix.file_descr)
    (path : string) =
  let args = ["stream_decode"; path] in
  let qcow_tool = !Xapi_globs.qcow_stream_tool in
  Vhd_qcow_parsing.run_tool qcow_tool progress_cb args ~input_fd:unix_fd

let read_header qcow_path =
  let args = ["read_headers"; qcow_path] in
  let qcow_tool = !Xapi_globs.qcow_stream_tool in
  let pipe_reader, pipe_writer = Unix.pipe ~cloexec:true () in

  let progress_cb _ = () in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Vhd_qcow_parsing.run_tool qcow_tool progress_cb args
        ~output_fd:pipe_writer
    )
    (fun () -> Unix.close pipe_writer) ;
  pipe_reader

let parse_header qcow_path =
  let pipe_reader = read_header qcow_path in
  Vhd_qcow_parsing.parse_header pipe_reader

let parse_header_interval qcow_path =
  let pipe_reader = read_header qcow_path in
  Vhd_qcow_parsing.parse_header_interval pipe_reader

let send ?relative_to (progress_cb : int -> unit) (unix_fd : Unix.file_descr)
    (path : string) (_size : Int64.t) =
  let qcow_of_device =
    Xapi_vdi_helpers.backing_file_of_device_with_driver ~driver:"qcow2"
  in
  let qcow_path = qcow_of_device path in

  (* If VDI is backed by QCOW, parse the header to determine nonzero clusters
     to avoid reading all of the raw disk *)
  let input_fd = Result.map read_header qcow_path |> Result.to_option in

  (* Parse the header of the VDI we are diffing against as well *)
  let relative_to_qcow_path =
    match relative_to with
    | Some x ->
        Result.to_option (qcow_of_device x)
    | None ->
        None
  in
  let diff_fd = Option.map read_header relative_to_qcow_path in

  let unique_string = Uuidx.(to_string (make ())) in
  let args =
    [path]
    @ (match relative_to with None -> [] | Some vdi -> ["--diff"; vdi])
    @ ( match relative_to_qcow_path with
      | None ->
          []
      | Some _ ->
          ["--json-header-diff"; unique_string]
      )
    @ match qcow_path with Error _ -> [] | Ok _ -> ["--json-header"]
  in
  let qcow_tool = !Xapi_globs.qcow_to_stdout in
  let replace_fds = Option.map (fun fd -> [(unique_string, fd)]) diff_fd in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Vhd_qcow_parsing.run_tool qcow_tool progress_cb args ?input_fd
        ~output_fd:unix_fd ?replace_fds
    )
    (fun () ->
      Option.iter Unix.close input_fd ;
      Option.iter Unix.close diff_fd
    )
