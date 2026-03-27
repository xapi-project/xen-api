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
  let progress_cb _ = () in
  let run_in_thread tool args pipe_writer replace_fds =
    Thread.create
      (fun () ->
        Xapi_stdext_pervasives.Pervasiveext.finally
          (fun () ->
            Vhd_qcow_parsing.run_tool tool progress_cb args
              ~output_fd:pipe_writer ~replace_fds
          )
          (fun () -> Unix.close pipe_writer)
      )
      ()
  in

  let map_pipe_reader, map_pipe_writer = Unix.pipe ~cloexec:true () in
  let (_ : Thread.t) =
    run_in_thread !Xapi_globs.qemu_img
      ["map"; qcow_path; "--output=json"]
      map_pipe_writer []
  in

  let info_pipe_reader, info_pipe_writer = Unix.pipe ~cloexec:true () in
  let (_ : Thread.t) =
    run_in_thread !Xapi_globs.qemu_img
      ["info"; qcow_path; "--output=json"]
      info_pipe_writer []
  in

  (map_pipe_reader, info_pipe_reader)

let parse_header qcow_path =
  let pipe, _ = read_header qcow_path in
  Vhd_qcow_parsing.parse_header pipe

let parse_header_interval qcow_path =
  let pipes = read_header qcow_path in
  Vhd_qcow_parsing.parse_header_qemu_img pipes

let send ?relative_to (progress_cb : int -> unit) (unix_fd : Unix.file_descr)
    (path : string) (_size : Int64.t) =
  let qcow_of_device =
    Xapi_vdi_helpers.backing_file_of_device_with_driver ~driver:"qcow2"
  in
  let qcow_path = qcow_of_device path in

  (* If VDI is backed by QCOW, parse the header to determine nonzero clusters
     to avoid reading all of the raw disk *)
  let input_fds = Result.map read_header qcow_path |> Result.to_option in

  (* TODO: If VHD headers are to be consulted as well, qcow2-to-stdout
     needs to properly account for cluster_bits. Currently QCOW2 export
     from VHD-backed VDIs will just revert to raw, without any
     allocation accounting. *)

  (* Parse the header of the VDI we are diffing against as well *)
  let relative_to_qcow_path =
    match relative_to with
    | Some x ->
        Result.to_option (qcow_of_device x)
    | None ->
        None
  in
  let diff_fds = Option.map read_header relative_to_qcow_path in

  let map_fd_string = Uuidx.(to_string (make ())) in
  let info_fd_string = Uuidx.(to_string (make ())) in
  let diff_map_fd_string = Uuidx.(to_string (make ())) in
  let diff_info_fd_string = Uuidx.(to_string (make ())) in

  let args =
    [path]
    @ (match relative_to with None -> [] | Some vdi -> ["--diff"; vdi])
    @ ( match relative_to_qcow_path with
      | None ->
          []
      | Some _ ->
          [
            "--json-header-diff-map"
          ; diff_map_fd_string
          ; "--json-header-diff-info"
          ; diff_info_fd_string
          ]
      )
    @
    match qcow_path with
    | Error _ ->
        []
    | Ok _ ->
        [
          "--json-header-map"
        ; map_fd_string
        ; "--json-header-info"
        ; info_fd_string
        ]
  in
  let qcow_tool = !Xapi_globs.qcow_to_stdout in
  let replace_fds =
    Option.map
      (fun (map_fd, info_fd) ->
        let rfds = [(map_fd_string, map_fd); (info_fd_string, info_fd)] in
        match diff_fds with
        | Some (diff_map_fd, diff_info_fd) ->
            (diff_map_fd_string, diff_map_fd)
            :: (diff_info_fd_string, diff_info_fd)
            :: rfds
        | None ->
            rfds
      )
      input_fds
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Vhd_qcow_parsing.run_tool qcow_tool progress_cb args ~output_fd:unix_fd
        ?replace_fds
    )
    (fun () ->
      Option.iter (fun (x, y) -> Unix.close x ; Unix.close y) input_fds ;
      Option.iter (fun (x, y) -> Unix.close x ; Unix.close y) diff_fds
    )
