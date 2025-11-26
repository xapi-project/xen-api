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

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let run_qcow_tool qcow_tool ?(replace_fds = []) ?input_fd ?output_fd
    (_progress_cb : int -> unit) (args : string list) =
  info "Executing %s %s" qcow_tool (String.concat " " args) ;
  let open Forkhelpers in
  match
    with_logfile_fd "qcow-tool" (fun log_fd ->
        let pid =
          safe_close_and_exec input_fd output_fd (Some log_fd) replace_fds
            qcow_tool args
        in
        let _, status = waitpid pid in
        if status <> Unix.WEXITED 0 then (
          error "qcow-tool failed, returning VDI_IO_ERROR" ;
          raise
            (Api_errors.Server_error
               (Api_errors.vdi_io_error, ["Device I/O errors"])
            )
        )
    )
  with
  | Success (out, _) ->
      debug "qcow-tool successful export (%s)" out
  | Failure (out, _e) ->
      error "qcow-tool output: %s" out ;
      raise (Api_errors.Server_error (Api_errors.vdi_io_error, [out]))

let update_task_progress (__context : Context.t) (x : int) =
  TaskHelper.set_progress ~__context (float_of_int x /. 100.)

let receive (progress_cb : int -> unit) (unix_fd : Unix.file_descr)
    (path : string) =
  let args = ["stream_decode"; path] in
  let qcow_tool = !Xapi_globs.qcow_stream_tool in
  run_qcow_tool qcow_tool progress_cb args ~input_fd:unix_fd

let read_header qcow_path =
  let args = ["read_headers"; qcow_path] in
  let qcow_tool = !Xapi_globs.qcow_stream_tool in
  let pipe_reader, pipe_writer = Unix.pipe ~cloexec:true () in

  let progress_cb _ = () in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> run_qcow_tool qcow_tool progress_cb args ~output_fd:pipe_writer)
    (fun () -> Unix.close pipe_writer) ;
  pipe_reader

let parse_header qcow_path =
  let pipe_reader = read_header qcow_path in
  let ic = Unix.in_channel_of_descr pipe_reader in
  let buf = Buffer.create 4096 in
  let json = Yojson.Basic.from_channel ~buf ~fname:"qcow_header.json" ic in
  In_channel.close ic ;
  let cluster_size =
    1 lsl Yojson.Basic.Util.(member "cluster_bits" json |> to_int)
  in
  let cluster_list =
    Yojson.Basic.Util.(member "data_clusters" json |> to_list |> List.map to_int)
  in
  (cluster_size, cluster_list)

let send ?relative_to (progress_cb : int -> unit) (unix_fd : Unix.file_descr)
    (path : string) (_size : Int64.t) =
  let qcow_of_device =
    Vhd_tool_wrapper.backing_file_of_device ~driver:"qcow2"
  in
  let qcow_path = qcow_of_device path in

  (* If VDI is backed by QCOW, parse the header to determine nonzero clusters
     to avoid reading all of the raw disk *)
  let input_fd = Option.map read_header qcow_path in

  (* Parse the header of the VDI we are diffing against as well *)
  let relative_to_qcow_path = Option.bind relative_to qcow_of_device in
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
    @ match qcow_path with None -> [] | Some _ -> ["--json-header"]
  in
  let qcow_tool = !Xapi_globs.qcow_to_stdout in
  let replace_fds = Option.map (fun fd -> [(unique_string, fd)]) diff_fd in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      run_qcow_tool qcow_tool progress_cb args ?input_fd ~output_fd:unix_fd
        ?replace_fds
    )
    (fun () ->
      Option.iter Unix.close input_fd ;
      Option.iter Unix.close diff_fd
    )
