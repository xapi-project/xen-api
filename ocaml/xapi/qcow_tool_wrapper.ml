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

let run_qcow_tool qcow_tool ?input_fd ?output_fd (_progress_cb : int -> unit)
    (args : string list) =
  info "Executing %s %s" qcow_tool (String.concat " " args) ;
  let open Forkhelpers in
  match
    with_logfile_fd "qcow-tool" (fun log_fd ->
        let pid =
          safe_close_and_exec input_fd output_fd (Some log_fd) [] qcow_tool args
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
  let args = [path] in
  let qcow_tool = !Xapi_globs.qcow_stream_tool in
  run_qcow_tool qcow_tool progress_cb args ~input_fd:unix_fd

let send ?relative_to (progress_cb : int -> unit) (unix_fd : Unix.file_descr)
    (path : string) (_size : Int64.t) =
  let args =
    [path] @ match relative_to with None -> [] | Some vdi -> ["--diff"; vdi]
  in
  let qcow_tool = !Xapi_globs.qcow_to_stdout in
  run_qcow_tool qcow_tool progress_cb args ~output_fd:unix_fd
