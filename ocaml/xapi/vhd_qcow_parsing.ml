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

let run_tool tool ?(replace_fds = []) ?input_fd ?output_fd
    (_progress_cb : int -> unit) (args : string list) =
  info "Executing %s %s" tool (String.concat " " args) ;
  (* with_logfile_fd takes a name without slashes *)
  let log_name = Filename.basename tool in
  let open Forkhelpers in
  match
    with_logfile_fd log_name (fun log_fd ->
        let pid =
          safe_close_and_exec input_fd output_fd (Some log_fd) replace_fds tool
            args
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
      debug "%s successful export (%s)" tool out
  | Failure (out, _e) ->
      error "%s output: %s" tool out ;
      raise (Api_errors.Server_error (Api_errors.vdi_io_error, [out]))

let parse_header_aux pipe_reader =
  let ic = Unix.in_channel_of_descr pipe_reader in
  let buf = Buffer.create 4096 in
  let json = Yojson.Basic.from_channel ~buf ~fname:"header.json" ic in
  In_channel.close ic ;
  let cluster_size =
    1 lsl Yojson.Basic.Util.(member "cluster_bits" json |> to_int)
  in
  (cluster_size, json)

let parse_header pipe_reader =
  let cluster_size, json = parse_header_aux pipe_reader in
  let cluster_list =
    Yojson.Basic.Util.(member "data_clusters" json |> to_list |> List.map to_int)
  in
  (cluster_size, cluster_list)

let parse_header_interval pipe_reader =
  let cluster_size, json = parse_header_aux pipe_reader in
  let cluster_list =
    Yojson.Basic.Util.(
      member "data_clusters" json
      |> to_list
      |> List.map (fun x ->
          match to_list x with
          | x :: y :: _ ->
              (to_int x, to_int y)
          | _ ->
              raise (Invalid_argument "Invalid JSON")
      )
    )
  in
  (cluster_size, cluster_list)
