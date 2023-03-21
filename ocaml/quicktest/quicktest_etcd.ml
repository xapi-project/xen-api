(*
 * Copyright (C) Cloud Software Group, Inc.
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

open Xapi_metastore

(* TODO: allocate port dynamically *)
let peer_port = 12380

let port = 12379

let live =
  Config.Live.
    {
      advertise_client_urls=
        [Uri.make ~host:"localhost" ~port ~scheme:"http" ()]
        (* empty value not valid here *)
    ; initial_advertise_peer_urls=
        [Uri.make ~host:"localhost" ~port:peer_port ~scheme:"http" ()]
    }
  

let local =
  Config.Local.
    {
      config=
        {
          name= "localtest"
        ; enable_v2= false
        ; log_level= Some Config_field.DEBUG
        ; log_output= Some Stdout
        }
    ; peer= (None, [Uri.make ~host:"localhost" ~port:peer_port ()])
    ; client= (None, [Uri.make ~host:"localhost" ~port ()])
    }
  

let config = Config.make live local

let etcd = ref "/usr/bin/etcd"

let etcdctl = ref "/usr/bin/etcdctl"

module StringMap = Map.Make (String)

let kill_after pid =
  Thread.delay 0.5 ;
  try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let read_line ch =
  match input_line ch with
  | line ->
      Some (line, ch)
  | exception End_of_file ->
      None

let read_all ch = Seq.unfold read_line ch |> List.of_seq |> String.concat "\n"

(* easier to debug when starting directly, without systemd, because we get the
   output directly *)
let test_start_stop_direct config () =
  Logs.debug (fun m -> m "Starting with config %a" Config.dump config) ;
  Helpers.with_temp_file "etcd_config" ".conf" @@ fun (conf, _) ->
  let conf = Fpath.v conf in
  let env =
    Array.of_list
      (config
      |> Config.to_dict
      |> StringMap.bindings
      |> List.rev_map @@ fun (k, v) -> Printf.sprintf "%s=%s" k v
      )
  in
  Logs.debug (fun m -> m "environment: %a" Fmt.Dump.(array string) env) ;
  config |> Config.serialize |> Serialization.string_to_file_exn conf ;
  (* TODO: use forkexec helpers or bos here? *)
  let cmd, args = (!etcd, []) in
  let stdout, stdin, stderr =
    Unix.open_process_full (Filename.quote_command cmd args) env
  in
  let pid = Unix.process_full_pid (stdout, stdin, stderr) in
  let t = Thread.create kill_after pid in
  Fun.protect ~finally:(fun () -> Thread.join t) @@ fun () ->
  Logs.debug (fun m -> m "stdout: %s" @@ read_all stdout) ;
  Logs.debug (fun m -> m "stderr: %s" @@ read_all stderr) ;
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 ->
      Logs.debug (fun m -> m "terminated OK")
  | Unix.WSIGNALED n when n = Sys.sigterm ->
      Logs.debug (fun m -> m "terminated")
  | Unix.WEXITED n ->
      Fmt.failwith "exited with code %d" n
  | Unix.WSTOPPED n ->
      Fmt.failwith "stopped by signal %d" n
  | Unix.WSIGNALED n ->
      Fmt.failwith "killed by signal %d" n

let tests () =
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Logs.set_level ~all:true (Some Logs.Debug) ;
  [("start/stop etcd directly", `Quick, test_start_stop_direct config)]
