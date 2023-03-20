(*
 * Copyright (C) 2019 Citrix Systems Inc.
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
type status = {
    result: string
  ; exec_main_pid: int
  ; exec_main_status: int
  ; active_state: string
}

module StringMap = Map.Make (String)

type 'a string_map = 'a StringMap.t

let systemctl = "/usr/bin/systemctl"

let test_mode = ref false

let execute_command_get_output cmd args =
  (* for systemctl --user to work it needs env vars *)
  let env, args =
    if !test_mode then
      (Some (Unix.environment ()), "--user" :: args)
    else
      (None, args)
  in
  Forkhelpers.execute_command_get_output ?env cmd args

let action ?(args = []) ~service action =
  let _, _ = execute_command_get_output systemctl (action :: service :: args) in
  ()

let default_env =
  StringMap.singleton "PATH" @@ String.concat ":" Forkhelpers.default_path

let run_path = "/run/systemd/system/"

(* see https://www.freedesktop.org/software/systemd/man/systemd.syntax.html#Quoting
   Neither [Filename.quote] or [String.escaped] would produce correct results
*)
let systemd_quote s =
  let b = Buffer.create (String.length s) in
  Buffer.add_char b '\'' ;
  let () =
    s
    |> String.iter @@ function
       | '\\' ->
           Buffer.add_string b {|\\|}
       | '\'' ->
           (* these hex escapes work regardless what outer quotes are used *)
           Buffer.add_string b {|\x27|}
       | '"' ->
           Buffer.add_string b {|\x22|}
       | c when Astring.Char.Ascii.is_print c ->
           Buffer.add_char b c
       | _ ->
           invalid_arg ("Values can only contain printable characters: " ^ s)
  in
  Buffer.add_char b '\'' ; Buffer.contents b

let env_pair (k, v) = Printf.sprintf "%s=%s" k v

let environment env =
  env
  |> StringMap.bindings
  |> List.map env_pair
  |> List.map (fun v -> ("Environment", [v]))
(* we could build just a single environment line, but might be too long and
   difficult to debug *)

let build_properties env base properties =
  "[Service]"
  :: (List.concat [environment env; base; properties]
     |> List.map (fun (k, v) ->
            String.concat ""
              [k; "="; List.map systemd_quote v |> String.concat " "]
        )
     )

let start_transient ?(env = default_env) ?(properties = []) ~service cmd args =
  let syslog_key = service in
  let service = syslog_key ^ ".service" in
  let destination = Filename.concat run_path service in
  build_properties env
    [
      ("SyslogIdentifier", [syslog_key])
    ; ("SyslogLevel", ["debug"])
    ; ("StandardOutput", ["syslog"])
    ; ("StandardError", ["inherit"])
    ; ("StartLimitInterval", ["0"]) (* no rate-limit, for bootstorms *)
    ; ("ExecStart", cmd :: args)
    ; ("Type", ["simple"])
      (* our systemd is too old, and doesn't support 'exec' *)
    ; ("Restart", ["no"])
      (* can't restart the device-model, it would've lost all state already *)
    ; ("Slice", ["system.slice"])
    ; ("TimeoutStopSec", ["10"])
    ]
    properties
  |> List.append
       [
         "[Unit]"
       ; "Description=transient unit for " ^ syslog_key
       ; "DefaultDependencies=no" (* lifecycle tied to domain, not systemd *)
       ]
  |> String.concat "\n"
  |> Xapi_stdext_unix.Unixext.write_string_to_file destination ;
  action ~service "start"

let unit_path () =
  if !test_mode then
    let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/root" in
    Filename.concat home ".config/systemd/user"
  else
    "/etc/systemd/system"

let drop_in_path service =
  Printf.sprintf "%s/%s.service.d/10-xapi.conf" (unit_path ()) service

let set_properties ?(env = StringMap.empty) ?(properties = []) ~service () =
  let path = drop_in_path service in
  Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname path) 0o700 ;
  build_properties env [] properties
  |> String.concat "\n"
  |> Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0o600 path ;
  let _, _ = execute_command_get_output systemctl ["daemon-reload"] in
  ()

let start_templated ~template ~instance =
  action ~service:(Printf.sprintf "%s@%s" template instance) "start"

let show ~service =
  let result = "Result" in
  let exec_main_pid = "ExecMainPID" in
  let exec_main_status = "ExecMainStatus" in
  let active_state = "ActiveState" in
  let props =
    [result; exec_main_pid; exec_main_status; active_state] |> String.concat ","
    |> fun properties ->
    ["show"; "-p"; properties; service]
    |> execute_command_get_output systemctl
    |> fst
    |> Astring.String.cuts ~sep:"\n"
    |> List.to_seq
    |> Seq.filter_map (Astring.String.cut ~sep:"=")
    |> List.of_seq
  in
  {
    result= props |> List.assoc result
  ; exec_main_pid= props |> List.assoc exec_main_pid |> int_of_string
  ; exec_main_status= props |> List.assoc exec_main_status |> int_of_string
  ; active_state= props |> List.assoc active_state
  }

let stop ~service =
  action ~service "stop" ;
  (* Stopping shouldn't fail because it should fall back to SIGKILL which should almost always work,
   * unless there is a kernel bug that keeps a process stuck.
   * In the unlikely scenario that this does fail we leave the transient service file behind
   * so that the failure can be investigated.
   * *)
  let status = show ~service in
  (* allow systemd to garbage-collect the status and the unit, preventing leaks.
   * See CollectMode in systemd.unit(5) for details. *)
  ( if status.exec_main_status <> 0 then
      try action ~service "reset-failed" with _ -> ()
  ) ;
  let destination = Filename.concat run_path (service ^ ".service") in
  Xapi_stdext_unix.Unixext.unlink_safe destination ;
  let dropin = drop_in_path service in
  Xapi_stdext_unix.Unixext.unlink_safe dropin ;
  let () = try Unix.rmdir (Filename.dirname dropin) with _ -> () in
  status

let is_active ~service =
  let status =
    Forkhelpers.safe_close_and_exec None None None [] systemctl
      ["is-active"; "--quiet"; service]
    |> Forkhelpers.waitpid
    |> snd
  in
  Unix.WEXITED 0 = status

let exists ~service =
  Sys.file_exists (Filename.concat run_path (service ^ ".service"))

let start_transient ?env ?properties ~service cmd args =
  if exists ~service then
    (* this can only happen if there is a bug in the caller *)
    invalid_arg (Printf.sprintf "Tried to start %s twice" service) ;
  try start_transient ?env ?properties ~service cmd args
  with e ->
    Backtrace.is_important e ;
    (* If start failed we do not know what state the service is in:
     * try to stop it and clean up.
     * Stopping could fail as well, in which case report the original exception.
     * *)
    ( try
        let (_ : status) = stop ~service in
        ()
      with _ -> ()
    ) ;
    raise e

let set_test () = test_mode := true
