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
  result: string;
  exec_main_pid: int;
  exec_main_status: int;
  active_state: string;
}

let systemctl = "/usr/bin/systemctl"

let action ~service action =
  let (_, _stderr) = Forkhelpers.execute_command_get_output systemctl [action; service] in
  ()

let default_env = [ "PATH=" ^ (String.concat ":" Forkhelpers.default_path) ]

let run_path = "/run/systemd/system/"


let start_transient ?(env=Array.of_list default_env) ?(properties=[]) ~service cmd args =
  let syslog_key = service in
  let service = syslog_key ^ ".service" in
  let destination = Filename.concat run_path service in
  properties
  |> List.append
  ["Environment", env |> Array.to_list |> List.map Filename.quote |> String.concat " "
  ;"SyslogIdentifier", syslog_key
  ;"SyslogLevel", "debug"
  ;"StandardOutput", "syslog"
  ;"StandardError", "inherit"
  ;"StartLimitInterval", "0" (* no rate-limit, for bootstorms *)
  ;"ExecStart", String.concat " " (cmd :: List.map Filename.quote args)
  ;"Type", "simple" (* our systemd is too old, and doesn't support 'exec' *)
  ;"Restart", "no" (* can't restart the device-model, it would've lost all state already *)
  ;"Slice", "system.slice"
  ;"TimeoutStopSec", "10"
  ] |> List.map (fun (k, v) -> k ^ "=" ^ v)
  |> List.append
  [ "[Unit]"
  ; "Description=transient unit for " ^ syslog_key
  ; "DefaultDependencies=no" (* lifecycle tied to domain, not systemd *)
  ; "[Service]"]
  |> String.concat "\n"
  |> Xapi_stdext_unix.Unixext.write_string_to_file destination;
  action ~service "start"

let show ~service =
  let result = "Result" in
  let exec_main_pid = "ExecMainPID" in
  let exec_main_status = "ExecMainStatus" in
  let active_state = "ActiveState" in
  let props = [ result; exec_main_pid; exec_main_status; active_state ]
  |> String.concat ","
  |> fun properties -> [ "show"; "-p"; properties; service ]
  |> Forkhelpers.execute_command_get_output systemctl
  |> fst
  |> Astring.String.cuts ~sep:"\n"
  |> List.to_seq |> Seq.filter_map (Astring.String.cut ~sep:"=")
  |> List.of_seq in
  { result = props |> List.assoc result
  ; exec_main_pid = props |> List.assoc exec_main_pid |> int_of_string
  ; exec_main_status = props |> List.assoc exec_main_status |> int_of_string
  ; active_state = props |> List.assoc active_state
  }

let stop ~service =
  action ~service "stop";
  (* Stopping shouldn't fail because it should fall back to SIGKILL which should almost always work,
   * unless there is a kernel bug that keeps a process stuck.
   * In the unlikely scenario that this does fail we leave the transient service file behind
   * so that the failure can be investigated.
   * *)
  let status = show ~service in
  (* allow systemd to garbage-collect the status and the unit, preventing leaks.
   * See CollectMode in systemd.unit(5) for details. *)
  if status.exec_main_status <> 0 then
    begin try action ~service "reset-failed" with _ -> () end;
  let destination = Filename.concat run_path (service ^ ".service") in
  Xapi_stdext_unix.Unixext.unlink_safe destination;
  status

let is_active ~service =
  let status = Forkhelpers.safe_close_and_exec None None None [] systemctl ["is-active"; "--quiet"; service]
  |> Forkhelpers.waitpid |> snd in
  Unix.WEXITED 0 = status

let exists ~service =
  Sys.file_exists (Filename.concat run_path (service ^ ".service"))

let start_transient ?env ?properties ~service cmd args =
  if exists ~service then
    (* this can only happen if there is a bug in the caller *)
    invalid_arg (Printf.sprintf "Tried to start %s twice" service);
  try
    start_transient ?env ?properties ~service cmd args
  with e ->
    Backtrace.is_important e;
    (* If start failed we do not know what state the service is in:
     * try to stop it and clean up.
     * Stopping could fail as well, in which case report the original exception.
     * *)
    begin try let (_:status) = stop ~service in () with _ -> () end;
    raise e
