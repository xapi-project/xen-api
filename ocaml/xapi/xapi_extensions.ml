(*
 * Copyright (C) 2015 Citrix Systems Inc.
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
module D = Debug.Make(struct let name="xapi" end)
open D

(* The Xapi_globs.xapi_extensions_root contain scripts which are invoked with
   XenAPI XMLRPC on stdin, and must respond with XenAPI XMLRPC on stdout. *)

(* Only scripts in the Xapi_globs.xapi_extensions_root can be called *)
let find_extension name =
  let all = try Array.to_list (Sys.readdir !Xapi_globs.xapi_extensions_root) with _ -> [] in
  (* Sys.readdir output doesn't include "." or ".." *)
  if List.mem name all
  then Filename.concat !Xapi_globs.xapi_extensions_root name
  else raise (Api_errors.Server_error(Api_errors.message_method_unknown, [name]))

(* Execute the extension with XMLRPC-over-cmdline/stdout convention. *)
let call_extension rpc =
  try
    let path = find_extension rpc.Rpc.name in

    let output, _ =
      try
        Forkhelpers.execute_command_get_output_send_stdin path [ "--xmlrpc" ] (Xmlrpc.string_of_call rpc)
      with
      | Forkhelpers.Spawn_internal_error(log, output, Unix.WSTOPPED i) ->
        raise (Api_errors.Server_error (Api_errors.internal_error, [path; "task stopped"; output; log ]))
      | Forkhelpers.Spawn_internal_error(log, output, Unix.WSIGNALED i) ->
        raise (Api_errors.Server_error (Api_errors.internal_error, [path; Printf.sprintf "signal: %s" (Stdext.Unixext.string_of_signal i); output; log ]))
      | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED i) ->
        raise (Api_errors.Server_error (Api_errors.internal_error, [path; "non-zero exit"; output; log ])) in
    begin
      try
        Xmlrpc.response_of_string output
      with e ->
        raise (Api_errors.Server_error(Api_errors.internal_error, [ path; "failed to parse extension output"; output; Printexc.to_string e ]))
    end;
  with
  | Api_errors.Server_error(code, params) ->
    API.response_of_failure code params
  | e ->
    error "Unexpected exception calling extension %s: %s" rpc.Rpc.name (Printexc.to_string e);
    Debug.log_backtrace e (Backtrace.get e);
    API.response_of_failure Api_errors.internal_error [ rpc.Rpc.name; Printexc.to_string e ]
