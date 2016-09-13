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
(* Remote command execution *)

module D = Debug.Make(struct let name="remotecmd" end)
open D


open Forkhelpers



let do_cmd s cmd args =
  match with_logfile_fd "execute_command_get_output"
          (fun log_fd ->
             (* Capture stderr output for logging *)
             let pid = safe_close_and_exec (Some s) (Some s) (Some log_fd) [] cmd args in
             snd(waitpid pid)
          ) with
  | Success(log, status) ->
    debug "log: %s" log;
    begin match status with
      | Unix.WEXITED 0 -> ignore(log)
      | _ -> raise (Spawn_internal_error(log, "", status))
    end
  | Failure(log, exn) ->
    raise exn

let allowed_cmds = ["rsync","/usr/bin/rsync"]

(* Handle URIs of the form: vmuuid:port *)
let handler (req: Http.Request.t) s _ =
  let q = req.Http.Request.query in
  debug "remotecmd handler running";
  Xapi_http.with_context "Remote command" req s
    (fun __context ->
       let session_id = Context.get_session_id __context in
       if not (Db.Session.get_pool ~__context ~self:session_id) then
         begin
           failwith "Not a pool session"
         end;
       let cmd=List.assoc "cmd" q in
       let cmd=List.assoc cmd allowed_cmds in
       let args = List.map snd (List.filter (fun (x,y) -> x="arg") q) in
       do_cmd s cmd args
    )


