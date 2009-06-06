(* Remote command execution *)

module D = Debug.Debugger(struct let name="remotecmd" end)
open D


open Forkhelpers



let do_cmd s cmd args =
  let cmdline = String.concat " " (cmd :: args) in

  let pid = ref 0 in
  match with_logfile_fd "execute_command_get_output"
    (fun log_fd ->
      (* Capture stderr output for logging *)
      pid := safe_close_and_exec
	[ Dup2(s, Unix.stdout);
	  Dup2(s, Unix.stdin);
	  Dup2(log_fd, Unix.stderr);]
	[ Unix.stdout; Unix.stdin; Unix.stderr ] (* close all but these *)
	cmd args;
      snd(Unix.waitpid [] !pid)) with
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
let handler (req: Http.request) s =
  let q = req.Http.query in
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

  
