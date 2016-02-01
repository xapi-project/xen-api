(* 
 * Code to manage xenvmd
 *
 * This is a little bit of a layering violation.
 *
 *)

module D=Debug.Make(struct let name="xapi_xenvmd" end)
open D

let rpc x =
  if !Xcp_client.use_switch
  then Xcp_client.json_switch_rpc !(Storage_interface.queue_name) x
  else Xcp_client.http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string ~srcstr:"xapi" ~dststr:"xenops" Storage_interface.uri x
			  
(* We need to be able to talk SMAPIv2 *)
let local_url () = Http.Url.(Http { host="127.0.0.1"; auth=None; port=None; ssl=false }, { uri = "/services/SM"; query_params=["pool_secret",!Xapi_globs.pool_secret] } )
module SM = Storage_interface.Client(struct let rpc = rpc end)

exception NoConfigFile
exception XenvmdFailed

(* The storage backends know about these paths *)
let lockfile sr = Printf.sprintf "/var/lib/xenvmd/%s.lock" sr
let configfile sr = Printf.sprintf "/etc/xenvm.d/VG_XenStorage-%s.xenvmd.config" sr

let is_running sr =
  debug "Checking for xenvmd running for sr: %s" sr;
  try
    let l = lockfile sr in
    ignore(Unix.stat l);
    let fd = Unix.openfile l [ Unix.O_WRONLY; Unix.O_CREAT ] 0o0644 in
    Pervasiveext.finally
      (fun () -> Unix.lockf fd Unix.F_TEST 1)
      (fun () -> Unix.close fd);
    debug "Locked successfully: xenvmd not running";
    false
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> debug "Caught ENOENT (xenvmd not running)"; false (* Lockfile missing *)
  | Unix.Unix_error (Unix.EAGAIN, _, _) -> debug "Caught EAGAIN (xenvmd running)"; true  (* Locked by xenvmd *)
  | Unix.Unix_error (Unix.EACCES, _, _) -> debug "Caught EACCESS (xenvmd running)"; true  (* Locked by xenvmd *)
  | e -> raise e

let assert_config_present sr =
  try
    let l = configfile sr in
    ignore(Unix.stat l);
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise NoConfigFile
  | e -> raise e
           
let really_start sr =
  assert_config_present sr;
  try
    let output, stderr = Forkhelpers.execute_command_get_output !Xapi_globs.xenvmd_path
        [ "--daemon"; "--config"; configfile sr ] in
    (* Note that xenvmd will detect multiple copies of itself for the same VG.
       it will write an error message to the stdout/stderr but exit with code
       0 *)
    debug "Xenvmd started for SR %s. output='%s' stderr='%s'" sr output stderr
  with
  | Forkhelpers.Spawn_internal_error(log, output, x) ->
    debug "Failed to execute xenvmd. log='%s' output='%s'" log output;
    raise XenvmdFailed

(* Idempotent *)
let start sr =
  let rec retry n =
    if n >= 5 then raise XenvmdFailed;
    try
      if not (is_running sr)
      then really_start sr
    with
    | XenvmdFailed ->
      Thread.delay 5.0;
      retry (n+1)
    | NoConfigFile ->
      debug "No config file for SR=%s - assuming not thin-lvhd" sr;
  in
  retry 0

(* Let's try _really hard_ and _really carefully_ to kill xenvmd *)
let stop sr =
  info "Stopping xenvmd for SR uuid: %s" sr;
  let open Opt.Monad in
  let rec inner n =
    let signal = if n > 2 then Sys.sigkill else Sys.sigquit in
    debug "Getting pid";
    let pidopt = try lockfile sr |> Unixext.string_of_file |> int_of_string |> return with _ -> None in
    debug "Pid is: %d" (match pidopt with Some d -> d | None -> -1);
    pidopt >>= fun pid ->
    let cmdopt = try Some (Unix.readlink (Printf.sprintf "/proc/%d/exe" pid) |> Filename.basename) with _ -> None in
    cmdopt >>= fun cmd -> 
    debug "Got cmd: %s" cmd;
    if cmd="xenvmd" then begin
      Unix.kill pid signal;
      Thread.delay 0.5;
        try
          Unix.kill pid 0;
          Thread.delay 1.0;
          inner (n+1)                
        with
        | Unix.Unix_error (Unix.ESRCH, _, _) -> None
        | e -> raise e
    end else None
  in
  ignore (inner 0)

let operate_on_shared_srs op =
  let attached_srs = SM.SR.list ~dbg:"xapi_xenvmd" in
  Server_helpers.exec_with_new_task "Xapi_xenvmd: Managing xenvmd instances" (fun __context ->
    List.iter (fun uuid ->
      let self = Db.SR.get_by_uuid ~__context ~uuid in
      let shared = Db.SR.get_shared ~__context ~self in
      if shared then op uuid) attached_srs)
    
(* Called on transition to slave, on the way down *)
(* We must assume we're a slave, so not the SR master for any shared SRs *)
let kill_non_sr_master_xenvmds () = operate_on_shared_srs stop

(* Start xenvmd for shared SRs *)
let start_xenvmds_for_shared_srs () = operate_on_shared_srs start
    
