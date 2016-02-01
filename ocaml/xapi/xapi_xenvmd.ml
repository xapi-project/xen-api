exception NoConfigFile
exception XenvmdFailed

module D=Debug.Make(struct let name="xapi_xenvmd" end)
open D

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
    false
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | Unix.Unix_error (Unix.EAGAIN, _, _) -> true
  | Unix.Unix_error (Unix.EACCES, _, _) -> true
  | e -> raise e

let assert_config_present sr =
  try
    let l = configfile sr in
    ignore(Unix.stat l);
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise NoConfigFile
  | e -> raise e
           
let start sr =
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

let ensure_running sr =
  let rec retry n =
    if n >= 5 then raise XenvmdFailed;
    try
      if not (is_running sr)
      then start sr
    with
    | XenvmdFailed ->
      Thread.delay 5.0;
      retry (n+1)
  in
  retry 0

(* Let's try _really hard_ and _really carefully_ to kill xenvmd *)
let stop sr =
  let open Opt.Monad in
  let rec inner n =
    let signal = if n > 2 then Sys.sigkill else Sys.sigquit in
    let pidopt = try lockfile sr |> Unixext.string_of_file |> int_of_string |> return with _ -> None in
    pidopt >>= fun pid ->
    let cmdopt = try Some (Unix.readlink (Printf.sprintf "/proc/%d/exe" pid) |> Filename.basename) with _ -> None in
    cmdopt >>= fun cmd -> 
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

  
