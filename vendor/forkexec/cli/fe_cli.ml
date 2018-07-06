(* Simple command-line test program which invokes the FE service *)

let _ = 
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr " %s <command> [arg1] ... [argN]\n" Sys.argv.(0);
    exit 1
  | cmd :: args ->
    try
      let out, err = Forkhelpers.execute_command_get_output ~syslog_stdout:Forkhelpers.NoSyslogging cmd args in
      Printf.printf "stdout=[%s]\n" out;
      Printf.printf "stderr=[%s]\n" err;
      exit 0
    with Forkhelpers.Spawn_internal_error(err, out, ps) ->
      Printf.fprintf stderr "stdout=[%s]\n" out;
      Printf.fprintf stderr "stderr=[%s]\n" err;
      let n = 
        match ps with
        | Unix.WEXITED n ->
          Printf.fprintf stderr "WEXITED %d\n" n;
          n
        | Unix.WSTOPPED n ->
          Printf.fprintf stderr "WSTOPPED %d\n" n;
          n
        | Unix.WSIGNALED n ->
          Printf.fprintf stderr "WSIGNALED %d\n" n;
          n in
      exit n

