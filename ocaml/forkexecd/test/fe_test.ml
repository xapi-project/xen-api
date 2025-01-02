(* stdin stdout stderr (name, fd) cmd args *)

let mkints n = List.init n Fun.id

(* All combinations of stdin, stdout, stderr
   0 - 5 named fds on the commandline
   each named fd included either 1 or 2 times in the arg list
   0 - 900 additional fds *)

type config = {
    stdin: bool
  ; stdout: bool
  ; stderr: bool
  ; named_fds: int
  ; max_extra: int
  ; extra: int
}

let min_fds = 7

let max_fds = 1024 - 13 (* fe daemon has a bunch for its own use *)

let fail x =
  Xapi_stdext_unix.Unixext.write_string_to_file "/tmp/fe-test.log" x ;
  Printf.fprintf stderr "%s\n" x ;
  assert false

let fail fmt = Format.ksprintf fail fmt

let all_combinations fds =
  let y =
    {
      stdin= false
    ; stdout= false
    ; stderr= false
    ; named_fds= 0
    ; max_extra= fds
    ; extra= 0
    }
  in
  let x =
    List.map
      (fun n -> {y with named_fds= n; max_extra= y.max_extra - n})
      (mkints 5)
  in
  let x =
    List.map (fun x -> {x with stderr= true; max_extra= x.max_extra - 1}) x @ x
  in
  let x =
    List.map (fun x -> {x with stdout= true; max_extra= x.max_extra - 1}) x @ x
  in
  let x =
    List.map (fun x -> {x with stdin= true; max_extra= x.max_extra - 1}) x @ x
  in
  let x =
    List.concat
      (List.map
         (fun x ->
           List.map (fun n -> {x with extra= n}) (max_fds :: mkints x.max_extra)
         )
         x
      )
  in
  x

let shuffle x =
  let arr = Array.of_list x in
  let swap a b =
    let t = arr.(a) in
    arr.(a) <- arr.(b) ;
    arr.(b) <- t
  in
  for i = 0 to Array.length arr - 1 do
    swap i (Random.int (Array.length arr))
  done ;
  Array.to_list arr

let fds_fold f init =
  let path = "/proc/self/fd" in
  (* get rid of the fd used to read the directory *)
  Array.fold_right
    (fun fd_num acc ->
      try
        let link = Unix.readlink (Filename.concat path fd_num) in
        f fd_num link acc
      with _ -> acc
    )
    (Sys.readdir path) init

let fd_list () = fds_fold (fun fd_num link l -> (fd_num, link) :: l) []

let fd_count () = fds_fold (fun _ _ n -> n + 1) 0

let irrelevant_strings = ["irrelevant"; "not"; "important"]

let exe = Printf.sprintf "/proc/%d/exe" (Unix.getpid ())

let one fds x =
  (*Printf.fprintf stderr "named_fds = %d\n" x.named_fds;
    Printf.fprintf stderr "extra = %d\n" x.extra;*)
  let fd = Unix.stdin in
  let make_names n =
    List.map (fun _ -> Uuidx.(to_string (make ()))) (mkints n)
  in
  let names = make_names x.named_fds in
  let cmdline_names = irrelevant_strings @ names @ names in
  let number_of_extra = x.extra in
  let other_names = make_names number_of_extra in

  let table =
    (fun x -> List.combine x (List.map (fun _ -> fd) x)) (names @ other_names)
  in
  let args =
    "slave"
    :: string_of_int (fds - (x.max_extra - number_of_extra))
    :: shuffle cmdline_names
  in
  (* Printf.fprintf stderr "stdin = %s\n" (if x.stdin then "Some" else "None");
     Printf.fprintf stderr "stdout = %s\n" (if x.stdout then "Some" else "None");
     Printf.fprintf stderr "stderr = %s\n" (if x.stderr then "Some" else "None");
     List.iter (fun (uuid, _) -> Printf.fprintf stderr "uuid %s -> stdin\n" uuid) table;
     Printf.fprintf stderr "%s %s\n" exe (String.concat " " args);
  *)
  Forkhelpers.waitpid_fail_if_bad_exit
    (Forkhelpers.safe_close_and_exec
       (if x.stdin then Some fd else None)
       (if x.stdout then Some fd else None)
       (if x.stderr then Some fd else None)
       table exe args
    )

let test_delay () =
  let start = Unix.gettimeofday () in
  let args = ["sleep"] in
  (* Need to have fractional part because some internal usage split integer
     and fractional and do computation.
     Better to have a high fractional part (> 0.5) to more probably exceed
     the unit.
  *)
  let timeout = 1.7 in
  try
    Forkhelpers.execute_command_get_output ~timeout exe args |> ignore ;
    fail "Failed to timeout"
  with
  | Forkhelpers.Subprocess_timeout ->
      let elapsed = Unix.gettimeofday () -. start in
      Printf.printf "Caught timeout exception after %f seconds\n%!" elapsed ;
      if elapsed < timeout then
        failwith "Process exited too soon" ;
      if elapsed > timeout +. 0.2 then
        failwith "Excessive time elapsed"
  | e ->
      fail "Failed with unexpected exception: %s" (Printexc.to_string e)

let test_notimeout () =
  let args = ["sleep"] in
  try
    Forkhelpers.execute_command_get_output exe args |> ignore ;
    ()
  with e -> fail "Failed with unexpected exception: %s" (Printexc.to_string e)

let expect expected s =
  if s <> expected ^ "\n" then
    fail "output %s expected %s" s expected

let test_exitcode () =
  let run_expect cmd expected =
    try Forkhelpers.execute_command_get_output cmd [] |> ignore
    with Forkhelpers.Spawn_internal_error (_, _, Unix.WEXITED n) ->
      if n <> expected then
        fail "%s exited with code %d, expected %d" cmd n expected
  in
  run_expect "/bin/false" 1 ;
  run_expect "/bin/xe-fe-test-no-command" 127 ;
  run_expect "/bin/xe-fe-no-path/xe-fe-test-no-command" 127 ;
  run_expect "/etc/hosts" 126 ;
  Printf.printf "\nCompleted exitcode tests\n"

let test_output () =
  let expected_out = "output string" in
  let expected_err = "error string" in
  let args = ["echo"; expected_out; expected_err] in
  let out, err = Forkhelpers.execute_command_get_output exe args in
  expect expected_out out ;
  expect expected_err err ;
  print_endline "Completed output tests"

let test_input () =
  let input = "input string" in
  let args = ["replay"] in
  let out, _ =
    Forkhelpers.execute_command_get_output_send_stdin exe args input
  in
  expect input out ;
  print_endline "Completed input tests"

(* This test tests a failure inside Forkhelpers.safe_close_and_exec.
   Although the exact way of this reproduction is never supposed to
   happen in the real world, an internal failure could happen for instance
   if forkexecd daemon is restarted for a moment, so make sure we are
   able to detect and handle these cases *)
let test_internal_failure_error () =
  let initial_fd_count = fd_count () in
  let leak_fd_detect () =
    let current_fd_count = fd_count () in
    if current_fd_count <> initial_fd_count then
      fail "File descriptor leak detected initially %d files, now %d"
        initial_fd_count current_fd_count
  in
  (* this weird function will open and close "num" file descriptors
     and returns the last (now closed) of them, mainly to get an invalid
     file descriptor with some closed one before *)
  let rec waste_fds num =
    let fd = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o0 in
    let ret = if num = 0 then fd else waste_fds (num - 1) in
    Unix.close fd ; ret
  in
  let fd = waste_fds 20 in
  let args = ["sleep"] in
  try
    Forkhelpers.safe_close_and_exec None (Some fd) None [] exe args |> ignore ;
    fail "Expected an exception"
  with
  | Fd_send_recv.Unix_error _ | Unix.Unix_error (Unix.EBADF, _, _) ->
      leak_fd_detect ()
  | e ->
      Printexc.print_backtrace stderr ;
      fail "Failed with unexpected exception: %s" (Printexc.to_string e)

(* Emulate syslog and output lines to returned channel *)
let syslog_lines sockname =
  let clean () = try Unix.unlink sockname with _ -> () in
  clean () ;
  let sock = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let rd_pipe, wr_pipe = Unix.pipe ~cloexec:true () in
  Unix.bind sock (Unix.ADDR_UNIX sockname) ;
  match Unix.fork () with
  | 0 ->
      (* child, read from socket and output to pipe *)
      let term_handler = Sys.Signal_handle (fun _ -> clean () ; exit 0) in
      Sys.set_signal Sys.sigint term_handler ;
      Sys.set_signal Sys.sigterm term_handler ;
      Unix.close rd_pipe ;
      Unix.dup2 wr_pipe Unix.stdout ;
      Unix.close wr_pipe ;
      let buf = Bytes.create 1024 in
      let rec fwd () =
        let l = Unix.recv sock buf 0 (Bytes.length buf) [] in
        if l > 0 then (
          print_bytes (Bytes.sub buf 0 l) ;
          print_newline () ;
          (fwd [@tailcall]) ()
        )
      in
      fwd () ; exit 0
  | pid ->
      Unix.close sock ;
      Unix.close wr_pipe ;
      (pid, Unix.in_channel_of_descr rd_pipe)

let test_syslog with_stderr =
  let rec syslog_line ic =
    let line = input_line ic in
    (* ignore log lines from daemon *)
    if String.ends_with ~suffix:"\\x0A" line then
      syslog_line ic
    else
      let re = Str.regexp ": " in
      match Str.bounded_split re line 3 with
      | _ :: _ :: final :: _ ->
          final ^ "\n"
      | _ ->
          raise Not_found
  in
  let expected_out = "output string" in
  let expected_err = "error string" in
  let args = ["echo"; expected_out; expected_err] in
  let child, ic = syslog_lines "/tmp/xyz" in
  let out, err =
    Forkhelpers.execute_command_get_output ~syslog_stdout:Syslog_DefaultKey
      ~redirect_stderr_to_stdout:with_stderr exe args
  in
  expect "" (out ^ "\n") ;
  if with_stderr then
    expect "" (err ^ "\n")
  else
    expect expected_err err ;
  Unix.sleepf 0.05 ;
  Syslog.log Syslog.Daemon Syslog.Err "exe: XXX\n" ;
  Syslog.log Syslog.Daemon Syslog.Err "exe: YYY\n" ;
  let out = syslog_line ic in
  expect expected_out out ;
  let err = syslog_line ic in
  let expected = if with_stderr then expected_err else "XXX" in
  expect expected err ;
  Unix.kill child Sys.sigint ;
  Unix.waitpid [] child |> ignore ;
  close_in ic ;
  print_endline "Completed syslog test"

let master fds =
  Printf.printf "\nPerforming timeout tests\n%!" ;
  test_delay () ;
  test_notimeout () ;
  Printf.printf "\nCompleted timeout test\n%!" ;
  test_exitcode () ;
  Printf.printf "\nPerforming input/output tests\n%!" ;
  test_output () ;
  test_input () ;
  Printf.printf "\nPerforming internal failure test\n%!" ;
  test_internal_failure_error () ;
  Printf.printf "\nPerforming syslog tests\n%!" ;
  test_syslog true ;
  test_syslog false ;

  let combinations = shuffle (all_combinations fds) in
  Printf.printf "Starting %d tests\n%!" (List.length combinations) ;
  let i = ref 0 in
  let update_progress f x =
    incr i ;
    let frac = float_of_int !i /. float_of_int (List.length combinations) in
    let hashes = int_of_float (frac *. 70.) in
    let _percent = int_of_float (frac *. 100.) in
    Printf.printf "\r%5d %3d %s" !i
      (int_of_float (frac *. 100.))
      (String.concat "" (List.map (fun _ -> "#") (mkints hashes))) ;
    flush stdout ;
    f x
  in
  List.iter (update_progress (one fds)) combinations ;
  Printf.printf "\nCompleted %d tests\n" (List.length combinations)

let slave = function
  | [] ->
      fail "Error, at least one fd expected"
  | total_fds :: rest ->
      let total_fds = int_of_string total_fds in
      let fds =
        List.filter (fun x -> not (List.mem x irrelevant_strings)) rest
      in
      (* Check that these fds are present *)
      let pairs = fd_list () in
      (* Filter any of stdin,stdout,stderr which have been mapped to /dev/null *)
      let filtered =
        List.filter
          (fun x ->
            not
              (List.mem x
                 [("0", "/dev/null"); ("1", "/dev/null"); ("2", "/dev/null")]
              )
          )
          pairs
      in

      let ls =
        String.concat "\n"
          (List.map (fun (x, y) -> Printf.sprintf "%s -> %s" x y) filtered)
      in

      List.iter
        (fun fd ->
          if not (List.mem fd (List.map fst filtered)) then
            fail "fd %s not in /proc/self/fd [ %s ]" fd ls
        )
        fds ;
      (* Check that we have the expected number *)
      (*
		  Printf.fprintf stderr "%s %d\n" total_fds (List.length present - 1)
		*)
      if total_fds <> List.length filtered then
        fail "Expected %d fds; /proc/self/fd has %d: %s" total_fds
          (List.length filtered) ls

let sleep () = Unix.sleep 3 ; Printf.printf "Ok\n"

let echo out err =
  if out <> "" then print_endline out ;
  if err <> "" then prerr_endline err

let replay () =
  let line = read_line () in
  print_endline line

let usage () =
  Printf.printf "Usage:\n" ;
  Printf.printf
    " %s - perform a test of the fe service, testing up to the maximum fd count\n"
    Sys.argv.(0) ;
  Printf.printf
    " %s fds - perform a test of the fe services, testing up to <fds> fds\n"
    Sys.argv.(0) ;
  Printf.printf "          <fds> must be between %d and %d inclusive\n" min_fds
    max_fds ;
  exit 1

let _ =
  match Array.to_list Sys.argv with
  | _ :: "sleep" :: _ ->
      sleep ()
  | _ :: "slave" :: rest ->
      slave rest
  | _ :: "echo" :: out :: err :: _ ->
      echo out err
  | _ :: "replay" :: _ ->
      replay ()
  | [_] ->
      master max_fds
  | [_; fds] -> (
    match try Some (int_of_string fds) with _ -> None with
    | Some fds when fds >= min_fds && fds <= max_fds ->
        master fds
    | _ ->
        usage ()
  )
  | _ ->
      usage ()
