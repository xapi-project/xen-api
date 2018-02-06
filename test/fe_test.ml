(* stdin stdout stderr (name, fd) cmd args *)

let mkints n = Xapi_stdext_range.Range.to_list (Xapi_stdext_range.Range.make 0 n)

(* All combinations of stdin, stdout, stderr
   0 - 5 named fds on the commandline
   each named fd included either 1 or 2 times in the arg list
   0 - 900 additional fds *)

type config = {
  stdin: bool;
  stdout: bool;
  stderr: bool;
  named_fds: int;
  max_extra: int;
  extra: int;
}

let min_fds = 7
let max_fds = 1024 - 8 (* fe daemon has a bunch for its own use *)

let all_combinations fds = 
  let y = {
    stdin = false;
    stdout = false;
    stderr = false;
    named_fds = 0;
    max_extra = fds;
    extra = 0;
  } in
  let x = List.map (fun n -> { y with named_fds = n; max_extra = y.max_extra - n }) (mkints 5) in
  let x = List.map (fun x -> { x with stderr = true; max_extra = x.max_extra - 1 }) x @ x in
  let x = List.map (fun x -> { x with stdout = true; max_extra = x.max_extra - 1 }) x @ x in
  let x = List.map (fun x -> { x with stdin = true;  max_extra = x.max_extra - 1 }) x @ x in
  let x = List.concat (List.map (fun x -> List.map (fun n -> { x with extra = n }) (max_fds :: (mkints x.max_extra))) x) in
  x

let shuffle x = 
  let arr = Array.of_list x in
  let swap a b = let t = arr.(a) in arr.(a) <- arr.(b); arr.(b) <- t in
  for i = 0 to Array.length arr - 1 do
    swap i (Random.int (Array.length arr))
  done;
  Array.to_list arr

let irrelevant_strings = [ "irrelevant"; "not"; "important" ]

let one fds x =
  (*Printf.fprintf stderr "named_fds = %d\n" x.named_fds;
    Printf.fprintf stderr "extra = %d\n" x.extra;*)
  let fd = Unix.stdin in
  let make_names n = List.map (fun _ -> Uuidm.to_string (Uuidm.create `V4)) (mkints n) in
  let names = make_names x.named_fds in
  let cmdline_names = irrelevant_strings @ names @ names in
  let number_of_extra = x.extra in
  let other_names = make_names number_of_extra in

  let exe = Printf.sprintf "/proc/%d/exe" (Unix.getpid()) in
  let table = (fun x -> List.combine x (List.map (fun _ -> fd) x)) (names @ other_names) in
  let args = "slave" :: (string_of_int (fds - (x.max_extra - number_of_extra))) :: (shuffle cmdline_names) in
  (*  Printf.fprintf stderr "stdin = %s\n" (if x.stdin then "Some" else "None");
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
       table exe args)

let test_delay () =
  let start = Unix.gettimeofday () in
  let exe = Printf.sprintf "/proc/%d/exe" (Unix.getpid()) in
  let args = ["sleep"] in
  try
    Forkhelpers.execute_command_get_output ~timeout:4.0 exe args |> ignore;
    failwith "Failed to timeout"
  with
  | Forkhelpers.Subprocess_timeout ->
    Printf.printf "Caught timeout exception after %f seconds\n%!" (Unix.gettimeofday () -. start);
    ()
  | e ->
    failwith (Printf.sprintf "Failed with unexpected exception: %s" (Printexc.to_string e))

let test_notimeout () =
  let exe = Printf.sprintf "/proc/%d/exe" (Unix.getpid()) in
  let args = ["sleep"] in
  try
    Forkhelpers.execute_command_get_output exe args |> ignore;
    ()
  with
  | e ->
    failwith (Printf.sprintf "Failed with unexpected exception: %s" (Printexc.to_string e))

let fail x =
  Xapi_stdext_unix.Unixext.write_string_to_file "/tmp/fe-test.log" x;
  Printf.fprintf stderr "%s\n" x;
  assert false

let test_exitcode () =
  let run_expect cmd expected =
    try Forkhelpers.execute_command_get_output cmd [] |> ignore
    with
    | Forkhelpers.Spawn_internal_error(_, _, Unix.WEXITED n) ->
        if n <> expected then
          fail (Printf.sprintf "%s exited with code %d, expected %d" cmd n expected)
  in
  run_expect "/bin/false" 1;
  run_expect "/bin/xe-fe-test-no-command" 127;
  run_expect "/etc/hosts" 126;
  Printf.printf "\nCompleted exitcode tests\n"

let master fds =
  test_delay ();
  test_notimeout ();
  Printf.printf "\nCompleted timeout test\n";
  test_exitcode ();
  let combinations = shuffle (all_combinations fds) in
  Printf.printf "Starting %d tests\n" (List.length combinations);
  let i = ref 0 in
  let update_progress f x = 
    incr i;
    let frac = float_of_int (!i) /. (float_of_int (List.length combinations)) in
    let hashes = int_of_float (frac *. 70.) in
    let _percent = int_of_float (frac *. 100.) in
    Printf.printf "\r%5d %3d %s" !i (int_of_float (frac *. 100.)) (String.concat "" (List.map (fun _ -> "#") (mkints hashes)));
    flush stdout;
    f x;
  in
  List.iter (update_progress (one fds)) combinations;
  Printf.printf "\nCompleted %d tests\n" (List.length combinations)


let slave = function
  | [] -> failwith "Error, at least one fd expected"
  | total_fds :: rest ->
    let total_fds = int_of_string total_fds in
    let fds = List.filter (fun x -> not(List.mem x irrelevant_strings)) rest in
    (* Check that these fds are present *)
    let pid = Unix.getpid () in
    let path = Printf.sprintf "/proc/%d/fd" pid in
    let raw = 
      List.filter (* get rid of the fd used to read the directory *)
        (fun x -> try ignore(Unix.readlink (Filename.concat path x)); true with _ -> false) 
        (Array.to_list (Sys.readdir path)) in
    let pairs = List.map (fun x -> x, Unix.readlink (Filename.concat path x)) raw in
    (* Filter any of stdin,stdout,stderr which have been mapped to /dev/null *)
    let filtered = List.filter (fun x -> not(List.mem x [ "0", "/dev/null"; "1", "/dev/null"; "2", "/dev/null" ])) pairs in

    let ls = String.concat "\n" (List.map (fun (x, y) -> Printf.sprintf "%s -> %s" x y) filtered) in

    List.iter (fun fd -> if not(List.mem fd (List.map fst filtered))
                then fail (Printf.sprintf "fd %s not in /proc/%d/fd [ %s ]" fd pid ls)) fds;
    (* Check that we have the expected number *)
  (*
		  Printf.fprintf stderr "%s %d\n" total_fds (List.length present - 1)
		*)
    if total_fds <> (List.length filtered)
    then fail (Printf.sprintf "Expected %d fds; /proc/%d/fd has %d: %s" total_fds pid (List.length filtered) ls)

let sleep () =
  Unix.sleep 5;
  Printf.printf "Ok\n"

let usage () =
  Printf.printf "Usage:\n";
  Printf.printf
    " %s - perform a test of the fe service, testing up to the maximum fd count\n"
    Sys.argv.(0);
  Printf.printf
    " %s fds - perform a test of the fe services, testing up to <fds> fds\n"
    Sys.argv.(0);
  Printf.printf
    "          <fds> must be between %d and %d inclusive\n" min_fds max_fds;
  exit 1

let _ = 
  match Array.to_list Sys.argv with
  | _ :: "sleep" :: _ -> sleep ()
  | _ :: "slave" :: rest -> slave rest
  | _ :: [] -> master max_fds
  | _ :: fds :: [] -> begin
      match (try Some (int_of_string fds) with _ -> None) with
      | Some fds when fds >= min_fds && fds <= max_fds -> master fds
      | _ -> usage ()
    end
  | _ -> usage ()

