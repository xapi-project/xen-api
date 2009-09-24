
(** Take a list of file descriptors, a program and a set of arguments. Close
    all fds except the set provided and then exec the supplied program. *)


let usage () = 
  Printf.printf "Usage:\n";
  Printf.printf "  %s fd0 .. fdN -- argv0 .. argvN\n" Sys.argv.(0);
  exit 1

let _ = 
  let fds = ref [] in
  let args = ref [] in
  let found_dash = ref false in
  for i = 1 to Array.length(Sys.argv) - 1 do
    let x = Sys.argv.(i) in
    if x = "--" 
    then found_dash := true
    else if !found_dash then args := x :: !args else fds := x :: !fds
  done;
  let fds = List.map (fun x -> Unixext.file_descr_of_int (int_of_string x)) !fds in
  let args = List.rev !args in

  if List.length args < 1 then usage();
  
  Unixext.close_all_fds_except fds;
  let cmd = List.hd args in
  (* CA-18955: xapi now runs with priority -3. We then set his sons priority to 0. *)
  Unix.nice (-(Unix.nice 0));
  
  let rec doit n =
    if n=0 then failwith "Max retries exceeded";
    try
      Unix.execv cmd (Array.of_list args)
    with 
      | Unix.Unix_error (Unix.EUNKNOWNERR x,b,c) as e ->
	  Printf.fprintf stderr "Unix error: %s (%s,%s)\n" (Unix.error_message (Unix.EUNKNOWNERR x)) b c;
	  if x=26 (* ETXTBSY *) then 
	    (Thread.delay 1.0; doit (n-1)) else raise e
      | Unix.Unix_error (a,b,c) as e ->
	  Printf.fprintf stderr "Unix error: %s (%s,%s)\n" (Unix.error_message a) b c;
	  raise e
  in
  doit 5
