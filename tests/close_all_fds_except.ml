
(** Test the close_all_fds_except *)

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let list_of_option_array x = List.concat (List.map (function Some x -> [ x ] | None -> []) (Array.to_list x))

let compare (fds: Unix.file_descr list) = 
  let pid = Unix.getpid () in
  let dir = Printf.sprintf "/proc/%d/fd" pid in
  let string_of ints = String.concat "; " (List.map string_of_int (List.sort compare ints)) in
  let open_fds = List.map int_of_string (Array.to_list (Sys.readdir dir)) in
  Printf.printf "actually open  = [ %s ]\n" (string_of open_fds);
  let fds' = List.map (fun x -> (Obj.magic x: int)) fds in
  Printf.printf "should be open = [ %s ] (NB spurious extra Sys.readdir fd)\n" (string_of fds');
  (* fds' are the ones we think should be open. open_fds are the ones which actually are *)
  let should_be_open = set_difference fds' open_fds in
  let should_be_closed = set_difference open_fds fds' in
  List.iter (Printf.printf "FD %d should be open but is in fact closed.\n") should_be_open;
  (* NB there is always one extra fd corresponding to the open directory handle *)
  let fatal = if List.length should_be_closed = 1 then "Non-fatal:" else "FATAL:" in
  List.iter (Printf.printf "%s FD %d should be closed but is in fact open.\n" fatal) should_be_closed;
  if should_be_open <> [] || List.length should_be_closed <> 1
  then failwith "Test failed"

let _ = 
  let cycles = ref 10 in
  Arg.parse 
    [ "-cycles", Arg.Set_int cycles, Printf.sprintf "number of iterations (default %d)" !cycles;
    ]
    (fun x -> Printf.printf "Ignoring unknown argument: %s" x)
    "Test the close_all_fds_except code";

  let inouterr = [ Unix.stdout; Unix.stderr; Unix.stdin ] in

  for i = 1 to !cycles do
    compare inouterr;
    
    let dev_null () = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o0 in
    let fds = Array.init (Random.int 100) (fun _ -> Some (dev_null ())) in
    (* fds is now an array of file descriptor options *)
    compare (inouterr @ (list_of_option_array fds));
    
    (* close some of the fds *)
    let fds = Array.map (function 
			 | Some x -> if Random.bool () then (Unix.close x; None) else Some x
			 | None -> None) fds in
    compare (inouterr @ (list_of_option_array fds));
    
    (* choose some of the fds to keep *)
    let chosen = List.filter (fun _ -> Random.bool ()) (list_of_option_array fds) in
    Unixext.close_all_fds_except (inouterr @ chosen);
    compare (inouterr @ chosen);
    List.iter Unix.close chosen;
    compare inouterr;
  done;
  print_endline "All tests passed"
  


	      
