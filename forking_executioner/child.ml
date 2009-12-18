open Stringext

let debug (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s -> Printf.fprintf stderr "%s\n" s) fmt)

exception Cancelled

type state_t = {
  cmdargs : string list;
  env : string list;
  id_to_fd_map : (string * int option) list;
  ids_received : (string * Unix.file_descr) list;
  fd_sock2 : Unix.file_descr option;
  finished : bool;
}

open Fe_debug

let handle_fd_sock fd_sock state =
  try
    let (newfd,buffer) = Fecomms.receive_named_fd fd_sock in
    let dest_fd = List.assoc buffer state.id_to_fd_map in
    let fd = begin 
      match dest_fd with 
	| Some d -> 
	    debug "Received fd named: %s - duping to %d (from %d)" buffer d (Unixext.int_of_file_descr newfd);
	    let d = Unixext.file_descr_of_int d in
	    begin
	      if d = newfd
	      then ()
	      else begin
		Unix.dup2 newfd d;
		Unix.close newfd;
	      end
	    end;
	    d
	| None -> 
	    debug "Received fd named: %s (%d)" buffer (Unixext.int_of_file_descr newfd);
	    newfd
    end in
    {state with ids_received = (buffer,fd) :: state.ids_received}
  with Fecomms.Connection_closed -> 
    {state with fd_sock2 = None}

let handle_comms_sock comms_sock state =
  let call = Fecomms.read_raw_rpc comms_sock in
  match call with 
    | Fe.Cancel -> debug "Cancel"; raise Cancelled
    | Fe.Exec -> debug "Exec"; {state with finished=true;}
    | _ -> 
	debug "Ignoring unknown command";
	state

let handle_comms_no_fd_sock2 comms_sock fd_sock state =
  debug "Selecting in handle_comms_no_fd_sock2";
  let (ready,_,_) = Unix.select [comms_sock; fd_sock] [] [] (-1.0) in
  debug "Done";
  if List.mem fd_sock ready then begin
    debug "fd sock";
    let fd_sock2,_ = Unix.accept fd_sock in
    {state with fd_sock2=Some fd_sock2}
  end else begin
    debug "comms sock";
    handle_comms_sock comms_sock state    
  end
  
let handle_comms_with_fd_sock2 comms_sock fd_sock fd_sock2 state =
  debug "Selecting in handle_comms_with_fd_sock2";
  let (ready,_,_) = Unix.select [comms_sock; fd_sock2] [] [] (-1.0) in
  debug "Done";
  if List.mem fd_sock2 ready then begin
    debug "fd sock2";
    handle_fd_sock fd_sock2 state 
  end else begin
    debug "comms sock";
    handle_comms_sock comms_sock state    
  end

let handle_comms comms_sock fd_sock state =
  match state.fd_sock2 with 
    | None -> handle_comms_no_fd_sock2 comms_sock fd_sock state
    | Some x -> handle_comms_with_fd_sock2 comms_sock fd_sock x state

let run state comms_sock fd_sock fd_sock_path =
  let rec inner state =
    let state = handle_comms comms_sock fd_sock state in
    if state.finished then state else inner state
  in

  try
    dbuffer := Buffer.create 500;

    debug "Started: state.cmdargs = [%s]" (String.concat ";" (state.cmdargs));
    debug "Started: state.env = [%s]" (String.concat ";" (state.env));

    let fd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o0 in
    Unix.dup2 fd Unix.stdin;
    Unix.dup2 fd Unix.stdout;
    Unix.dup2 fd Unix.stderr;

    if fd<>Unix.stdin && fd<>Unix.stdout && fd<>Unix.stderr then Unix.close fd;

    let state = inner state in

    debug "Finished...";
    Unix.close fd_sock;
    (match state.fd_sock2 with Some x -> Unix.close x | None -> ());

    Unixext.unlink_safe fd_sock_path;
    
    (* Finally, replace placeholder uuids in the commandline arguments
       to be the string representation of the fd (where we don't care what
       fd it ends up being) *)
    let args = List.map (fun arg ->
      try 
	let (id_received,fd) = List.find (fun (id_received,fd) -> String.endswith id_received arg) state.ids_received in
	let stem = String.sub arg 0 (String.length arg - String.length id_received) in
	stem ^ (string_of_int (Unixext.int_of_file_descr fd));
      with _ -> arg) state.cmdargs in

    debug "Args after replacement = [%s]" (String.concat ";" args);    

    let fds = List.map snd state.ids_received in
    
    debug "I've received the following fds: [%s]\n" 
      (String.concat ";" (List.map (fun fd -> string_of_int (Unixext.int_of_file_descr fd)) fds));

    let result = Unix.fork () in

    if result=0 then begin
      (* child *)
      (* Now let's close everything except those fds mentioned in the ids_received list *)
      Unixext.close_all_fds_except ([Unix.stdin; Unix.stdout; Unix.stderr] @ fds);
      
      (* And exec *)
      Unix.execve (List.hd args) (Array.of_list args) (Array.of_list state.env)
    end else begin
      Fecomms.write_raw_rpc comms_sock (Fe.Execed result);

      List.iter (fun fd -> Unix.close fd) fds;
      let (pid,status) = Unix.waitpid [] result in
      let pr = match status with
	| Unix.WEXITED n -> Fe.WEXITED n
	| Unix.WSIGNALED n -> Fe.WSIGNALED n 
	| Unix.WSTOPPED n -> Fe.WSTOPPED n
      in
      let result = Fe.Finished (pr) in
      Fecomms.write_raw_rpc comms_sock result;
      Unix.close comms_sock;
      exit 0;
    end
  with 
    | Cancelled ->
	debug "Cancelling";
	Unix.close comms_sock;
	Unix.close fd_sock;
	Unixext.unlink_safe fd_sock_path;
	exit 0;
    | e -> 
	debug "Caught unexpected exception: %s" (Printexc.to_string e);
	write_log ();
	exit 1
	  
