open Stringext
open Pervasiveext

let debug (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s -> Printf.fprintf stderr "%s\n" s) fmt)

exception Cancelled

type syslog_stdout_t = {
  enabled : bool;
  key : string option;
}

type state_t = {
  cmdargs : string list;
  env : string list;
  id_to_fd_map : (string * int option) list;
  syslog_stdout : syslog_stdout_t;
  ids_received : (string * Unix.file_descr) list;
  fd_sock2 : Unix.file_descr option;
  finished : bool;
}

open Fe_debug

let handle_fd_sock fd_sock state =
  try
    let (newfd,buffer) = Fecomms.receive_named_fd fd_sock in
    if Fd_send_recv.int_of_fd newfd = -1 then begin
      debug "Failed to receive an fd associated with the message '%s'" buffer;
      failwith "Didn't get an fd"
    end;
    let dest_fd = List.assoc buffer state.id_to_fd_map in
    let fd = begin 
      match dest_fd with 
	| Some d -> 
	    debug "Received fd named: %s - duping to %d (from %d)" buffer d (Fd_send_recv.int_of_fd newfd);
	    let d = Fd_send_recv.fd_of_int d in
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
	    debug "Received fd named: %s (%d)" buffer (Fd_send_recv.int_of_fd newfd);
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
	stem ^ (string_of_int (Fd_send_recv.int_of_fd fd));
      with _ -> arg) state.cmdargs in

    debug "Args after replacement = [%s]" (String.concat ";" args);    
	let name = List.hd args in

    let fds = List.map snd state.ids_received in
    
    debug "I've received the following fds: [%s]\n" 
      (String.concat ";" (List.map (fun fd -> string_of_int (Fd_send_recv.int_of_fd fd)) fds));

    let in_childlogging = ref None in
    let out_childlogging = ref None in
    if state.syslog_stdout.enabled then begin
      (* Create a pipe used to listen to the child process's stdout *)
      let (in_fd, out_fd) = Unix.pipe () in
      in_childlogging := Some in_fd;
      out_childlogging := Some out_fd
    end;

    let result = Unix.fork () in

    if result=0 then begin
		(* child *)

        (* Make the child's stdout go into the pipe *)
		Opt.iter (fun out_fd -> Unix.dup2 out_fd Unix.stdout) !out_childlogging;

      (* Now let's close everything except those fds mentioned in the ids_received list *)
      Unixext.close_all_fds_except ([Unix.stdin; Unix.stdout; Unix.stderr] @ fds);
      
      (* Distance ourselves from our parent process: *)
      if Unix.setsid () == -1 then failwith "Unix.setsid failed";	  

      (* And exec *)
      Unix.execve name (Array.of_list args) (Array.of_list state.env)
    end else begin
      Fecomms.write_raw_rpc comms_sock (Fe.Execed result);

      List.iter (fun fd -> Unix.close fd) fds;

      (* Close the end of the pipe that's only supposed to be written to by the child process. *)
	  Opt.iter Unix.close !out_childlogging;

      let log_failure reason code = 
		(* The commandline might be too long to clip it *)
		let cmdline = String.concat " " args in
		let limit = 80 - 3 in
		let cmdline' = if String.length cmdline > limit then String.sub cmdline 0 limit ^ "..." else cmdline in
		if code=0 && name = "/opt/xensource/sm/ISOSR" && (String.has_substr cmdline' "sr_scan") then () else
			Syslog.syslog Fe_debug.syslog `LOG_ERR (Printf.sprintf "%d (%s) %s %d" result cmdline' reason code) in

	  let status = ref (Unix.WEXITED (-1)) in
	  finally
		  (fun () ->
			  Opt.iter
				  (fun in_fd ->
					  let key = (match state.syslog_stdout.key with None -> Filename.basename name | Some key -> key) in
					  (* Read from the child's stdout and write each one to syslog *)
					  Unixext.lines_iter
						  (fun line ->
							Syslog.syslog Fe_debug.syslog ~fac:`LOG_DAEMON `LOG_INFO (Printf.sprintf "%s[%d]: %s" key result line)
						  ) (Unix.in_channel_of_descr in_fd)
				  ) !in_childlogging
		  ) (fun () -> status := snd (Unix.waitpid [] result));

      let pr = match !status with
		| Unix.WEXITED n -> 
			(* Unfortunately logging this was causing too much spam *)
			if n <> 0 then log_failure "exitted with code" n;
			  Fe.WEXITED n
		| Unix.WSIGNALED n -> 
			  log_failure (Printf.sprintf "exitted with signal: %s" (Unixext.string_of_signal n)) 0;
			  Fe.WSIGNALED n 
		| Unix.WSTOPPED n -> 
			  log_failure (Printf.sprintf "stopped with signal: %s" (Unixext.string_of_signal n)) 0;
			  Fe.WSTOPPED n
      in
      let result = Fe.Finished (pr) in
	  (* If the controlling process has called Forkhelpers.dontwaitpid
		 then the comms_sock will be closed. We don't need to write our full debug
		 logs in syslog if this happens: *)
	  begin
		  try
			  Fecomms.write_raw_rpc comms_sock result
		  with Unix.Unix_error(Unix.EPIPE, _, _) -> ()
	  end;
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
	Unixext.unlink_safe fd_sock_path;
	exit 1
	  
