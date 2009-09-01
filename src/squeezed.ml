let default_pidfile = "/var/run/squeezed.pid" 
let log_file_path = "file:/var/log/squeezed.log" 


open Pervasiveext 
open Squeezed_rpc
open Squeezed_state

(* We assume only one instance of a named service logs in at a time and therefore can use
   the service name as a session_id. *)

(* val login: service -> session_id *)
let login args = 
  if not(List.mem_assoc _service_name args)
  then [ _code, _error_missing_argument_code; _description, _service_name ]
  else begin
    let service_name = List.assoc _service_name args in
    Debug.with_thread_associated (Printf.sprintf "login(%s)" service_name)
      (fun () ->
	 (* remove any existing reservations associated with this service *)
	 with_xc_and_xs
	   (fun xc xs ->
	      xs.Xs.rm (state_path _service ^ "/" ^ service_name)
	   )
      ) ();
    [ _session_id, service_name ]
  end

(* val reserve_memory: session_id -> kib -> reservation_id *)
let reserve_memory args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _kib args)
  then [ _code, _error_missing_argument_code; _description, _kib ]
  else begin
    let session_id = List.assoc _session_id args 
    and kib = List.assoc _kib args in
    let reservation_id = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    try
      Debug.with_thread_associated (Printf.sprintf "reserve_memory(%s, %s)" session_id kib)
	(fun () ->
	   with_xc_and_xs
	     (fun xc xs ->
		Squeeze_xen.free_memory ~xc ~xs (Int64.of_string kib);
		debug "reserved %s kib for reservation %s" kib reservation_id;
		add_reservation xs _service session_id reservation_id kib
	     )
	) ();
      [ _reservation_id, reservation_id ]
    with
    | Squeeze_xen.Cannot_free_this_much_memory _ ->
	[ _code, _error_cannot_free_this_much_memory_code; _description, kib ]
    | Squeeze_xen.Domains_refused_to_cooperate domids ->
	[ _code, _error_domains_refused_to_cooperate_code; _description, String.concat "," (List.map string_of_int domids) ]
  end

(* val reserve_memory_range: session_id -> min -> max -> reservation_id *)
let reserve_memory_range args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _min args)
  then [ _code, _error_missing_argument_code; _description, _min ]
  else if not(List.mem_assoc _max args)
  then [ _code, _error_missing_argument_code; _description, _max ]
  else begin
    let session_id = List.assoc _session_id args in
    let min = List.assoc _min args in
    let max = List.assoc _max args in
    let reservation_id = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    try
      Debug.with_thread_associated (Printf.sprintf "reserve_memory_range(%s, %s, %s)" session_id min max)
	(fun () ->
	   with_xc_and_xs
	     (fun xc xs ->
		let amount = Squeeze_xen.free_memory_range ~xc ~xs (Int64.of_string min) (Int64.of_string max) in
		debug "reserved %Ld kib for reservation %s" amount reservation_id;
		add_reservation xs _service session_id reservation_id (Int64.to_string amount);
		[ _kib, Int64.to_string amount; _reservation_id, reservation_id ]
	     )
	) ()
    with 
    | Squeeze_xen.Cannot_free_this_much_memory _ ->
	[ _code, _error_cannot_free_this_much_memory_code; _description, min ]
    | Squeeze_xen.Domains_refused_to_cooperate domids ->
	[ _code, _error_domains_refused_to_cooperate_code; _description, String.concat "," (List.map string_of_int domids) ]
  end
  

(* val delete_reservation: reservation_id -> unit *)
let delete_reservation args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _reservation_id args)
  then [ _code, _error_missing_argument_code; _description, _reservation_id ]
  else begin
    let session_id = List.assoc _session_id args 
    and reservation_id = List.assoc _reservation_id args in
    Debug.with_thread_associated (Printf.sprintf "delete_reservation(%s)" reservation_id)
      (fun () ->
	 with_xc_and_xs
	   (fun xc xs ->
	      del_reservation xs _service session_id reservation_id;
	      []
	   )
      ) ()
  end

(* val transfer_reservation_to_domain: session_id -> reservation_id -> domid -> unit *)
let transfer_reservation_to_domain args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _reservation_id args)
  then [ _code, _error_missing_argument_code; _description, _reservation_id ]
  else if not(List.mem_assoc _domid args)
  then [ _code, _error_missing_argument_code; _description, _domid ]
  else begin
    let session_id = List.assoc _session_id args 
    and reservation_id = List.assoc _reservation_id args 
    and domid = int_of_string (List.assoc _domid args) in
    try
      Debug.with_thread_associated (Printf.sprintf "transfer_reservation_to_domain(%s, %d)" reservation_id domid)
	(fun () ->
	   with_xc_and_xs
	     (fun xc xs ->
		let kib = xs.Xs.read (reservation_path _service session_id reservation_id) in
		(* This code is single-threaded, no need to make this transactional: *)
		xs.Xs.write (xs.Xs.getdomainpath domid ^ "/memory/initial-reservation") kib;
		del_reservation xs _service session_id reservation_id;
		[]
	     )
	) ()
    with Xb.Noent ->
      [ _code, _error_unknown_reservation; _description, reservation_id ]
  end

(* val balance_memory: unit -> unit *)
let balance_memory args = 
  Debug.with_thread_associated "balance"
    (fun () ->
       with_xc_and_xs
	 (fun xc xs ->
	    Squeeze_xen.balance_memory ~xc ~xs
	 )
    ) ();
  []

(* val reopen_logs: unit -> unit *)
let reopen_logs _ = 
  debug "Reopening logfiles";
  Logs.reopen ();
  debug "Logfiles reopened";
  []

let function_table = [
  "echo", (fun x -> x);
  _login, login;
  _reserve_memory, reserve_memory;
  _reserve_memory_range, reserve_memory_range;
  _transfer_reservation_to_domain, transfer_reservation_to_domain;
  _delete_reservation, delete_reservation;
  _balance_memory, balance_memory;
  _reopen_logs, reopen_logs;
]

let read_hostname () = 
  (* Get the hostname and configure the debug module *)
  let pid, pipe_exit = Unixext.execv_get_output "/bin/hostname" [| "/bin/hostname" |] in 
  let hostname = String.make 128 ' ' in
  let len = Unix.read pipe_exit hostname 0 (String.length hostname) in
  let hostname = Stringext.String.strip Stringext.String.isspace (String.sub hostname 0 len) in
  Unix.waitpid [] pid;
  hostname

(** Called periodically to look for unbalanced memory and take corrective action *)
let idle_callback ~xc ~xs () = 
  if Squeeze_xen.is_host_memory_unbalanced ~xc ~xs
  then Debug.with_thread_associated "auto-balance" (fun () -> Squeeze_xen.balance_memory ~xc ~xs) ()
  
let _ = 
  let pidfile = ref default_pidfile in
  let daemonize = ref false in
 
  Arg.parse (Arg.align [
	       "-debug", Arg.Set print_debug, Printf.sprintf "Set debug to stdout rather than log file (default %s)" log_file_path;
	       "-daemon", Arg.Set daemonize, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
	     ])
    (fun _ -> failwith "Invalid argument")
    "Usage: squeezed [-daemon] [-pidfile filename]";

  Logs.reset_all [ log_file_path ];

  let hostname = read_hostname () in
  Debug.get_hostname := (fun () -> hostname);

  if !daemonize then Unixext.daemonize ();

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

  debug "Starting daemon";
  try
    with_xc_and_xs (fun xc xs -> Rpc.loop ~xc ~xs ~service:_service ~function_table ~idle_timeout:10. ~idle_callback:(idle_callback ~xc ~xs) () );
    debug "Graceful shutdown";
    exit 0
  with e ->
    debug "Caught exception %s" (Printexc.to_string e);
    exit 1

