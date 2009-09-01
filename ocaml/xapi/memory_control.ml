module D = Debug.Debugger(struct let name="memory_control" end)
open D

open Pervasiveext
open Threadext

let catch_generic_errors f = 
  try
    f ()
  with 
  | Squeezed_rpc.Server_not_registered ->
      error "The ballooning daemon is not running";
      failwith "The ballooning daemon is not running" (* should never happen but just in case *)
  | Squeezed_rpc.Error(code, descr) ->
      error "Got unexpected (%s, %s) from ballooning daemon" code descr;
      failwith (Printf.sprintf "The ballooning daemon failed: %s, %s" code descr) (* should never happen *)
  | e ->
      error "Unexpected exception talking to ballooning daemon: %s" (ExnHelper.string_of_exn e);
      raise e

let call_daemon xs fn args = Squeezed_rpc.Rpc.client ~xs ~service:Squeezed_rpc._service ~fn ~args
let ignore_results (_: (string * string) list) = ()

(** Log into the ballooning service (on every xapi start) *)
let login ~xs = 
  catch_generic_errors
    (fun () ->
       debug "logging into ballooning service";
       let args = [ Squeezed_rpc._service_name, "xapi" ] in
       let results = call_daemon xs Squeezed_rpc._login args in
       List.assoc Squeezed_rpc._session_id results
    )

(** Maintain a cached login session with the ballooning service; return the cached value on demand *)
let get_session_id = 
  let session_id = ref None in
  let m = Mutex.create () in
  fun ~xs ->
    Mutex.execute m
      (fun () ->
	 match !session_id with
	 | Some x -> x
	 | None ->
	     let s = login ~xs in
	     session_id := Some s;
	     s
      )

(** If we fail to allocate because VMs failed to co-operate then retry for a while before finally giving up.
    In particular this should help smooth over the period when VMs are booting and haven't loaded their balloon
    drivers yet. *)
let retry_if_not_cooperative f = 
  let start = Unix.gettimeofday () in
  let interval = 5. in
  let timeout = 30. in
  let rec loop () = 
    try
      f ()
    with Api_errors.Server_error(code, params) as e when code = Api_errors.vms_failed_to_cooperate ->
      let now = Unix.gettimeofday () in
      if now -. start > timeout then raise e else begin
	debug "Sleeping %.0f before retrying" interval;
	Thread.delay interval;
	loop ()
      end in
  loop ()

(** Reserve a particular amount of memory and return a reservation id *)
let reserve_memory ~__context ~xc ~xs ~kib = 
  let session_id = get_session_id ~xs in
  retry_if_not_cooperative
    (fun () ->
       catch_generic_errors
	 (fun () ->
	    try
	      debug "reserve_memory kib=%Ld" kib;
	      let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._kib, Int64.to_string kib ] in
	      let results = call_daemon xs Squeezed_rpc._reserve_memory args in
	      List.assoc Squeezed_rpc._reservation_id results
	    with 
	    | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_cannot_free_this_much_memory_code ->
		debug "Got error_cannot_free_this_much_memory from ballooning daemon";
		let free = Memory.get_free_memory_kib ~xc in
		raise (Api_errors.Server_error (Api_errors.host_not_enough_free_memory, [ Int64.to_string kib; Int64.to_string free ]))
	    | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_domains_refused_to_cooperate_code ->
		debug "Got error_domains_refused_to_cooperate_code from ballooning daemon";
		let domids = List.map int_of_string (Stringext.String.split ',' descr) in
		let vms = List.concat (List.map (fun domid -> 
						   try [ Vmopshelpers.vm_of_domid ~__context domid ]
						   with _ -> warn "When reporting ballooning error, failed to resolve domid %d to a VM" domid; []
						) domids) in
		raise (Api_errors.Server_error(Api_errors.vms_failed_to_cooperate, List.map Ref.string_of vms))
	 )
    )

(** Reserve a particular amount of memory and return a reservation id *)
let reserve_memory_range ~__context ~xc ~xs ~min ~max = 
  let session_id = get_session_id ~xs in
  retry_if_not_cooperative
    (fun () ->
       catch_generic_errors
	 (fun () ->
	    try
	      debug "reserve_memory_range min=%Ld max=%Ld" min max;
	      let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._min, Int64.to_string min; Squeezed_rpc._max, Int64.to_string max ] in
	      let results = call_daemon xs Squeezed_rpc._reserve_memory_range args in
	      let kib = List.assoc Squeezed_rpc._kib results
	      and reservation_id = List.assoc Squeezed_rpc._reservation_id results in
	      debug "reserve_memory_range actual = %s" kib;
	      Int64.of_string kib, reservation_id
	    with 
	    | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_cannot_free_this_much_memory_code ->
		debug "Got error_cannot_free_this_much_memory from ballooning daemon";
		let free = Memory.get_free_memory_kib ~xc in
		raise (Api_errors.Server_error (Api_errors.host_not_enough_free_memory, [ Int64.to_string min; Int64.to_string free ]))
	    | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_domains_refused_to_cooperate_code ->
		debug "Got error_domains_refused_to_cooperate_code from ballooning daemon";
		let domids = List.map int_of_string (Stringext.String.split ',' descr) in
		let vms = List.concat (List.map (fun domid -> 
						   try [ Vmopshelpers.vm_of_domid ~__context domid ]
						   with _ -> warn "When reporting ballooning error, failed to resolve domid %d to a VM" domid; []
						) domids) in
		raise (Api_errors.Server_error(Api_errors.vms_failed_to_cooperate, List.map Ref.string_of vms))
	 )
    )

(** Delete a reservation given by [reservation_id] *)
let delete_reservation ~xs ~reservation_id =
  let session_id = get_session_id ~xs in
  catch_generic_errors
    (fun () ->
       debug "delete_reservation %s" reservation_id;
       let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._reservation_id, reservation_id ] in
       ignore_results (call_daemon xs Squeezed_rpc._delete_reservation args)
    )

(** Transfer this 'reservation' to the given domain id *)
let transfer_reservation_to_domain ~xs ~reservation_id ~domid = 
  let session_id = get_session_id ~xs in
  catch_generic_errors
    (fun () ->
       try
	 debug "transfer_reservation_to_domain %s -> %d" reservation_id domid;
	 let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._reservation_id, reservation_id; Squeezed_rpc._domid, string_of_int domid ] in
	 ignore_results (call_daemon xs Squeezed_rpc._transfer_reservation_to_domain args)
       with 
       | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_unknown_reservation ->
	   debug "Got error_unknown_reservation_id (%s)" reservation_id;
	   failwith "unknown memory reservation" (* should really never happen *)
    )

(** After an event which frees memory (eg a domain destruction), perform a one-off memory rebalance *)
let balance_memory ~xc ~xs =
  catch_generic_errors
    (fun () ->
       debug "rebalance_memory";
       ignore_results (call_daemon xs Squeezed_rpc._balance_memory [])
    )
