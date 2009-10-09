(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module D = Debug.Debugger(struct let name="memory_control" end)
open D

open Pervasiveext
open Threadext

let throw_exceptions f ~__context = 
  try
    f ()
  with 
  | Squeezed_rpc.Server_not_registered ->
      error "The ballooning daemon is not running";
      failwith "The ballooning daemon is not running" (* should never happen but just in case *)
  | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_invalid_memory_value ->
      debug "Got error_invalid_memory_value from ballooning daemon";
      failwith "error_invalid_memory_value" (* should never happen *)
  | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_cannot_free_this_much_memory_code ->
      debug "Got error_cannot_free_this_much_memory(%s) from ballooning daemon" descr;
      begin match Stringext.String.split ',' descr with
      | [ needed; free ] ->
	  let needed = Memory.bytes_of_kib (Int64.of_string needed) in
	  let free = Memory.bytes_of_kib (Int64.of_string free) in
	  raise (Api_errors.Server_error (Api_errors.host_not_enough_free_memory, [ Int64.to_string needed; Int64.to_string free ]))
      | _ -> failwith "Failed to parse cannot_free_this_much_memory_error (old ballooning daemon?)"
      end
  | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_domains_refused_to_cooperate_code ->
      debug "Got error_domains_refused_to_cooperate_code from ballooning daemon";
      let domids = List.map int_of_string (Stringext.String.split ',' descr) in
      let vms = List.concat (List.map (fun domid -> 
					 try [ Vmopshelpers.vm_of_domid ~__context domid ]
					 with _ -> warn "When reporting ballooning error, failed to resolve domid %d to a VM" domid; []
				      ) domids) in
      raise (Api_errors.Server_error(Api_errors.vms_failed_to_cooperate, List.map Ref.string_of vms))
  | Squeezed_rpc.Error(code, descr) when code = Squeezed_rpc._error_unknown_reservation ->
      debug "Got error_unknown_reservation_id (%s)" descr;
      failwith "unknown memory reservation" (* should really never happen *)
  | Squeezed_rpc.Error(code, descr) ->
      error "Got unexpected (%s, %s) from ballooning daemon" code descr;
      failwith (Printf.sprintf "The ballooning daemon failed: %s, %s" code descr) (* should never happen *)
  | e ->
      error "Unexpected exception talking to ballooning daemon: %s" (ExnHelper.string_of_exn e);
      raise e

let call_daemon xs fn args = Squeezed_rpc.Rpc.client ~xs ~service:Squeezed_rpc._service ~fn ~args
let ignore_results (_: (string * string) list) = ()

(** Log into the ballooning service (on every xapi start) *)
let login ~__context ~xs = 
  throw_exceptions ~__context
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
  fun ~__context ~xs ->
    Mutex.execute m
      (fun () ->
	 match !session_id with
	 | Some x -> x
	 | None ->
	     let s = login ~__context ~xs in
	     session_id := Some s;
	     s
      )

(** If we fail to allocate because VMs either failed to co-operate or because they are still booting
    and haven't written their feature-balloon flag then retry for a while before finally giving up.
    In particular this should help smooth over the period when VMs are booting and haven't loaded their balloon
    drivers yet. *)
let retry f = 
  let start = Unix.gettimeofday () in
  let interval = 10. in
  let timeout = 60. in
  let rec loop () = 
    try
      f ()
    with Api_errors.Server_error(code, params) as e 
      when code = Api_errors.vms_failed_to_cooperate 
	|| code = Api_errors.host_not_enough_free_memory ->
      let now = Unix.gettimeofday () in
      if now -. start > timeout then raise e else begin
	debug "Sleeping %.0f before retrying" interval;
	Thread.delay interval;
	loop ()
      end in
  loop ()

(** Reserve a particular amount of memory and return a reservation id *)
let reserve_memory ~__context ~xc ~xs ~kib = 
  let session_id = get_session_id ~__context ~xs in
  retry
    (fun () ->
       throw_exceptions ~__context
	 (fun () ->
	    debug "reserve_memory kib=%Ld" kib;
	    let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._kib, Int64.to_string kib ] in
	    let results = call_daemon xs Squeezed_rpc._reserve_memory args in
	    List.assoc Squeezed_rpc._reservation_id results
	 )
    )

(** Reserve a particular amount of memory and return a reservation id *)
let reserve_memory_range ~__context ~xc ~xs ~min ~max = 
  let session_id = get_session_id ~__context ~xs in
  let reserved_memory, reservation_id =
  retry
    (fun () ->
       throw_exceptions ~__context
	 (fun () ->
	    debug "reserve_memory_range min=%Ld max=%Ld" min max;
	    let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._min, Int64.to_string min; Squeezed_rpc._max, Int64.to_string max ] in
	    let results = call_daemon xs Squeezed_rpc._reserve_memory_range args in
	    let kib = List.assoc Squeezed_rpc._kib results
	    and reservation_id = List.assoc Squeezed_rpc._reservation_id results in
	    debug "reserve_memory_range actual = %s" kib;
	    Int64.of_string kib, reservation_id
	 )
    ) in
  debug "reserved_memory = %Ld; min = %Ld; max = %Ld" reserved_memory min max;
	(* Post condition: *)
	assert (reserved_memory >= min);
	assert (reserved_memory <= max);

	reserved_memory, reservation_id

(** Delete a reservation given by [reservation_id] *)
let delete_reservation ~__context ~xs ~reservation_id =
  let session_id = get_session_id ~__context ~xs in
  throw_exceptions ~__context
    (fun () ->
       debug "delete_reservation %s" reservation_id;
       let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._reservation_id, reservation_id ] in
       ignore_results (call_daemon xs Squeezed_rpc._delete_reservation args)
    )

(** Transfer this 'reservation' to the given domain id *)
let transfer_reservation_to_domain ~__context ~xs ~reservation_id ~domid = 
  let session_id = get_session_id ~__context ~xs in
  throw_exceptions ~__context
    (fun () ->
	 debug "transfer_reservation_to_domain %s -> %d" reservation_id domid;
	 let args = [ Squeezed_rpc._session_id, session_id; Squeezed_rpc._reservation_id, reservation_id; Squeezed_rpc._domid, string_of_int domid ] in
	 ignore_results (call_daemon xs Squeezed_rpc._transfer_reservation_to_domain args)
    )

(** After an event which frees memory (eg a domain destruction), perform a one-off memory rebalance *)
let balance_memory ~__context ~xc ~xs =
  throw_exceptions ~__context
    (fun () ->
       debug "rebalance_memory";
       ignore_results (call_daemon xs Squeezed_rpc._balance_memory [])
    )

(** Arrange to have at least one more memory rebalance happen in the background. *)
let async_balance_memory =
  At_least_once_more.make "balance_memory" 
    (fun () ->
       Server_helpers.exec_with_new_task "balance_memory"
	 (fun __context ->
	    Vmopshelpers.with_xc_and_xs
	      (fun xc xs ->
		 balance_memory ~__context ~xc ~xs
	      )
	 )
    )
