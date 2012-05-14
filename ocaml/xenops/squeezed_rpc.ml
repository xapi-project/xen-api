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

(** Potentially generic xenstore RPC stuff *)

open Pervasiveext
open Xenops_helpers
open Xenstore

(* Service-specific: *)
let _service = "squeezed" (* prefix in xenstore of daemon *)
let _rpc = "rpc"

(* val login: service_name -> session_id *)
let _login = "login"
let _service_name = "service_name"
let _session_id = "session_id"
(* val reserve_memory: session_id -> kib -> reservation_id *)
let _reserve_memory = "reserve-memory" (* _session_id, _kib -> _reservation_id *)
let _kib = "kib"
let _reservation_id = "reservation_id"
(* val reserve_memory_range: session_id -> min -> max -> reservation_id *)
let _reserve_memory_range = "reserve-memory-range"
let _min = "min"
let _max = "max"
(* val transfer_reservation_to_domain: session_id -> reservation_id -> domid -> unit *)
let _transfer_reservation_to_domain = "transfer-reservation-to-domain" (* _session_id, _reservation_id, _domid -> unit *)
let _domid = "domid"
(* val delete_reservation: session_id -> reservation_id -> unit *)
let _delete_reservation = "delete-reservation"
(* val balance_memory: session_id -> unit *)
let _balance_memory = "balance-memory"
(* val reopen_logs: session_id -> unit *)
let _reopen_logs = "reopen-logs"

let _error_cannot_free_this_much_memory_code = "1000"
let _error_domains_refused_to_cooperate_code = "1001"
let _error_unknown_reservation               = "1002"
let _error_invalid_memory_value              = "1003" (* -ve *)
let _echo = "echo"

(* Generic: *)
let _pid = "pid"
let _request = "request"
let _response = "response"
let _code = "code"
let _description = "description"
let _error_unknown_function_code = "1"
let _error_exception_code = "2"
let _error_missing_argument_code = "3"

module D = Debug.Debugger(struct let name = "xenstore-rpc" end)
open D

let path = List.fold_left Filename.concat "/"

let listdir xs path = try List.filter (fun x -> x <> "") (xs.Xs.directory path) with Xenbus.Xb.Noent -> []
let xs_read xs path = try xs.Xs.read path with Xenbus.Xb.Noent as e -> begin debug "xenstore-read %s returned ENOENT" path; raise e end

exception Server_not_registered
exception Error of string * string
exception Missing_argument of string
exception Unknown_function of string

module Rpc_internal = struct
  type handler = (string * string) list -> (string * string) list

  let write_request xs service fn args = 
    let unique_id = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    let p = path [ service; _rpc; _request; fn; unique_id ] in
    Xs.transaction xs 
      (fun t ->
	 t.Xst.mkdir p;
	 t.Xst.writev p args
      );
    unique_id
      
  let write_response xs service fn unique_id args = 
    let p = path [ service; _rpc; _response; fn; unique_id ] in
    Xs.transaction xs
      (fun t ->
	 t.Xst.mkdir p;
	 t.Xst.writev p args
      )
  let write_error xs service fn unique_id (code, description) = 
    write_response xs service fn unique_id [ _code, code; _description, description ]
      
  let client ~xs ~service ~fn ~args = 
    (* Check that the server process is present *)
    begin
      try
	let pid = int_of_string (xs.Xs.read (path [ service; _pid ])) in
	Unix.kill pid 0;
      with _ ->
	raise Server_not_registered
    end;
    let id = write_request xs service fn args in
    ignore_string (Watch.wait_for ~xs (Watch.value_to_appear (path [ service; _rpc; _response; fn; id ])));
    let arg_keys = listdir xs (path [ service; _rpc; _response; fn; id ]) in
    let arg_vals = List.map (fun x -> xs.Xs.read (path [ service; _rpc; _response; fn; id; x ])) arg_keys in
    let args = List.combine arg_keys arg_vals in
    xs.Xs.rm (path [ service; _rpc; _request; fn; id ]);
    xs.Xs.rm (path [ service; _rpc; _response; fn; id ]);
    if List.mem_assoc _code args && (List.mem_assoc _description args) then begin
      let code = List.assoc _code args and description = List.assoc _description args in
      if code = _error_missing_argument_code
      then raise (Missing_argument description)
      else if code = _error_unknown_function_code
      then raise (Unknown_function description)
      else raise (Error(code, description));
    end else args
      
  (** Return a list of (fn_name, [ unique_id ]) corresponding to unanswered requests *)
  let list_new_requests xs service =
    let fns = listdir xs (path [ service; _rpc; _request ]) in
    let requests_of_fn x = listdir xs (path [ service; _rpc; _request; x ]) in
    let responses_of_fn x = listdir xs (path [ service; _rpc; _response; x ]) in
    let new_requests_of_fn x = Listext.List.set_difference (requests_of_fn x) (responses_of_fn x) in
    List.combine fns (List.map new_requests_of_fn fns)
      
      
  (** Read the request from xenstore and call the given function, trapping all exceptions *)
  let call_server_fn xs service fn unique_id f =
    (* Read the args *)
    try
      debug "listdir %s"  (path [ service; _rpc; _request; fn; unique_id ]);
      let arg_keys = listdir xs (path [ service; _rpc; _request; fn; unique_id ]) in
      let arg_vals = List.map (fun x -> 
				 debug "read %s" (path [ service; _rpc; _request; fn; unique_id; x ]);
				 xs.Xs.read (path [ service; _rpc; _request; fn; unique_id; x ])) arg_keys in
      let args = List.combine arg_keys arg_vals in
      f args 
    with e ->
      let e_string = Printexc.to_string e in
      debug "Function %s/%s/%s threw exception: %s" service fn unique_id e_string;
      [ _code, _error_exception_code;
	_description, e_string ]
	
  (** Process the given request, throws no exceptions *)
  let process_generic_fn xs service fn function_table unique_id = 
    debug "Request for %s/%s/%s" service fn unique_id;
    if not (List.mem_assoc fn function_table) 
    then write_error xs service fn unique_id (_error_unknown_function_code, fn)
    else 
      let f = List.assoc fn function_table in
      let results = call_server_fn xs service fn unique_id f in
      write_response xs service fn unique_id results
	
  (** Service requests forever *)
  let loop ~service ~function_table ~xc ~xs ?(idle_timeout=(-1.)) ?(idle_callback=(fun () -> ())) () = 
    (* Write our pid to the store so clients can see we are alive *)
    xs.Xs.write (path [ service; _pid ]) (string_of_int (Unix.getpid ()));

    xs.Xs.watch (path [ service; _rpc; _request ])  "X";
    
    let process_new_requests () = 
      let fns_ids = list_new_requests xs service in
      List.iter (fun (fn, ids) -> List.iter (process_generic_fn xs service fn function_table) ids) fns_ids in
    
    (* list the requests which arrived before our watch was established *)
    process_new_requests ();
    while true do
      if Xs.has_watchevents xs then begin
		(* Drain the watch event queue *)
		while Xs.has_watchevents xs do
		  ignore(Xs.get_watchevent xs)
		done
      end else begin
		(* Nothing in the queue, wait for an event on the fd *)
		let r, _, _ = Unix.select [ Xs.get_fd xs ] [] [] idle_timeout in
		if r = []
		then idle_callback ()
		else ignore(Xs.read_watchevent xs);
      end;
	  (* We think there is some work to do *)
	  process_new_requests ()
    done
end
  
module type RPC = sig
  
  type handler = (string * string) list -> (string * string) list

  val loop: service:string -> function_table:((string * handler) list) -> xc:Xenctrl.handle -> xs:Xs.xsh -> ?idle_timeout:float -> ?idle_callback:(unit -> unit) -> unit -> unit

  val client: xs:Xs.xsh -> service:string -> fn:string -> args:((string * string) list) -> (string * string) list

end

module Rpc = (Rpc_internal: RPC)
