(*
 * Copyright (C) Citrix Systems Inc.
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

open Xenops_interface
open Xenops_utils
open Xenops_client
open Fun

module D = Debug.Debugger(struct let name = service_name end)
open D

(*
let local_rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix "/var/lib/xcp/xenopsd") ~http:(xmlrpc ~version:"1.0" "/") call

let _metadata = "VM.import_metadata"
let _failure = "VM.client_migrate_failed"
let _complete = "VM.migrate_complete"

module Receiver = struct
	type created_object =
		| Vm_metadata of Vm.id
		| Vm_created of Vm.id

	type state =
		| Waiting_metadata
		| Received_metadata of Vm.id
		| Completed
		| Error of string
	with rpc

	type t = state * created_object list

	let cleanup _ = ()
(*
		= function
		| Vm_metadata id ->
			debug "Removing VM metadata for VM id %s" id;
			Client.VM.remove local_rpc id |> unwrap
		| Vm_created id ->
			debug "Destroying VM id %s" id;
			Client.VM.destroy local_rpc id |> success |> wait_for_task local_rpc |> success_task local_rpc |> ignore_task
*)
	let initial = Waiting_metadata, []
(*
	let next (state, created_objects) call = match state, call.Rpc.name, call.Rpc.params with
		| Waiting_metadata, call, [ Rpc.String md ] when call = _metadata ->
			let vm = md |> Client.VM.import_metadata |> unwrap in
			let created_objects = Vm_metadata vm :: created_objects in
(*
			Client.VM.create local_rpc vm |> success |> wait_for_task local_rpc |> success_task local_rpc |> ignore_task;
			let created_objects = Vm_created vm :: created_objects in
*)
			Received_metadata vm, created_objects
		| state, name, _ ->
			List.iter cleanup created_objects;
			Error (Printf.sprintf "Unexpected call. State = %s; Call = %s" (state |> rpc_of_state |> Jsonrpc.to_string) name), []
*)
	let string_of_state x = x |> fst |> rpc_of_state |> Jsonrpc.to_string
end

type sender_state = unit

(*
let rec receiver_loop req s state =
	let next_req, next_state = try
		let call = Unixext.really_read_string s (req.Http.Request.content_length |> Opt.unbox |> Int64.to_int) |> Jsonrpc.call_of_string in
		let next_state = Receiver.next state call in
		debug "previous state = %s; next state = %s" (Receiver.string_of_state state) (Receiver.string_of_state next_state);
		let response = Rpc.success (next_state |> fst |> Receiver.rpc_of_state) in
		let body = response |> Jsonrpc.string_of_response in
		let length = body |> String.length |> Int64.of_int in
		let response = Http.Response.make ~version:"1.1" ~length ~body "200" "OK" in
		response |> Http.Response.to_wire_string |> Unixext.really_write_string s;
		(* We need to unmarshal the next HTTP request ourselves. *)
		match Http_svr.request_of_bio (Buf_io.of_fd s) with
			| None ->
				debug "Failed to parse HTTP request";
				failwith "Failed to parse HTTP request"
			| Some req -> req, next_state
	with e ->
		debug "Receiver thread caught: %s" (Printexc.to_string e);
		raise e in
	receiver_loop next_req s next_state

let receive req s _ =
	let _, _ = receiver_loop req s Receiver.initial in
	()
*)

let http_post url length body =
	Http.Request.make ~version:"1.1" ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" ~length ~query:(Http.Url.get_query_params url) ~body Http.Post (Http.Url.get_uri url)
*)
let http_put ?(cookie=[]) url =
	Http.Request.make ~version:"1.1" ~keep_alive:false ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" ~query:(Http.Url.get_query_params url) ~cookie Http.Put (Http.Url.get_uri url)
(*
let remote_rpc url rpc fd =
	let body = rpc |> Jsonrpc.string_of_call in
	let length = body |> String.length |> Int64.of_int in
	Http_client.rpc fd (http_post url length body)
		(fun response _ ->
			let body = Unixext.really_read_string fd (response.Http.Response.content_length |> Opt.unbox |> Int64.to_int) in
		debug "body = [%s]" body;
		body |> Jsonrpc.response_of_string |> (fun x -> x.Rpc.contents) |> Receiver.state_of_rpc
		)

let send_metadata url metadata fd =
	let open Receiver in
	match remote_rpc url (Rpc.call _metadata [ Rpc.String metadata ]) fd with
		| Received_metadata id -> id
		| x -> failwith (Printf.sprintf "Unexpected response: %s" (x |> rpc_of_state |> Jsonrpc.to_string))

let send_failure url fd =
	let _ = remote_rpc url (Rpc.call _failure []) fd in
	()

let send_complete url i fd =
	let open Receiver in
	match remote_rpc url (Rpc.call _complete [ ]) fd with
		| Completed -> ()
		| x -> failwith (Printf.sprintf "Unexpected response: %s" (x |> rpc_of_state |> Jsonrpc.to_string))


let serve_rpc s f =
	let sent_response = ref false in
	try
		match Http_svr.request_of_bio (Buf_io.of_fd s) with
			| None ->
				debug "Failed to parse HTTP request";
				failwith "Failed to parse HTTP request"
			| Some req ->
				let body = match req.Http.Request.content_length with
					| Some x -> Unixext.really_read_string s (Int64.to_int x)
					| None -> Unixext.string_of_fd s in
				let body = f body in
				let length = body |> String.length |> Int64.of_int in
				let response = Http.Response.make ~version:"1.1" ~length ~body "200" "OK" in
				sent_response := true;
				response |> Http.Response.to_wire_string |> Unixext.really_write_string s
	with e ->
		debug "%s" (Printexc.to_string e);
		debug "%s" (Printexc.get_backtrace ());
		if not !sent_response then begin
			let body = String.concat "\n" [
				"The following information may be useful for debugging:";
				"";
				Printf.sprintf "Exception: %s" (Printexc.to_string e);
				"";
				"Stacktrace:";
				(Printexc.get_backtrace ())
			] in
			let length = body |> String.length |> Int64.of_int in
			let response = Http.Response.make ~version:"1.1" ~length ~body "500" "Not so good" in
			response |> Http.Response.to_wire_string |> Unixext.really_write_string s
		end
*)

exception Remote_failed of string

(** Functions to synchronise between the sender and receiver via binary messages of the form:
    00 00 -- success
    11 22 <0x1122 bytes of data> -- failure, with error message
    Used rather than the API for signalling between sender and receiver to avoid having to
    go through the master and interact with locking. *)
module Handshake = struct
	type result =
		| Success
		| Error of string

	let string_of_result = function
		| Success -> "Success"
		| Error x -> "Error: " ^ x

	(** Receive a 'result' from the remote *)
	let recv ?verbose:(verbose=false) (s: Unix.file_descr) : result =
		let buf = String.make 2 '\000' in
		if verbose then debug "Handshake.recv: about to read result code from remote.";
		(try
			Unixext.really_read s buf 0 (String.length buf)
		with _ ->
			raise (Remote_failed "unmarshalling result code from remote"));
		if verbose then debug "Handshake.recv: finished reading result code from remote.";
		let len = int_of_char buf.[0] lsl 8 lor (int_of_char buf.[1]) in
		if len = 0
		then Success
		else begin
			let msg = String.make len '\000' in
			if verbose then debug "Handshake.recv: about to read error message from remote.";
			(try Unixext.really_read s msg 0 len
				with _ ->
					raise (Remote_failed "unmarshalling error message from remote"));
			if verbose then debug "Handshake.recv: finished reading error message from remote.";
			Error msg
		end

	(** Expects to receive a success code from the server, throws an exception otherwise *)
	let recv_success ?verbose (s: Unix.file_descr) : unit = match recv ?verbose s with
		| Success -> ()
		| Error x -> raise (Remote_failed ("error from remote: " ^ x))

	(** Transmit a 'result' to the remote *)
	let send ?verbose:(verbose=false) (s: Unix.file_descr) (r: result) =
		let len = match r with
			| Success -> 0
			| Error msg -> String.length msg in
		let buf = String.make (2 + len) '\000' in
		buf.[0] <- char_of_int ((len lsr 8) land 0xff);
		buf.[1] <- char_of_int ((len lsr 0) land 0xff);
		(match r with
			| Success -> ()
			| Error msg -> String.blit msg 0 buf 2 len);
		if verbose then debug "Handshake.send: about to write result to remote.";
		if Unix.write s buf 0 (len + 2) <> len + 2
		then raise (Remote_failed "writing result to remote");
		if verbose then debug "Handshake.send: finished writing result to remote.";
end

