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

open Xs_protocol
open Junk

let ( |> ) a b = b a

let debug fmt = Logging.debug "call" fmt
let error fmt = Logging.error "call" fmt

exception Parse_failure

exception Transaction_again

exception Transaction_nested

let get_namespace_implementation path = match Store.Path.to_string_list path with
	| "tool" :: "xenstored" :: "quota" :: rest ->
		Store.Path.of_string_list rest, (module Quota_interface: Namespace.IO)
	| "tool" :: "xenstored" :: "connection" :: rest ->
		Store.Path.of_string_list rest, (module Connection.Interface: Namespace.IO)
	| "tool" :: "xenstored" :: "log" :: rest ->
		Store.Path.of_string_list rest, (module Logging_interface: Namespace.IO)
	| "tool" :: "xenstored" :: "memory" :: rest ->
		Store.Path.of_string_list rest, (module Heap_debug_interface: Namespace.IO)
	| _ ->
		path, (module Transaction: Namespace.IO)

(* Perform a 'simple' operation (not a Transaction_start or Transaction_end)
   and create a response. *)
let op_exn _store c t (payload: Request.payload) : Response.payload =
	let connection_path = c.Connection.domainpath in
	let resolve data = Store.Path.create data connection_path in

	let open Request in
	match payload with
		| Transaction_start
		| Transaction_end _
		| Watch(_, _)
		| Unwatch(_, _)
		| Debug _
		| Introduce(_, _, _)
		| Resume(_)
		| Release(_)
		| Set_target(_, _)
		| Restrict _
		| Isintroduced _
		| Error _
		| Watchevent _ -> assert false
		| Getdomainpath domid ->
			let v = Store.Path.getdomainpath domid |> Store.Path.to_string in
			Response.Getdomainpath v
		| PathOp(path, op) ->
			let path = resolve path in

			let path, m = get_namespace_implementation path in
			let module Impl = (val m: Namespace.IO) in

			let mkdir_p t creator perm path =
				let dirname = Store.Path.get_parent path in
				if not (Impl.exists t perm dirname) then (
					let rec check_path p =
						match p with
						| []      -> []
						| h :: l  ->
							if Impl.exists t perm h then
								check_path l
							else
								p in
					let ret = check_path (List.tl (Store.Path.get_hierarchy dirname)) in
					List.iter (fun s -> Impl.mkdir ~with_watch:false t creator perm s) ret
				) in
			begin match op with
			| Read ->
				let v = Impl.read t c.Connection.perm path in
				Response.Read v
			| Directory ->
				let entries = Impl.list t c.Connection.perm path in
				Response.Directory entries
			| Getperms ->
				let v = Impl.getperms t c.Connection.perm path in
				Response.Getperms v
			| Write value ->
				mkdir_p t c.Connection.domid c.Connection.perm path;
				Impl.write t c.Connection.domid c.Connection.perm path value;
				Response.Write
			| Mkdir ->
				mkdir_p t c.Connection.domid c.Connection.perm path;
				Impl.mkdir t c.Connection.domid c.Connection.perm path;
				Response.Mkdir
			| Rm ->
				Impl.rm t c.Connection.perm path;
				Response.Rm
			| Setperms perms ->
				Impl.setperms t c.Connection.perm path perms;
				Response.Setperms
			end

(* Replay a stored transaction against a fresh store, check the responses are
   all equivalent: if so, commit the transaction. Otherwise send the abort to
   the client. *)
let transaction_replay store c t =
	let ops = Transaction.get_operations t in
	let tid = Connection.register_transaction c store in
	let t = Transaction.make tid store in
	let con = "replay request:" ^ c.Connection.domstr in
	let perform_exn (request, response) =
		Logging.request ~tid ~con:("replay request:" ^ c.Connection.domstr) request;
		Logging.response ~tid ~con:("replay reply1: " ^ c.Connection.domstr) response;
		let response' = op_exn store c t request in
		Logging.response ~tid ~con:("replay reply2: " ^ c.Connection.domstr) response';
		Logging.response ~tid ~con response';
		if response <> response' then begin
			raise Transaction_again
		end in
	try
		Logging.start_transaction ~con ~tid;
		List.iter perform_exn ops;
		Logging.end_transaction ~tid ~con;

		Transaction.commit ~con t
	with e ->
		error "transaction_replay caught: %s" (Printexc.to_string e);
		false

let reply_exn store c (request: t) : Response.payload =
	let tid = get_tid request in
	let t =
		if tid = Transaction.none
		then Transaction.make tid store
		else Connection.get_transaction c tid in
	let payload : Xs_protocol.Request.payload = match Xs_protocol.Request.parse (request: t) with
		| None ->
 			error "Failed to parse request: got %s" (hexify (Bytes.to_string @@ Xs_protocol.to_bytes request));
			raise Parse_failure
		| Some x -> x in

	Logging.request ~tid ~con:c.Connection.domstr payload;

	let response_payload = match payload with
		| Request.Transaction_start ->
			if tid <> Transaction.none then raise Transaction_nested;
			let tid = Connection.register_transaction c store in
			Response.Transaction_start tid
		| Request.Transaction_end commit ->
			Connection.unregister_transaction c tid;
			if commit then begin
				Logging.end_transaction ~tid ~con:c.Connection.domstr;
				if true
					&& not(Transaction.commit ~con:c.Connection.domstr t)
					&& not(transaction_replay store c t)
				then raise Transaction_again;
				Transaction.get_paths t |> List.rev |> List.iter Connection.fire;
				Response.Transaction_end
			end else begin
				(* Don't log an explicit abort *)
				Response.Transaction_end
			end
		| Request.Watch(path, token) ->
			let watch = Connection.add_watch c (Store.Name.of_string path) token in
			Connection.fire_one None watch;
			Response.Watch
		| Request.Unwatch(path, token) ->
			Connection.del_watch c (Store.Name.of_string path) token;
			Response.Unwatch
		| Request.Debug cmd ->
			Perms.has c.Connection.perm Perms.DEBUG;
			Response.Debug (
				try match cmd with
				| "print" :: msg :: _ ->
					Logging.debug_print ~tid:0l ~con:c.Connection.domstr msg;
					[]
				| _ -> []
				with _ -> [])
		| Request.Introduce(domid, mfn, remote_port) ->
			Perms.has c.Connection.perm Perms.INTRODUCE;
			Introduce.(introduce { domid = domid; mfn = mfn; remote_port = remote_port });
			Connection.fire (Xs_protocol.Op.Write, Store.Name.introduceDomain);
			Response.Introduce
		| Request.Resume _ ->
			Perms.has c.Connection.perm Perms.RESUME;
			(* register domain *)
			Response.Resume
		| Request.Release _ ->
			Perms.has c.Connection.perm Perms.RELEASE;
			(* unregister domain *)
			Connection.fire (Xs_protocol.Op.Write, Store.Name.releaseDomain);
			Response.Release
		| Request.Set_target(mine, yours) ->
			Perms.has c.Connection.perm Perms.SET_TARGET;
			Hashtbl.iter
				(fun address c ->
					if Xs_protocol.domain_of_address address = mine
					then c.Connection.perm <- Perms.set_target c.Connection.perm yours;
				) Connection.by_address;
			Response.Set_target
		| Request.Restrict domid ->
			Perms.has c.Connection.perm Perms.RESTRICT;
			c.Connection.perm <- Perms.restrict c.Connection.perm domid;
			Response.Restrict
		| Request.Isintroduced _ ->
			Perms.has c.Connection.perm Perms.ISINTRODUCED;
			Response.Isintroduced false
		| Request.Error msg ->
			error "client sent us an error: %s" (hexify msg);
			raise Parse_failure
		| Request.Watchevent msg ->
			error "client sent us a watch event: %s" (hexify msg);
			raise Parse_failure
		| op ->
			let reply = op_exn store c t op in
			if tid <> Transaction.none then Transaction.add_operation t op reply;
			reply in

	if tid = Transaction.none
	then Transaction.get_paths t |> List.rev |> List.iter Connection.fire;
	response_payload

let gc store =
	if Symbol.created () > 1000 || Symbol.used () > 20000
	then begin
		debug "Started symbol GC";
		Symbol.mark_all_as_unused ();
		Store.mark_symbols store;
		Hashtbl.iter (fun _ c -> Connection.mark_symbols c) Connection.by_address;
		Symbol.garbage ()
	end

let reply store c request =
	gc store;
	c.Connection.stat_nb_ops <- c.Connection.stat_nb_ops + 1;
	let tid = get_tid request in
	let rid = get_rid request in
	let response_payload, info =
		try
			reply_exn store c request, None
		with e ->
			let default = Some (Printexc.to_string e) in
			let reply code = Response.Error code in
			begin match e with
				| Store.Already_exists p           -> reply "EEXIST", Some p
				| Store.Path.Doesnt_exist p        -> reply "ENOENT", Some p
				| Perms.Permission_denied          -> reply "EACCES", default
				| Not_found                        -> reply "ENOENT", default
				| Parse_failure                    -> reply "EINVAL", default
				| Invalid_argument i               -> reply "EINVAL", Some i
				| Transaction_again                -> reply "EAGAIN", default
				| Transaction_nested               -> reply "EBUSY",  default
				| Quota.Limit_reached              -> reply "EQUOTA", default
				| Quota.Data_too_big               -> reply "E2BIG",  default
				| Quota.Transaction_opened         -> reply "EQUOTA", default
				| Failure _												 -> reply "EINVAL", default
				| Namespace.Unsupported            -> reply "ENOTSUP",default
				| _                                -> reply "EIO",    default
			end in
	Logging.response ~tid ~con:c.Connection.domstr ?info response_payload;

	Response.print response_payload tid rid


