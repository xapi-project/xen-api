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

(*
 * everything RPC related stuff happens in : /local/domain/<domid>/RPC
 *
 * typical layout:
 * <servicename>/<origin_domid>
 *    query/
 *       <idquery>/
 *           data[0-9]{2}  -- contains the actual query splitted in
 *                            512 bytes entries. (max at 99 nodes)
 *           cmd           -- the cmd executed
 *           submit        -- created with the number of packet to read once
 *                            every data has been inserted in xs.
 *    reply/
 *       <idquery>/
 *           data[0-9]{2}  -- contains the reply splitted in
 *                            512 bytes entries. (max at 99 nodes)
 *           submit        -- just like inside the query node
 *
 * a query will be cleaned by the asking side by removing the <idquery>
 * directory from the query and reply directory.
 *                            
 *)

exception Stop_listen
exception Timeout
exception Protocol_error of string

type status = Error | Success

let status_of_string s =
	match s with "success" -> Success | _ -> Error

let string_of_status s =
	match s with Success -> "success" | Error -> "error"

type t = {
	xs: Xs.xsh;
	rpcpath: string;
	service: string;
	dstid: int;
	mutable id: int;
}

(* some string utility *)
let endswith suffix x =
	let x_l = String.length x and suffix_l = String.length suffix in
	suffix_l <= x_l && String.sub x (x_l - suffix_l) suffix_l = suffix

let split_size sz s =
	let len = String.length s in
	let modsz = if len mod sz = 0 then true else false in
	let a = Array.make (len / sz + (if modsz then 0 else 1)) "" in
	for i = 0 to (len / sz)
	do
		let chunk = min sz (len - (i * sz)) in
		if chunk > 0 then
			a.(i) <- String.sub s (i * sz) chunk
	done;
	a

let rec split_string ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0 then
		[ s ]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split_string ~limit: nlimit c b)

(** open a RPC channel to @t sending queries, and t replying *)
let bind dstid service =
	(* FIXME: only works from dom0 at the moment since domU might not
	   be able to create for sure node in others domain directories.
	   need to create a small thing running in dom0 to grant access 
	   to create channel from domU to domU *)
	let xs = Xs.daemon_open () in
	let self =
		try int_of_string (xs.Xs.read "domid")
		with Xb.Invalid -> 0
		in
	let rpcpath = xs.Xs.getdomainpath dstid ^ (Printf.sprintf "/RPC/%s/%d" service self) in
	xs.Xs.mkdir rpcpath;
	xs.Xs.setperms rpcpath (self, Xsraw.PERM_RDWR, [ dstid, Xsraw.PERM_RDWR ]);
	{ xs = xs; rpcpath = rpcpath; dstid = dstid; id = 0; service = service }

let query_with_id h id cmd data =
	let len = String.length data in

	let querypath = Printf.sprintf "%s/query/%s" h.rpcpath id in
	let replypath = Printf.sprintf "%s/reply/%s" h.rpcpath id in

	let qdatapath i = Printf.sprintf "%s/data%.2d" querypath i in
	let rdatapath i = Printf.sprintf "%s/data%.2d" replypath i in

	let a = Array.mapi (fun i x -> qdatapath i, x) (split_size 512 data) in
	Array.iter (fun (path, value) -> h.xs.Xs.write path value) a;
	h.xs.Xs.write (querypath ^ "/cmd") cmd;
	h.xs.Xs.write (querypath ^ "/submit") (string_of_int (Array.length a));

	(* now wait for a reply. wrap the xs.timeout in xsrpc.timeout *)
	let callback (path, _) =
		if endswith "/submit" path then
			try ignore (int_of_string (h.xs.Xs.read path)); true with _ -> false
		else
			false
		in
	begin
		try Xs.monitor_paths h.xs [ replypath, "rpc-r" ] 120. callback
		with Xs.Timeout -> raise Timeout
	end;

	let nbnodes = int_of_string (h.xs.Xs.read (Printf.sprintf "%s/submit" replypath)) in
	let status = status_of_string (try h.xs.Xs.read (replypath ^ "/status") with _ -> "") in
	let buf = Buffer.create (nbnodes * 512) in
	for i = 0 to nbnodes - 1
	do
		let data =
			try h.xs.Xs.read (rdatapath i)
			with _ -> raise (Protocol_error "datapath missing")
			in
		Buffer.add_string buf data
	done;
	(* after the query, we try to clean up the tree by removing the query/<id>
	 * and reply/<id> directories *)
	begin try h.xs.Xs.rm querypath with _ -> (); end;
	begin try h.xs.Xs.rm replypath with _ -> (); end;
	(status, Buffer.contents buf)

let query h cmd data =
	(* send the query *)
	let id = h.id in
	h.id <- h.id + 1;
	query_with_id h (string_of_int id) cmd data

(** until f raise a Stop_listening, carry on with listening new commands *)
let listen service f =
	let xs = Xs.daemon_open () in
	let self =
		try int_of_string (xs.Xs.read "domid")
		with Xb.Invalid -> 0
		in
	let rpcpath = xs.Xs.getdomainpath self ^ "/RPC/" ^ service in
	let callback (path, _) =
		let l = split_string '/' path in
		begin match l with
		| "" :: "local" :: "domain" :: domid :: "RPC" :: service :: sdomid :: "query" :: id :: [ "submit" ] -> (
			let sdomid = int_of_string sdomid in
			let nbdata = int_of_string (xs.Xs.read path) in

			let querypath = Printf.sprintf "/local/domain/%s/RPC/%s/%d/query/%s"
			                               domid service sdomid id in
			let replypath = Printf.sprintf "/local/domain/%s/RPC/%s/%d/reply/%s"
			                               domid service sdomid id in
			let cmd = try xs.Xs.read (querypath ^ "/cmd") with _ -> "" in
			let qdatapath i = Printf.sprintf "%s/data%.2d" querypath i in
			let rdatapath i = Printf.sprintf "%s/data%.2d" replypath i in

			(* read query *)
			let buf = Buffer.create 1024 in
			for i = 0 to nbdata - 1
			do
				let s = xs.Xs.read (qdatapath i) in
				Buffer.add_string buf s
			done;
			let data = Buffer.contents buf in

			(* start callback with all parameters *)
			match f sdomid id cmd data with
			| None -> ()
			| Some (status, data) ->
				let a = Array.mapi (fun i x -> rdatapath i, x) (split_size 512 data) in
				Array.iter (fun (path, value) -> xs.Xs.write path value) a;
				xs.Xs.write (replypath ^ "/status") (string_of_status status);
				xs.Xs.write (replypath ^ "/submit") (string_of_int (Array.length a));
			)
		| _ ->
			()
		end;
		false
		in
	let quit = ref false in
	while not !quit
	do
		try
			Xs.monitor_paths xs [ rpcpath, "rpc" ] 3600. callback
		with
		| Xs.Timeout -> ()
		| Stop_listen -> quit := true
	done;
	()
