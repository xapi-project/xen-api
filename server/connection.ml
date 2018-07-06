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

let info  fmt = Logging.info  "connection" fmt
let error fmt = Logging.debug "connection" fmt

exception End_of_file

type watch = {
	con: t;
	token: string;
	name: Store.Name.t;
	mutable count: int;
}

and t = {
	address: Xs_protocol.address;
	interface: (module Namespace.IO) option;
	domid: int;
	domstr: string;
	idx: int; (* unique counter *)
	transactions: (int32, Transaction.t) Hashtbl.t;
	mutable next_tid: int32;
	watches: (Store.Name.t, watch list) Hashtbl.t;
	mutable nb_watches: int;
	mutable nb_dropped_watches: int;
	mutable stat_nb_ops: int;
	mutable perm: Perms.t;
	watch_events: (string * string) Queue.t;
	cvar: unit Lwt_condition.t;
	domainpath: Store.Path.t;
}

let by_address : (Xs_protocol.address, t) Hashtbl.t = Hashtbl.create 128
let by_index   : (int,               t) Hashtbl.t = Hashtbl.create 128

let watches : (string, watch list) Trie.t ref = ref (Trie.create ())

let list_of_watches () =
	Trie.fold (fun path v_opt acc ->
		match v_opt with
		| None -> Printf.sprintf "%s <- None" path :: acc
		| Some vs -> Printf.sprintf "%s <- %s" path (String.concat ", " (List.map (fun v -> v.con.domstr) vs)) :: acc
	) !watches []

let watch_create ~con ~name ~token = { 
	con = con; 
	token = token; 
	name = name;
	count = 0;
}

let get_con w = w.con
 
let number_of_transactions con =
	Hashtbl.length con.transactions

let anon_id_next = ref 1

let destroy address =
	try
		let c = Hashtbl.find by_address address in
		Logging.end_connection ~tid:Transaction.none ~con:c.domstr;
		watches := Trie.map
			(fun watches ->
				match List.filter (fun w -> w.con != c) watches with
				| [] -> None
				| ws -> Some ws
			) !watches;
		Hashtbl.remove by_address address;
		Hashtbl.remove by_index c.idx;
	with Not_found ->
		error "Failed to remove connection for: %s" (Xs_protocol.string_of_address address)

let counter = ref 0

let create address interface =
	if Hashtbl.mem by_address address then begin
		info "Connection.create: found existing connection for %s: closing" (Xs_protocol.string_of_address address);
		destroy address
	end;
	let dom = Xs_protocol.domain_of_address address in
	let con = 
	{
		address = address;
		interface = interface;
		domid = dom;
		idx = !counter;
		domstr = Xs_protocol.string_of_address address;
		transactions = Hashtbl.create 5;
		next_tid = 1l;
		watches = Hashtbl.create 8;
		nb_watches = 0;
		nb_dropped_watches = 0;
		stat_nb_ops = 0;
		perm = Perms.of_domain dom;
		watch_events = Queue.create ();
		cvar = Lwt_condition.create ();
		domainpath = Store.Path.getdomainpath dom;
	}
	in
	incr counter;
	Logging.new_connection ~tid:Transaction.none ~con:con.domstr;
	Hashtbl.replace by_address address con;
	Hashtbl.replace by_index con.idx con;
	con

let restrict con domid =
	con.perm <- Perms.restrict con.perm domid

let get_watches (con: t) name =
	if Hashtbl.mem con.watches name
	then Hashtbl.find con.watches name
	else []

let add_watch con name token =
	if con.nb_watches >= (Quota.maxwatch_of_domain con.domid)
	then raise Quota.Limit_reached;

	let l = get_watches con name in
	if List.exists (fun w -> w.token = token) l
	then raise (Store.Already_exists (Printf.sprintf "%s:%s" (Store.Name.to_string name) token));
	let watch = watch_create ~con ~token ~name in
	Hashtbl.replace con.watches name (watch :: l);
	con.nb_watches <- con.nb_watches + 1;

	watches :=
		(let key = Store.Name.(to_key (make_absolute name (Store.Path.to_string con.domainpath))) in
		let ws =
            if Trie.mem !watches key
            then Trie.find !watches key
            else []
        in
        Trie.set !watches key (watch :: ws));
	watch

let del_watch con name token =
	let ws = Hashtbl.find con.watches name in
	let w = List.find (fun w -> w.token = token) ws in
	let filtered = List.filter (fun e -> e != w) ws in
	if List.length filtered > 0 then
		Hashtbl.replace con.watches name filtered
	else
		Hashtbl.remove con.watches name;
	con.nb_watches <- con.nb_watches - 1;

	watches :=
		(let key = Store.Name.(to_key (make_absolute name (Store.Path.to_string con.domainpath))) in
		let ws = List.filter (fun x -> x != w) (Trie.find !watches key) in
        if ws = [] then
                Trie.unset !watches key
        else
                Trie.set !watches key ws)


let fire_one name watch =
	let name = match name with
		| None ->
			(* If no specific path was modified then we fire the generic watch *)
			watch.name
		| Some name ->
			(* If the watch was registered as a relative path, then we make
			   all the watch events relative too *)
			if Store.Name.is_relative watch.name
			then Store.Path.make_relative watch.con.domainpath name
			else name in
	let name = Store.Name.to_string name in
	let open Xs_protocol in
	Logging.response ~tid:0l ~con:watch.con.domstr (Response.Watchevent(name, watch.token));
	watch.count <- watch.count + 1;
	if Queue.length watch.con.watch_events >= (Quota.maxwatchevent_of_domain watch.con.domid) then begin
		error "domid %d reached watch event quota (%d >= %d): dropping watch %s:%s" watch.con.domid (Queue.length watch.con.watch_events) (Quota.maxwatchevent_of_domain watch.con.domid) name watch.token;
		watch.con.nb_dropped_watches <- watch.con.nb_dropped_watches + 1
	end else begin
		Queue.add (name, watch.token) watch.con.watch_events;
		Lwt_condition.signal watch.con.cvar ()
	end

let fire (op, name) =
	let key = Store.Name.to_key name in
	Trie.iter_path
		(fun _ w -> match w with
		| None -> ()
		| Some ws -> List.iter (fire_one (Some name)) ws
		) !watches key;
	
	if op = Xs_protocol.Op.Rm
	then Trie.iter
		(fun _ w -> match w with
		| None -> ()
		| Some ws -> List.iter (fire_one None) ws
		) (Trie.sub !watches key)

let find_next_tid con =
	let ret = con.next_tid in con.next_tid <- Int32.add con.next_tid 1l; ret

let register_transaction con store =
	if Hashtbl.length con.transactions >= (Quota.maxtransaction_of_domain con.domid)
	then raise Quota.Limit_reached;	

	let id = find_next_tid con in
	let ntrans = Transaction.make id store in
	Hashtbl.add con.transactions id ntrans;
	Logging.start_transaction ~tid:id ~con:con.domstr;
	id

let unregister_transaction con tid =
	Hashtbl.remove con.transactions tid

let get_transaction con tid =
	try
		Hashtbl.find con.transactions tid
	with Not_found as e ->
		error "Failed to find transaction %lu on %s" tid con.domstr;
		raise e

let mark_symbols con =
	Hashtbl.iter (fun _ t -> Store.mark_symbols (Transaction.get_store t)) con.transactions

let stats con =
	Hashtbl.length con.watches, con.stat_nb_ops

let debug con =
	let list_watches con =
		let ll = Hashtbl.fold 
			(fun _ watches acc -> List.map (fun watch -> watch.name, watch.token) watches :: acc)
			con.watches [] in
		List.concat ll in

	let watches = List.map (fun (name, token) -> Printf.sprintf "watch %s: %s %s\n" con.domstr (Store.Name.to_string name) token) (list_watches con) in
	String.concat "" watches

module Interface = struct
	include Namespace.Unsupported

	let read_connection t perms path c = function
		| [] ->
			""
		| "address" :: [] ->
			Xs_protocol.string_of_address c.address
		| "current-transactions" :: [] ->
			string_of_int (Hashtbl.length c.transactions)
		| "total-operations" :: [] ->
			string_of_int c.stat_nb_ops
		| "current-watch-queue-length" :: [] ->
			string_of_int (Queue.length c.watch_events)
		| "total-dropped-watches" :: [] ->
			string_of_int c.nb_dropped_watches
		| "watch" :: [] ->
			""
		| "watch" :: n :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then Store.Path.doesnt_exist path;
			""
		| "watch" :: n :: "name" :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then Store.Path.doesnt_exist path;
			Store.Name.to_string (List.nth all n).name
		| "watch" :: n :: "token" :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then Store.Path.doesnt_exist path;
			(List.nth all n).token
		| "watch" :: n :: "total-events" :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then Store.Path.doesnt_exist path;
			string_of_int (List.nth all n).count
		| "backend" :: rest ->
			begin match c.interface with
			| None -> Store.Path.doesnt_exist path
			| Some i ->
				let module I = (val i: Namespace.IO) in
				I.read t perms (Store.Path.of_string_list rest)
			end
		| _ -> Store.Path.doesnt_exist path

	let read t (perms: Perms.t) (path: Store.Path.t) =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> ""
		| "socket" :: [] -> ""
		| "socket" :: idx :: rest ->
			let idx = int_of_string idx in
			if not(Hashtbl.mem by_index idx) then Store.Path.doesnt_exist path;
			let c = Hashtbl.find by_index idx in
			read_connection t perms path c rest
		| "domain" :: [] -> ""
		| "domain" :: domid :: rest ->
			let address = Xs_protocol.Domain(int_of_string domid) in
			if not(Hashtbl.mem by_address address) then Store.Path.doesnt_exist path;
			let c = Hashtbl.find by_address address in
			read_connection t perms path c rest
		| _ -> Store.Path.doesnt_exist path

	let exists t perms path = try ignore(read t perms path); true with Store.Path.Doesnt_exist _ -> false

	let write_connection t creator perms path c v = function
		| "backend" :: rest ->
			begin match c.interface with
			| None -> Store.Path.doesnt_exist path
			| Some i ->
				let module I = (val i: Namespace.IO) in
				I.write t creator perms (Store.Path.of_string_list rest) v
			end
		| _ -> raise Perms.Permission_denied

	let write t creator (perms: Perms.t) (path: Store.Path.t) v =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| "socket" :: idx :: rest ->
			let idx = int_of_string idx in
			if not(Hashtbl.mem by_index idx) then Store.Path.doesnt_exist path;
			let c = Hashtbl.find by_index idx in
			write_connection t creator perms path c v rest
		| "domain" :: domid :: rest ->
			let address = Xs_protocol.Domain(int_of_string domid) in
			if not(Hashtbl.mem by_address address) then Store.Path.doesnt_exist path;
			let c = Hashtbl.find by_address address in
			write_connection t creator perms path c v rest
		| _ -> raise Perms.Permission_denied

	let rec between start finish = if start > finish then [] else start :: (between (start + 1) finish)

	let list_connection t perms c = function
		| [] ->
			[ "address"; "current-transactions"; "total-operations"; "watch"; "current-watch-queue-length"; "total-dropped-watches"; "backend" ]
		| [ "watch" ] ->
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			List.map string_of_int (between 0 (List.length all - 1))
		| [ "watch"; _ ] -> [ "name"; "token"; "total-events" ]
		| "backend" :: rest ->
			begin match c.interface with
			| None -> []
			| Some i ->
				let module I = (val i: Namespace.IO) in
				I.list t perms (Store.Path.of_string_list rest)
			end
		| _ -> []

	let list t perms path =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> [ "socket"; "domain" ]
		| [ "socket" ] ->
			Hashtbl.fold (fun x c acc -> match x with
			| Xs_protocol.Unix _ -> string_of_int c.idx :: acc
			| _ -> acc) by_address []
		| [ "domain" ] ->
			Hashtbl.fold (fun x _ acc -> match x with
			| Xs_protocol.Domain x -> string_of_int x :: acc
			| _ -> acc) by_address []
		| "domain" :: domid :: rest ->
			let address = Xs_protocol.Domain(int_of_string domid) in
			if not(Hashtbl.mem by_address address) then Store.Path.doesnt_exist path;
			let c = Hashtbl.find by_address address in
			list_connection t perms c rest
		| "socket" :: idx :: rest ->
			let idx = int_of_string idx in
			if not(Hashtbl.mem by_index idx) then Store.Path.doesnt_exist path;
			let c = Hashtbl.find by_index idx in
			list_connection t perms c rest
		| _ -> []
end
