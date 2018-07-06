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

let debug fmt = Logging.debug "transaction" fmt

open Junk

let none = 0l
let test_eagain = ref false

let check_parents_perms_identical root1 root2 path =
	let hierarch = Store.Path.get_hierarchy path in
	let permdiff = List.fold_left (fun acc path ->
		let n1 = Store.lookup root1 path
		and n2 = Store.lookup root2 path in
		match n1, n2 with
		| Some n1, Some n2 ->
			(Store.Node.get_perms n1) <> (Store.Node.get_perms n2) || acc
		| _ ->
			true || acc
	) false hierarch in
	(not permdiff)

let get_lowest path1 path2 =
	match path2 with
	| None       -> Some path1
	| Some path2 -> Some (Store.Path.get_common_prefix path1 path2)

let test_coalesce oldroot currentroot path =
	let oldnode = Store.lookup oldroot path
	and currentnode = Store.lookup currentroot path in

	match oldnode, currentnode with
		| (Some oldnode), (Some currentnode) ->
			if oldnode == currentnode then (
				check_parents_perms_identical oldroot currentroot path
			) else (
				false
			)
		| None, None -> (
			(* ok then it doesn't exists in the old version and the current version,
			   just sneak it in as a child of the parent node if it exists, or else fail *)
			let pnode = Store.lookup currentroot (Store.Path.get_parent path) in
			match pnode with
			| None       -> false (* ok it doesn't exists, just bail out. *)
			| Some _pnode -> true
			)
		| _ ->
			false

let can_coalesce oldroot currentroot path =
	try test_coalesce oldroot currentroot path with _ -> false

type ty = No | Full of (int32 * Store.Node.t * Store.t)

type t = {
	ty: ty;
	store: Store.t;
	quota: Quota.t;
	mutable paths: (Xs_protocol.Op.t * Store.Name.t) list;
	mutable operations: (Xs_protocol.Request.payload * Xs_protocol.Response.payload) list;
	mutable read_lowpath: Store.Path.t option;
	mutable write_lowpath: Store.Path.t option;
}

let make id store =
	let ty = if id = none then No else Full(id, store.Store.root, store) in
	{
		ty = ty;
		store = if id = none then store else Store.copy store;
		quota = Quota.copy  store.Store.quota;
		paths = [];
		operations = [];
		read_lowpath = None;
		write_lowpath = None;
	}

let get_id t = match t.ty with No -> none | Full (id, _, _) -> id
let get_store t = t.store
let get_paths t = t.paths

let add_wop t ty path = t.paths <- (ty, Store.Path.to_name path) :: t.paths
let add_operation t request response = t.operations <- (request, response) :: t.operations
let get_operations t = List.rev t.operations
let set_read_lowpath t path = t.read_lowpath <- get_lowest path t.read_lowpath
let set_write_lowpath t path = t.write_lowpath <- get_lowest path t.write_lowpath

let exists t _perms path = Store.exists t.store path

let write t creator perm path value =
	let path_existed = exists t perm path in
	Store.write t.store creator perm path value;
	if path_existed
	then set_write_lowpath t path
	else set_write_lowpath t (Store.Path.get_parent path);
	add_wop t Xs_protocol.Op.Write path

let mkdir ?(with_watch=true) t creator perm path =
	Store.mkdir t.store creator perm path;
	set_write_lowpath t path;
	if with_watch then
		add_wop t Xs_protocol.Op.Mkdir path

let setperms t perm path perms =
	Store.setperms t.store perm path perms;
	set_write_lowpath t path;
	add_wop t Xs_protocol.Op.Setperms path

let rm t perm path =
	Store.rm t.store perm path;
	set_write_lowpath t (Store.Path.get_parent path);
	add_wop t Xs_protocol.Op.Rm path

let list t perm path =	
	let r = Store.ls t.store perm path in
	set_read_lowpath t path;
	r

let read t perm path =
	let r = Store.read t.store perm path in
	set_read_lowpath t path;
	r

let getperms t perm path =
	let r = Store.getperms t.store perm path in
	set_read_lowpath t path;
	r

let commit ~con t =
	let has_write_ops = List.length t.paths > 0 in
	let has_coalesced = ref false in
	let has_commited =
	match t.ty with
	| No                         -> true
	| Full (_id, oldroot, cstore) ->
		let commit_partial oldroot cstore store =
			(* get the lowest path of the query and verify that it hasn't
			   been modified by others transactions. *)
			let readpath_ok = match t.read_lowpath with
				| None -> true (* no reads recorded *)
				| Some path -> can_coalesce oldroot cstore.Store.root path in
			let writepath_ok = match t.write_lowpath with
				| None -> true (* no writes recorded *)
				| Some path -> can_coalesce oldroot cstore.Store.root path in
			if readpath_ok && writepath_ok then (
				maybe (fun p ->
					let n = Store.lookup store.Store.root p in

					(* it has to be in the store, otherwise it means bugs
					   in the lowpath registration. we don't need to handle none. *)
					maybe (fun n -> Store.set_node cstore p n t.quota store.Store.quota) n;
					Logging.write_coalesce ~tid:(get_id t) ~con (Store.Path.to_string p);
				) t.write_lowpath;
				maybe (fun p ->
					Logging.read_coalesce ~tid:(get_id t) ~con (Store.Path.to_string p)
					) t.read_lowpath;
				has_coalesced := true;
				cstore.Store.stat_transaction_coalesce <- cstore.Store.stat_transaction_coalesce + 1;
				true
			) else (
				(* cannot do anything simple, just discard the queries,
				   and the client need to redo it later *)
				cstore.Store.stat_transaction_abort <- cstore.Store.stat_transaction_abort + 1;
				false
			)
			in
		let try_commit oldroot cstore store =
			if oldroot == cstore.Store.root then (
				(* move the new root to the current store, if the oldroot
				   has not been modified *)
				if has_write_ops then (
					Store.set_root cstore store.Store.root;
					Store.set_quota cstore store.Store.quota
				);
				true
			) else
				(* we try a partial commit if possible *)
				commit_partial oldroot cstore store
			in
		if !test_eagain && Random.int 3 = 0 then
			false
		else
			try_commit oldroot cstore t.store
	in
(*
	if has_commited && has_write_ops then
		Disk.write t.store;
*)
	if not has_commited 
	then Logging.conflict ~tid:(get_id t) ~con
	else if not !has_coalesced 
	then Logging.commit ~tid:(get_id t) ~con;
	has_commited
