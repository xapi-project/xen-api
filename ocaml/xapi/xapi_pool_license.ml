(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

open Fun

module D=Debug.Debugger(struct let name="xapi" end)
open D

(* Compare two date options, where None is always greater than (Some _) *)
let compare_dates (a: Date.iso8601 option) (b: Date.iso8601 option) =
	match a, b with
	| None, None -> 0
	| None, Some _ -> 1
	| Some _, None -> -1
	| Some a', Some b' -> compare a' b'

(* Get the earliest expiry date of a list of hosts. *)
let get_earliest_expiry_date ~__context ~hosts =
	List.map (fun host -> License_check.get_expiry_date ~__context ~host) hosts
		|> List.sort compare_dates
		|> List.hd

(* If any hosts are free edition, then the pool is free edition.
 * Otherwise, the pool has the same edition as the first host.
 * We assume that the pool won't contain a mixture of xendesktop and
 * per-socket licenses. *)
let get_lowest_edition ~__context ~hosts =
	let all_editions =
		List.map
			(fun host -> Db.Host.get_edition ~__context ~self:host)
			hosts
	in
	if List.mem "free" all_editions
	then "free"
	else List.hd all_editions

(* Separate this logic out from Xapi_pool.apply_edition for testing purposes. *)
let apply_edition_with_rollback ~__context ~hosts ~edition ~apply_fn =
	(* Filter out the hosts which already have the correct edition;
	 * list the other hosts against the edition we're upgrading *from*. *)
	let to_apply =
		List.fold_left
			(fun acc host ->
				let old_edition = Db.Host.get_edition ~__context ~self:host in
				if old_edition <> edition
				then (host, old_edition) :: acc
				else acc)
			[] hosts
	in
	(* This list will be added to as hosts have the new edition applied. *)
	let to_rollback = ref [] in
	try
		List.iter
			(fun (host, old_edition) ->
				apply_fn ~__context ~host ~edition;
				to_rollback := (host, old_edition) :: !to_rollback)
			to_apply
	with e ->
		debug
			"Caught %s while trying to upgrade pool to edition %s - attempting rollback"
			(Printexc.to_string e) edition;
		(* Best-effort attempt to roll everything back. *)
		List.iter
			(fun (host, old_edition) ->
				try apply_fn ~__context ~host ~edition:old_edition with _ -> ())
			!to_rollback;
		(* Raise the original exception. *)
		raise e
