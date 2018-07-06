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

let warn  fmt = Logging.warn  "quota" fmt

exception Limit_reached
exception Data_too_big
exception Transaction_opened

type domid = int

(* Global defaults *)
let maxent = ref (10000)
let maxsize = ref (4096)
let maxwatch = ref 50
let maxtransaction = ref 20
let maxwatchevent = ref 256

type overrides = (int, int) Hashtbl.t

let maxent_overrides = Hashtbl.create 10
let maxwatch_overrides = Hashtbl.create 10
let maxtransaction_overrides = Hashtbl.create 10
let maxwatchevent_overrides = Hashtbl.create 10

let get_override t domid =
	if Hashtbl.mem t domid
	then Some(Hashtbl.find t domid)
	else None

let set_override t domid override = match override with
	| None -> Hashtbl.remove t domid
	| Some override -> Hashtbl.replace t domid override

let list_overrides t =
	Hashtbl.fold (fun domid x acc -> (domid, x) :: acc) t []

let of_domain t default domid =
	if Hashtbl.mem t domid
	then Hashtbl.find t domid
	else !default

let maxent_of_domain = of_domain maxent_overrides maxent
let maxwatch_of_domain = of_domain maxwatch_overrides maxwatch
let maxtransaction_of_domain = of_domain maxtransaction_overrides maxtransaction
let maxwatchevent_of_domain = of_domain maxwatchevent_overrides maxwatchevent

type t = {
	cur: (domid, int) Hashtbl.t; (* current domains entry usage *)
}

let create () =
	{ cur = Hashtbl.create 100; }

let copy quota = { cur = (Hashtbl.copy quota.cur) }

(*let del quota id = Hashtbl.remove quota.cur id*)

let check _quota id size =
	if size > !maxsize then (
		warn "domain %u err create entry: data too big %d" id size;
		raise Data_too_big
	)

let list quota =
	Hashtbl.fold (fun domid x acc -> (domid, x) :: acc) quota.cur []

let get quota id =
	if Hashtbl.mem quota.cur id
	then Hashtbl.find quota.cur id
	else 0

let set quota id nb =
	if nb = 0
	then Hashtbl.remove quota.cur id
	else begin
	if Hashtbl.mem quota.cur id then
		Hashtbl.replace quota.cur id nb
	else
		Hashtbl.add quota.cur id nb
	end

let decr quota id =
	let nb = get quota id in
	if nb > 0
	then set quota id (nb - 1)

let incr quota id =
	let nb = get quota id in
	let maxent = maxent_of_domain id in
	if nb >= maxent then raise Limit_reached;
	set quota id (nb + 1)

let union quota diff =
	Hashtbl.iter (fun id nb -> set quota id (get quota id + nb)) diff.cur

let merge orig_quota mod_quota dest_quota =
  Hashtbl.iter (fun id nb ->
    let diff = nb - (get orig_quota id) in
    if diff <> 0 then
      set dest_quota id ((get dest_quota id) + diff)) mod_quota.cur


