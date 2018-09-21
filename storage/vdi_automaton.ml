(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(**
 * @group Storage
 *)


(** An automaton representing the VDI state machine *)

type ro_rw = RO | RW [@@deriving rpcty]

let string_of_ro_rw = function
	| RO -> "RO" | RW -> "RW"

type state =
	| Detached
	| Attached of ro_rw
	| Activated of ro_rw
[@@deriving rpcty]

let string_of_state = function
	| Detached        -> "detached"
	| Attached ro_rw  -> Printf.sprintf "attached  %s" (string_of_ro_rw ro_rw)
	| Activated ro_rw -> Printf.sprintf "activated %s" (string_of_ro_rw ro_rw)

let every_state = [
	Detached;
	Attached RO; Attached RW;
	Activated RO; Activated RW
]

type op =
	| Nothing
	| Attach of ro_rw
	| Detach
	| Activate
	| Deactivate

let every_op = [
	Nothing;
	Attach RO; Attach RW;
	Activate;
	Detach; Deactivate;
]

let string_of_op = function
	| Nothing        -> "nothing"
	| Attach ro_rw   -> Printf.sprintf "attach(%s)" (string_of_ro_rw ro_rw)
	| Detach         -> "detach"
	| Activate       -> Printf.sprintf "activate"
	| Deactivate     -> "deactivate"

exception Bad_transition of state * op

let ( + ) state operation =
	let error () = raise (Bad_transition (state, operation)) in

	let ro_rw x y = match x,y with
		| RO, RO -> RO
		| RW, RW -> RW
		| RO, RW -> error ()
		| RW, RO -> RW in
	match state, operation with
		| x,           Nothing    -> x
		| Detached,    Attach x   -> Attached x
		| Detached,    Activate   -> error ()
		| Detached,    Deactivate -> Detached
		| Detached,    Detach     -> Detached
		| Attached x,  Attach y   -> Attached (ro_rw x y)
		| Attached x,  Activate   -> Activated x
		| Attached _,  Detach     -> Detached
		| Attached x,  Deactivate -> Attached x
		| Activated x, Attach y   -> Activated (ro_rw x y)
		| Activated x, Activate   -> Activated x
		| Activated x, Deactivate -> Attached x
		| Activated _, Detach     -> error ()

let superstate states =
	let activated = List.fold_left (fun acc s ->
		acc || (s = Activated RO) || (s = Activated RW)) false states in
	let rw = List.fold_left (fun acc s ->
		acc || (s = Activated RW) || (s = Attached RW)) false states in
	if states = []
	then Detached
	else
		if activated
		then Activated (if rw then RW else RO)
		else Attached (if rw then RW else RO)

exception No_operation of state * state

(* x - y = [ (op, state_on_fail)+ ] *)
let ( - ) x y = match x, y with
	| Detached,     Detached     -> [ Nothing, Detached ]
	| Attached RO,  Attached RO  -> [ Nothing, Attached RO ]
	| Activated RO, Activated RO -> [ Nothing, Activated RO ]
	| Attached RW,  Attached RW  -> [ Nothing, Attached RW ]
	| Activated RW, Activated RW -> [ Nothing, Activated RW ]
	| Attached r,   Detached     -> [ Detach, Attached r ]
	| Activated RO, Attached RO  -> [ Deactivate, Activated RO ]
	| Activated RW, Attached RW  -> [ Deactivate, Activated RW ]
	| Activated r,  Detached     -> [ Deactivate, Activated r; Detach, Attached r ]
	| Detached,     Attached RO  -> [ Attach RO, Detached ]
	| Detached,     Attached RW  -> [ Attach RW, Detached ]
	| Detached,     Activated RO -> [ Attach RO, Detached; Activate, Attached RO ]
	| Detached,     Activated RW -> [ Attach RW, Detached; Activate, Attached RW ]
	| Attached RO,  Activated RO -> [ Activate, Attached RO ]
	| Attached RW,  Activated RW -> [ Activate, Attached RW ]
	| _, _ -> raise (No_operation (x, y))

