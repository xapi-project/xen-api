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
(**
 * @group Memory
 *)

open Memory_interface
open Squeezed_state
open Squeezed_xenstore
open Threadext

module D = Debug.Make(struct let name = Memory_interface.service_name end)
open D

type context = unit

(** The main body of squeezed is single-threaded, so we protect it with a mutex here. *)
let big_server_mutex = Mutex.create ()

let wrap dbg f = 
	try
(*
		Debug.with_thread_associated dbg
		(fun () ->
*)
			Mutex.execute big_server_mutex f
(*
		) ()
*)
	with
	| Squeeze.Cannot_free_this_much_memory (needed, free) ->
		(* NB both needed and free have been inflated by the lowmem_emergency_pool etc *)
		let needed = Int64.sub needed Squeeze_xen.target_host_free_mem_kib 
		and free = Int64.sub free Squeeze_xen.target_host_free_mem_kib in
		raise (Memory_interface.Cannot_free_this_much_memory (needed, free))
	| Squeeze.Domains_refused_to_cooperate domids ->
		raise (Memory_interface.Domains_refused_to_cooperate(domids))


let start_balance_thread balance_check_interval =
	let body () =
		Xenctrl.with_intf
			(fun xc ->
				while true do
					Thread.delay !balance_check_interval;
					wrap "auto-balance"
					(fun () ->
						if Squeeze_xen.is_host_memory_unbalanced ~xc
						then Squeeze_xen.balance_memory ~xc
					)
				done
			) in
	let (_: Thread.t) = Thread.create body () in
	()
  

let get_diagnostics _ dbg = "diagnostics not yet available"

let login _ dbg service_name =
	wrap dbg
	(fun () ->
		(* We assume only one instance of a named service logs in at a time and therefore can use
		   the service name as a session_id. *)
		(* remove any existing reservations associated with this service *)
		Xenctrl.with_intf
		(fun xc ->
			Client.with_xs client (fun xs -> Client.rm xs (state_path _service ^ "/" ^ service_name))
		);
		service_name
	)

let reserve_memory _ dbg session_id kib =
	let reservation_id = Uuidm.to_string (Uuidm.create `V4) in
	if kib < 0L
	then raise (Invalid_memory_value kib);
	wrap dbg
	(fun () ->
		Xenctrl.with_intf
		(fun xc ->
			Squeeze_xen.free_memory ~xc kib;
			debug "reserved %Ld kib for reservation %s" kib reservation_id;
			add_reservation _service session_id reservation_id (Int64.to_string kib)
		);
		reservation_id
	)

let reserve_memory_range _ dbg session_id min max = 
	let reservation_id = Uuidm.to_string (Uuidm.create `V4) in
	if min < 0L 
	then raise (Invalid_memory_value min);
	if max < 0L 
	then raise (Invalid_memory_value max);
	wrap dbg
	(fun () ->
		Xenctrl.with_intf
		(fun xc ->
			let amount = Squeeze_xen.free_memory_range ~xc min max in
			debug "reserved %Ld kib for reservation %s" amount reservation_id;
			add_reservation _service session_id reservation_id (Int64.to_string amount);
			reservation_id, amount
		)
	)


let delete_reservation _ dbg session_id reservation_id =
	wrap dbg
	(fun () ->
		Xenctrl.with_intf
		(fun xc ->
			del_reservation _service session_id reservation_id
		)
	)

let transfer_reservation_to_domain _ dbg session_id reservation_id domid =
	wrap dbg
	(fun () ->
		Xenctrl.with_intf
		(fun xc ->
			try
				let kib = Client.with_xs client (fun xs -> Client.read xs (reservation_path _service session_id reservation_id)) in
				(* This code is single-threaded, no need to make this transactional: *)
				Client.with_xs client (fun xs -> Client.write xs (Printf.sprintf "/local/domain/%d/memory/initial-reservation" domid) kib);
				Opt.iter
					(fun maxmem -> Squeeze_xen.Domain.set_maxmem_noexn xc domid maxmem)
					(try Some (Int64.of_string kib) with _ -> None);
				del_reservation _service session_id reservation_id;
			with Xs_protocol.Enoent _ ->
				raise (Unknown_reservation reservation_id)
		)
	)

let balance_memory _ dbg = 
	wrap dbg
	(fun () ->
		Xenctrl.with_intf
		(fun xc ->
			Squeeze_xen.balance_memory ~xc
		)
	)

let get_host_reserved_memory _ dbg = Squeeze_xen.target_host_free_mem_kib
