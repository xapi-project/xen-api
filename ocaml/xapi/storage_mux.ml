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

module D=Debug.Debugger(struct let name="mux" end)
open D

type processor = Rpc.call -> Rpc.response

open Storage_interface

type plugin = {
	processor: processor;
	backend_domain: string;
}
let plugins : (sr, plugin) Hashtbl.t = Hashtbl.create 10

let debug_printer rpc call =
	debug "Rpc.call = %s" (Xmlrpc.string_of_call call);
	let result = rpc call in
	debug "Rpc.response = %s" (Xmlrpc.string_of_response result);
	result

let register sr m d = Hashtbl.replace plugins sr { processor = debug_printer m; backend_domain = d }
let unregister sr = Hashtbl.remove plugins sr

exception No_storage_plugin_for_sr of string

(* This is the policy: *)
let of_sr sr =
	if not (Hashtbl.mem plugins sr) then begin
		error "No storage plugin for SR: %s" sr;
		raise (No_storage_plugin_for_sr sr)
	end else (Hashtbl.find plugins sr).processor

let domid_of_sr sr =
	if not (Hashtbl.mem plugins sr) then begin
		error "No storage plugin for SR: %s" sr;
		raise (No_storage_plugin_for_sr sr)
	end;
	let uuid = (Hashtbl.find plugins sr).backend_domain in
	try
		Vmopshelpers.with_xc
			(fun xc ->
				let all = Xenctrl.domain_getinfolist xc 0 in
				let di = List.find (fun x -> Uuid.to_string (Uuid.uuid_of_int_array x.Xenctrl.handle) = uuid) all in
				di.Xenctrl.domid
			)
	with _ ->
		failwith (Printf.sprintf "Failed to find domid of driver domain %s (SR %s)" uuid sr)

open Fun

let multicast f = Hashtbl.fold (fun sr plugin acc -> (sr, f plugin.processor) :: acc) plugins []

let partition = List.partition (success ++ snd)

let choose x = snd(List.hd x)

let fail_or f results =
	let successes, errors = partition results in
	if errors <> [] then choose errors else f successes

module Mux = struct
	type context = Smint.request

    let query context () = {
        name = "storage multiplexor";
        vendor = "XCP";
        version = "0.1";
        features = [];
    }
	module DP = struct
		let create context ~task ~id = id (* XXX: is this pointless? *)
		let destroy context ~task ~dp ~allow_leak =
			(* Tell each plugin about this *)
			fail_or choose (multicast (fun rpc ->
				let module C = Client(struct let rpc = rpc end) in
				C.DP.destroy ~task ~dp ~allow_leak))
		let diagnostics context () =
			let combine results =
				let all = List.fold_left (fun acc (sr, result) ->
					Printf.sprintf "For SR: %s" sr :: (string_of_result result) :: acc) [] results in
				Success (String (String.concat "\n" all)) in
			fail_or combine (multicast (fun rpc ->
				let module C = Client(struct let rpc = rpc end) in
				C.DP.diagnostics ()))
	end
	module SR = struct
		let attach context ~task ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.attach ~task ~sr
		let detach context ~task ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.detach ~task ~sr
		let destroy context ~task ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.destroy ~task ~sr
		let scan context ~task ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.scan ~task ~sr
		let list context ~task =
			List.fold_left (fun acc (sr, list) -> list @ acc) [] (multicast (fun rpc ->
				let module C = Client(struct let rpc = rpc end) in
				C.SR.list ~task))
		let reset context ~task ~sr = assert false
	end
	module VDI = struct
		let create context ~task ~sr ~vdi_info ~params =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.create ~task ~sr ~vdi_info ~params

		let stat context ~task ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.stat ~task ~sr ~vdi
		let destroy context ~task ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.destroy ~task ~sr ~vdi
		let attach context ~task ~dp ~sr ~vdi ~read_write =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.attach ~task ~dp ~sr ~vdi ~read_write
		let activate context ~task ~dp ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.activate ~task ~dp ~sr ~vdi
		let deactivate context ~task ~dp ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.deactivate ~task ~dp ~sr ~vdi
		let detach context ~task ~dp ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.detach ~task ~dp ~sr ~vdi
	end
end

module Server = Storage_interface.Server(Storage_impl.Wrapper(Mux))
