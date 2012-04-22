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

open Fun

let multicast f = Hashtbl.fold (fun sr plugin acc -> (sr, f sr plugin.processor) :: acc) plugins []

let partition = List.partition (success ++ snd)

let choose x = snd(List.hd x)

let fail_or f results =
	let successes, errors = partition results in
	if errors <> [] then choose errors else f successes

let success_or f results =
    let successes, errors = partition results in
    if successes <> [] then f successes else f errors

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
			fail_or choose (multicast (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.DP.destroy ~task ~dp ~allow_leak))
		let diagnostics context () =
			let combine results =
				let all = List.fold_left (fun acc (sr, result) ->
					Printf.sprintf "For SR: %s" sr :: (string_of_result result) :: acc) [] results in
				Success (String (String.concat "\n" all)) in
			fail_or combine (multicast (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.DP.diagnostics ()))
		let params context ~task ~sr ~vdi ~dp =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.DP.params ~task ~sr ~vdi ~dp
			
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
			List.fold_left (fun acc (sr, list) -> list @ acc) [] (multicast (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
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
        let snapshot context ~task ~sr ~vdi ~vdi_info ~params =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.snapshot ~task ~sr ~vdi ~vdi_info ~params
        let clone context ~task ~sr ~vdi ~vdi_info ~params =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.clone ~task ~sr ~vdi ~vdi_info ~params
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
        let get_by_name context ~task ~sr ~name =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.get_by_name ~task ~sr ~name
        let set_content_id context ~task ~sr ~vdi ~content_id =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.set_content_id ~task ~sr ~vdi ~content_id
        let similar_content context ~task ~sr ~vdi =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.similar_content ~task ~sr ~vdi
		let compose context ~task ~sr ~vdi1 ~vdi2 =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.compose ~task ~sr ~vdi1 ~vdi2
        let copy context ~task ~sr ~vdi ~url ~dest = Storage_migrate.copy ~task ~sr ~vdi ~url ~dest
        let get_url context ~task ~sr ~vdi =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.get_url ~task ~sr ~vdi

	end

    let get_by_name context ~task ~name =
        (* Assume it has either the format:
           SR/VDI -- for a particular SR and VDI
           content_id -- for a particular content *)
        let open Stringext in
        match List.filter (fun x -> x <> "") (String.split '/' name) with
            | [ sr; name ] ->
                let module C = Client(struct let rpc = of_sr sr end) in
                C.VDI.get_by_name ~task ~sr ~name
            | [ name ] ->
                success_or choose (multicast (fun sr rpc ->
                    let module C = Client(struct let rpc = of_sr sr end) in
                    C.VDI.get_by_name ~task ~sr ~name
                ))
            | _ ->
                Failure Vdi_does_not_exist

    module Mirror = struct
        let start context ~task ~sr ~vdi ~dp ~url ~dest = Storage_migrate.start ~task ~sr ~vdi ~dp ~url ~dest 
        let stop context ~task ~sr ~vdi = Storage_migrate.stop ~task ~sr ~vdi
		let active context = Storage_migrate.active
		let receive_start context = Storage_migrate.receive_start
		let receive_finalize context = Storage_migrate.receive_finalize
		let receive_cancel context = Storage_migrate.receive_cancel
    end	

end

module Server = Storage_interface.Server(Storage_impl.Wrapper(Mux))
