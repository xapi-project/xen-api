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

type 'a sm_result = SMSuccess of 'a | SMFailure of exn

let multicast f = Hashtbl.fold (fun sr plugin acc -> (sr, try SMSuccess (f sr plugin.processor) with e -> SMFailure e) :: acc) plugins []

let success = function | SMSuccess _ -> true | _ -> false

let string_of_sm_result f = function 
	| SMSuccess x -> Printf.sprintf "Success: %s" (f x)
	| SMFailure e -> Printf.sprintf "Failure: %s" (Printexc.to_string e)

let partition l = List.partition (success ++ snd) l

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
		let create context ~dbg ~id = id (* XXX: is this pointless? *)
		let destroy context ~dbg ~dp ~allow_leak =
			(* Tell each plugin about this *)
			match fail_or choose (multicast (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.DP.destroy ~dbg ~dp ~allow_leak)) with
				| SMSuccess x -> x
				| SMFailure e -> raise e

		let diagnostics context () =
			let combine results =
				let all = List.fold_left (fun acc (sr, result) ->
					(Printf.sprintf "For SR: %s" sr :: (string_of_sm_result (fun s -> s) result) :: acc)) [] results in
				SMSuccess (String.concat "\n" all) in
			match fail_or combine (multicast (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.DP.diagnostics ())) with
				| SMSuccess x -> x
				| SMFailure e -> raise e

		let attach_info context ~dbg ~sr ~vdi ~dp =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.DP.attach_info ~dbg ~sr ~vdi ~dp
			
	end
	module SR = struct
		let attach context ~dbg ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.attach ~dbg ~sr
		let detach context ~dbg ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.detach ~dbg ~sr
		let destroy context ~dbg ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.destroy ~dbg ~sr
		let scan context ~dbg ~sr =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.scan ~dbg ~sr
		let list context ~dbg =
			List.fold_left (fun acc (sr, list) -> match list with SMSuccess l -> l @ acc | x -> acc) [] (multicast (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.SR.list ~dbg)) 
		let reset context ~dbg ~sr = assert false
	end
	module VDI = struct
		let create context ~dbg ~sr ~vdi_info ~params =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.create ~dbg ~sr ~vdi_info ~params

		let stat context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.stat ~dbg ~sr ~vdi
        let snapshot context ~dbg ~sr ~vdi ~vdi_info ~params =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.snapshot ~dbg ~sr ~vdi ~vdi_info ~params
        let clone context ~dbg ~sr ~vdi ~vdi_info ~params =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.clone ~dbg ~sr ~vdi ~vdi_info ~params
		let destroy context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.destroy ~dbg ~sr ~vdi
		let attach context ~dbg ~dp ~sr ~vdi ~read_write =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write
		let activate context ~dbg ~dp ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.activate ~dbg ~dp ~sr ~vdi
		let deactivate context ~dbg ~dp ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.deactivate ~dbg ~dp ~sr ~vdi
		let detach context ~dbg ~dp ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.detach ~dbg ~dp ~sr ~vdi
        let get_by_name context ~dbg ~sr ~name =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.get_by_name ~dbg ~sr ~name
        let set_content_id context ~dbg ~sr ~vdi ~content_id =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.set_content_id ~dbg ~sr ~vdi ~content_id
        let similar_content context ~dbg ~sr ~vdi =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.similar_content ~dbg ~sr ~vdi
		let copy context ~dbg ~sr ~vdi ~dp ~url ~dest = Storage_migrate.copy ~dbg ~sr ~vdi ~dp ~url ~dest
		let compose context ~dbg ~sr ~vdi1 ~vdi2 =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.compose ~dbg ~sr ~vdi1 ~vdi2
        let copy_into context ~dbg ~sr ~vdi ~url ~dest = Storage_migrate.copy_into ~dbg ~sr ~vdi ~url ~dest
        let get_url context ~dbg ~sr ~vdi =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.get_url ~dbg ~sr ~vdi

	end

    let get_by_name context ~dbg ~name =
        (* Assume it has either the format:
           SR/VDI -- for a particular SR and VDI
           content_id -- for a particular content *)
        let open Stringext in
        match List.filter (fun x -> x <> "") (String.split '/' name) with
            | [ sr; name ] ->
                let module C = Client(struct let rpc = of_sr sr end) in
                C.VDI.get_by_name ~dbg ~sr ~name
            | [ name ] ->
                (match success_or choose (multicast (fun sr rpc ->
                    let module C = Client(struct let rpc = of_sr sr end) in
                    C.VDI.get_by_name ~dbg ~sr ~name
                )) with SMSuccess x -> x
					| SMFailure e -> raise e)
            | _ ->
                raise Vdi_does_not_exist

    module Mirror = struct
        let start context ~dbg ~sr ~vdi ~dp ~url ~dest = Storage_migrate.start ~dbg ~sr ~vdi ~dp ~url ~dest 
        let stop context ~dbg ~sr ~vdi = Storage_migrate.stop ~dbg ~sr ~vdi
		let list context = Storage_migrate.list
		let receive_start context = Storage_migrate.receive_start
		let receive_finalize context = Storage_migrate.receive_finalize
		let receive_cancel context = Storage_migrate.receive_cancel
    end	

	module TASK = struct
		let stat context ~dbg ~task = assert false
		let cancel context ~dbg ~task = assert false				
		let destroy context ~dbg ~task = assert false
		let list context ~dbg = assert false
    end

	module UPDATES = struct
		let get context ~dbg ~from ~timeout = assert false
	end

end

module Server = Storage_interface.Server(Storage_impl.Wrapper(Mux))
