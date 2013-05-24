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

open Threadext
open Storage_interface

type plugin = {
	processor: processor;
	backend_domain: string;
	query_result: query_result;
}
let plugins : (sr, plugin) Hashtbl.t = Hashtbl.create 10
let m = Mutex.create ()

let debug_printer rpc call =
	(* debug "Rpc.call = %s" (Xmlrpc.string_of_call call); *)
	let result = rpc call in
	(* debug "Rpc.response = %s" (Xmlrpc.string_of_response result); *)
	result

let register sr rpc d info =
	Mutex.execute m
		(fun () ->
			Hashtbl.replace plugins sr { processor = debug_printer rpc; backend_domain = d; query_result = info };
			debug "register SR %s (currently-registered = [ %s ])" sr (String.concat ", " (Hashtbl.fold (fun sr _ acc -> sr :: acc) plugins []))
		)

let unregister sr =
	Mutex.execute m
		(fun () ->
			Hashtbl.remove plugins sr;
			debug "unregister SR %s (currently-registered = [ %s ])" sr (String.concat ", " (Hashtbl.fold (fun sr _ acc -> sr :: acc) plugins []))
		)

let query_result_of_sr sr =
	try
		Mutex.execute m
			(fun () ->
				Some (Hashtbl.find plugins sr).query_result
			)
	with _ -> None

let features_of_sr sr = Opt.default [] (Opt.map (fun x -> x.features) (query_result_of_sr sr))

(* This is the policy: *)
let of_sr sr =
	Mutex.execute m
		(fun () ->
			if not (Hashtbl.mem plugins sr) then begin
				error "No storage plugin for SR: %s (currently-registered = [ %s ])" sr (String.concat ", " (Hashtbl.fold (fun sr _ acc -> sr :: acc) plugins []));
				raise (No_storage_plugin_for_sr sr)
			end else (Hashtbl.find plugins sr).processor
		)

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

	let forall f =
        let combine results =
            let all = List.fold_left (fun acc (sr, result) ->
                (Printf.sprintf "For SR: %s" sr :: (string_of_sm_result (fun s -> s) result) :: acc)) [] results in
            SMSuccess (String.concat "\n" all) in
        match fail_or combine (multicast f) with
			| SMSuccess x -> x
			| SMFailure e -> raise e

	module Query = struct
		let query context ~dbg = {
			driver = "mux";
			name = "storage multiplexor";
			description = "forwards calls to other plugins";
			vendor = "XCP";
			copyright = "see the source code";
			version = "2.0";
			required_api_version = "2.0";
			features = [];
			configuration = []
		}
		let diagnostics context ~dbg =
			forall (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.Query.diagnostics dbg
			)
	end
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
			forall (fun sr rpc ->
				let module C = Client(struct let rpc = of_sr sr end) in
				C.DP.diagnostics ()
			)

		let attach_info context ~dbg ~sr ~vdi ~dp =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.DP.attach_info ~dbg ~sr ~vdi ~dp

		let stat_vdi context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.DP.stat_vdi ~dbg ~sr ~vdi
			
	end
	module SR = struct
		let create context ~dbg ~sr ~device_config ~physical_size =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.create ~dbg ~sr ~device_config ~physical_size
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
		let update_snapshot_info_src context = Storage_migrate.update_snapshot_info_src
		let update_snapshot_info_dest context ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.SR.update_snapshot_info_dest ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs
	end
	module VDI = struct
		let create context ~dbg ~sr ~vdi_info =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.create ~dbg ~sr ~vdi_info
        let snapshot context ~dbg ~sr ~vdi_info =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.snapshot ~dbg ~sr ~vdi_info
        let clone context ~dbg ~sr ~vdi_info =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.clone ~dbg ~sr ~vdi_info
		let destroy context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.destroy ~dbg ~sr ~vdi
		let stat context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.stat ~dbg ~sr ~vdi
		let set_persistent context ~dbg ~sr ~vdi ~persistent =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.set_persistent ~dbg ~sr ~vdi ~persistent
		let epoch_begin context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.epoch_begin ~dbg ~sr ~vdi
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
		let epoch_end context ~dbg ~sr ~vdi =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.epoch_end ~dbg ~sr ~vdi
        let get_by_name context ~dbg ~sr ~name =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.get_by_name ~dbg ~sr ~name
        let set_content_id context ~dbg ~sr ~vdi ~content_id =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.set_content_id ~dbg ~sr ~vdi ~content_id
        let similar_content context ~dbg ~sr ~vdi =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.similar_content ~dbg ~sr ~vdi
		let compose context ~dbg ~sr ~vdi1 ~vdi2 =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.compose ~dbg ~sr ~vdi1 ~vdi2
		let add_to_sm_config context ~dbg ~sr ~vdi ~key =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.add_to_sm_config ~dbg ~sr ~vdi ~key
		let remove_from_sm_config context ~dbg ~sr ~vdi ~key =
			let module C = Client(struct let rpc = of_sr sr end) in
			C.VDI.remove_from_sm_config ~dbg ~sr ~vdi ~key
        let get_url context ~dbg ~sr ~vdi =
            let module C = Client(struct let rpc = of_sr sr end) in
            C.VDI.get_url ~dbg ~sr ~vdi

	end

    let get_by_name context ~dbg ~name =
        (* Assume it has either the format:
           SR/VDI -- for a particular SR and VDI
           content_id -- for a particular content *)
        let open Stringext in
        match List.filter (fun x -> x <> "") (String.split ~limit:2 '/' name) with
            | [ sr; name ] ->
                let module C = Client(struct let rpc = of_sr sr end) in
                sr, C.VDI.get_by_name ~dbg ~sr ~name
            | [ name ] ->
                (match success_or choose (multicast (fun sr rpc ->
                    let module C = Client(struct let rpc = of_sr sr end) in
                    sr, C.VDI.get_by_name ~dbg ~sr ~name
                )) with SMSuccess x -> x
					| SMFailure e -> raise e)
            | _ ->
                raise (Vdi_does_not_exist name)

	module DATA = struct
		let copy context = Storage_migrate.copy
        let copy_into context = Storage_migrate.copy_into

		module MIRROR = struct
			let start context = Storage_migrate.start
			let stop context = Storage_migrate.stop
			let list context = Storage_migrate.list
			let stat context = Storage_migrate.stat
			let receive_start context = Storage_migrate.receive_start
			let receive_finalize context = Storage_migrate.receive_finalize
			let receive_cancel context = Storage_migrate.receive_cancel
		end	
	end

	module Policy = struct
		let get_backend_vm context ~dbg ~vm ~sr ~vdi =
			if not(Hashtbl.mem plugins sr) then begin
				error "No registered plugin for sr = %s" sr;
				raise (No_storage_plugin_for_sr sr)				
			end else (Hashtbl.find plugins sr).backend_domain
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
