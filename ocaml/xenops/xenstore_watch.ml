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

open Fun
open Xenstore
open Xenops_helpers
open Threadext

module D = Debug.Debugger(struct let name = "xenstore_watch" end)
open D

let _introduceDomain = "@introduceDomain"
let _releaseDomain = "@releaseDomain"

module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

module type WATCH_ACTIONS = sig
	val all_domU_watches : int -> string -> string list
	val watches_of_device : Device_common.device -> string list
	val watch_fired : Xenstore.Xs.xsh -> string -> Xenctrl.Domain_info.t IntMap.t -> Device_common.device list IntMap.t -> (int -> unit) -> unit
	val unmanaged_domain : int -> string -> bool
	val found_running_domain : int -> string -> unit
end

module WatchXenstore = functor(Actions: WATCH_ACTIONS) -> struct

	let list_domains xc =
		let dis = Xenctrl.domain_getinfolist xc 0 in
		let ids = List.map (fun x -> x.Xenctrl.Domain_info.domid) dis in
		List.fold_left (fun map (k, v) -> IntMap.add k v map) IntMap.empty (List.combine ids dis)

	let domain_looks_different a b = match a, b with
		| None, Some _ -> true
		| Some _, None -> true
		| None, None -> false
		| Some a', Some b' ->
			let open Xenctrl.Domain_info in
			a'.shutdown <> b'.shutdown
			|| (a'.shutdown && b'.shutdown && (a'.shutdown_code <> b'.shutdown_code))

	let list_different_domains a b =
		let c = IntMap.merge (fun _ a b -> if domain_looks_different a b then Some () else None) a b in
		List.map fst (IntMap.bindings c)

	let watch_xenstore () =
		with_xc_and_xs
			(fun xc xs ->
				let domains = ref IntMap.empty in
				let watches = ref IntMap.empty in
				let uuids = ref IntMap.empty in

				let watch path =
					debug "xenstore watch %s" path;
					xs.Xs.watch path path in

				let unwatch path =
					try
						debug "xenstore unwatch %s" path;
						xs.Xs.unwatch path path
					with Xenbus.Xb.Noent ->
						debug "xenstore unwatch %s threw Xb.Noent" path in

				let add_domU_watches xs domid uuid =
					debug "Adding watches for: domid %d" domid;
					List.iter watch (Actions.all_domU_watches domid uuid);
					uuids := IntMap.add domid uuid !uuids;
					watches := IntMap.add domid [] !watches in

				let remove_domU_watches xs domid =
					debug "Removing watches for: domid %d" domid;
					if IntMap.mem domid !uuids then begin
						let uuid = IntMap.find domid !uuids in
						List.iter unwatch (Actions.all_domU_watches domid uuid);
						List.iter (fun d ->
							List.iter unwatch (Actions.watches_of_device d)
						) (try IntMap.find domid !watches with Not_found -> []);
						watches := IntMap.remove domid !watches;
						uuids := IntMap.remove domid !uuids;
					end in

				let cancel_domU_operations xs domid =
					(* Anyone blocked on a domain/device operation which won't happen because the domain
					   just shutdown should be cancelled here. *)
					debug "Cancelling watches for: domid %d" domid;
					Cancel_utils.on_shutdown ~xs domid in

				let add_device_watch xs device =
					let open Device_common in
					debug "Adding watches for: %s" (string_of_device device);
					let domid = device.frontend.domid in
					List.iter watch (Actions.watches_of_device device);
					watches := IntMap.add domid (device :: (IntMap.find domid !watches)) !watches in

				let remove_device_watch xs device =
					let open Device_common in
					debug "Removing watches for: %s" (string_of_device device);
					let domid = device.frontend.domid in
					let current = IntMap.find domid !watches in
					List.iter unwatch (Actions.watches_of_device device);
					watches := IntMap.add domid (List.filter (fun x -> x <> device) current) !watches in

				let look_for_different_domains () =
					let domains' = list_domains xc in
					let different = list_different_domains !domains domains' in
					List.iter
						(fun domid ->
							let open Xenctrl.Domain_info in
							debug "Domain %d may have changed state" domid;
							(* The uuid is either in the new domains map or the old map. *)
							let di = IntMap.find domid (if IntMap.mem domid domains' then domains' else !domains) in
							let id = Uuid.uuid_of_int_array di.handle |> Uuid.string_of_uuid in
							if Actions.unmanaged_domain domid id
							then begin
								debug "However domain %d is not managed by us: ignoring" domid;
								if IntMap.mem domid !uuids then begin
									debug "Cleaning-up the remaining watches for: domid %d" domid;
									cancel_domU_operations xs domid;
									remove_domU_watches xs domid;
								end;
							end else begin
								Actions.found_running_domain domid id;
								(* A domain is 'running' if we know it has not shutdown *)
								let running = IntMap.mem domid domains' && (not (IntMap.find domid domains').shutdown) in
								match IntMap.mem domid !watches, running with
									| true, true -> () (* still running, nothing to do *)
									| false, false -> () (* still offline, nothing to do *)
									| false, true ->
										add_domU_watches xs domid id
									| true, false ->
										cancel_domU_operations xs domid;
										remove_domU_watches xs domid
							end
						) different;
					domains := domains' in

				let look_for_different_devices domid =
					if not(IntMap.mem domid !watches)
					then debug "Ignoring frontend device watch on unmanaged domain: %d" domid
					else begin
						let devices = IntMap.find domid !watches in
						let devices' = Device_common.list_frontends ~xs domid in
						let old_devices = Listext.List.set_difference devices devices' in
						let new_devices = Listext.List.set_difference devices' devices in
						List.iter (add_device_watch xs) new_devices;
						List.iter (remove_device_watch xs) old_devices;
					end in

				xs.Xs.watch _introduceDomain "";
				xs.Xs.watch _releaseDomain "";
				look_for_different_domains ();

				let open Xenctrl.Domain_info in

				while true do
					let path, _ =
						if Xs.has_watchevents xs
						then Xs.get_watchevent xs
						else Xs.read_watchevent xs in
					if path = _introduceDomain || path = _releaseDomain
					then look_for_different_domains ()
					else Actions.watch_fired xs path !domains !watches look_for_different_devices
				done
			)

	let create_watcher_thread () =
		Thread.create
			(fun () ->
				while true do
					begin
						try
							Debug.with_thread_associated "xenstore" (watch_xenstore) ();
							debug "watch_xenstore thread exitted"
						with e ->
							debug "watch_xenstore thread raised: %s" (Printexc.to_string e);
							debug "watch_xenstore thread backtrace: %s" (Printexc.get_backtrace ())
					end;
					Thread.delay 5.
				done
			) ()
end
