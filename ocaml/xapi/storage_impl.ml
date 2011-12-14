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

(** Notes on failure handling:
    FH1: we always perform SMAPI "side-effects" before
         persisting our state to disk. If the process restarts unexpectedly we
         will attempt to execute the same side-effect twice: this is better than
         zero times.
         Implication: the SMAPI "side-effects" must be idempotent functions.
    FH2: when destroying a "datapath" and a "side-effect" fails, we consider that
         the datapath has "leaked". We remember these, on the assumption that
         the client does not. Before each operation on affected VDIs, we try to
         re-destroy the "leaked" datapath.
         Therefore: if a VDI.detach fails, then subsequent VDI.attaches will try
         to re-perform the VDI.detach, rather than immediately fail with "VDI
         cannot be attached R/W since it is already attached R/O"
    FH3: whenever we detect a leak, it will have a corresponding Error record.

    Notes on each SMAPI call:
    SR.attach:
        if SMAPI sr_attach fails: Failure returned; user should fix problem and retry
    SR.detach:
        if SMAPI vdi_detach* fails: Failures ignored
        if SMAPI sr_detach fails: Failure returned; user should fix problem and retry
        provided sr_detach succeeds then the SR (and VDIs) are forgotten
    VDI.attach, VDI.activate, VDI.deactivate, VDI.detach:
        if VDI has associated leaked datapath, another attempt is made to remove
        if SMAPI calls fails: Failure returned; user should fix problem and retry
        -- in practice the user will call Datapath.destroy's cleanup coode
    DP.destroy:
        if SMAPI vdi_detach* fails: Datapath is "leaked", failure returned
        -- in practice the user will not call this again and rely on SR.detach /
           further VDI operations to clear the problem
*)

(* Possible improvements:
   Always block attach depending on the VDI.read_only not the VBD.mode: this prevents
   the bad pygrub stuck state and we can delete FH2. This would be safer than explicitlly
   leaking an active device node. Although blkback does respect the "mode=r" flag, this
   might have unexpected consequences for the snapshot attach provisioning code. *)

(** Notes on locking:
    We have the following locks:
      SR_1, SR_2, ... SR_n: one lock per SR for serialising SR.attach/SR.detach per-SR
      VDI_1_a, VDI_1, b, ... VDI_n_z: one lock per VDI for serialising all VDI.* ops per-SR
      + various locks to protect accesses to individual tables

	We hold locks in one of the following sequences:
      VDI_p_q : for a VDI operation
      SR_p    : for an SR.attach
      SR_p, VDI_p_a, VDI_p_b, ..., VDI_p_z : for an SR.detach (to "quiesce" the SR)
*)

open Threadext
open Pervasiveext
open Fun
open Storage_interface

let print_debug = ref false
let log_to_stdout prefix (fmt: ('a , unit, string, unit) format4) =
	let time_of_float x =
		let time = Unix.gmtime x in
		Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
			(time.Unix.tm_year+1900)
			(time.Unix.tm_mon+1)
			time.Unix.tm_mday
			time.Unix.tm_hour
			time.Unix.tm_min
			time.Unix.tm_sec in
	Printf.kprintf
		(fun s ->
			Printf.printf "%s %s %s\n" (time_of_float (Unix.gettimeofday ())) prefix s;
			flush stdout) fmt

module D=Debug.Debugger(struct let name="storage_impl" end)
let debug (fmt: ('a, unit, string, unit) format4) = if !print_debug then log_to_stdout "debug" fmt else D.debug fmt
let error (fmt: ('a, unit, string, unit) format4) = if !print_debug then log_to_stdout "error" fmt else D.error fmt
let info  (fmt: ('a, unit, string, unit) format4) = if !print_debug then log_to_stdout "info" fmt else D.info fmt

let host_state_path = ref "/var/run/xapi/storage.db"

module Dp = struct
	type t = string with rpc
	let make username = username
end

let indent x = "    " ^ x

let string_of_date x = Date.to_string (Date.of_float x)

module Vdi = struct
	(** Represents the information known about a VDI *)
	type t = {
		params: params option;    (** Some path when attached; None otherwise *)
		dps: (Dp.t * Vdi_automaton.state) list; (** state of the VDI from each dp's PoV *)
		leaked: Dp.t list;                        (** "leaked" dps *)
	} with rpc
	let empty () = {
		params = None;
		dps = [];
		leaked = [];
	}
	(** [superstate x] returns the actual state of the backing VDI by finding the "max" of
	    the states from the clients' PsoV *)
	let superstate x = Vdi_automaton.superstate (List.map snd x.dps)

	let get_dp_state dp t =
		if List.mem_assoc dp t.dps
		then List.assoc dp t.dps
		else Vdi_automaton.Detached

	let set_dp_state dp state t =
		let rest = List.filter (fun (u, _) -> u <> dp) t.dps in
		{ t with dps = if state = Vdi_automaton.Detached then rest else (dp, state) :: rest }

	let get_leaked t = t.leaked

    let leaked t (x: Dp.t) = List.mem x t.leaked
    let all _ _ = true

	let remove_leaked dp t =
		{ t with leaked = List.filter (fun u -> u <> dp) t.leaked }

	let add_leaked dp t =
		let t' = remove_leaked dp t in
		{ t' with leaked = dp :: t'.leaked }

	let dps t = List.map fst t.dps

	(** [perform dp op t] updates VDI [t] given the request to perform [op] by [dp] *)
	let perform dp op t =
		let state = get_dp_state dp t in
		let state' = Vdi_automaton.(+) state op in
		set_dp_state dp state' t

	let to_string_list x =
		let title = Printf.sprintf "%s (device=%s)" (Vdi_automaton.string_of_state (superstate x)) (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) x.params)) in
		let of_dp (dp, state) = Printf.sprintf "DP: %s: %s%s" dp (Vdi_automaton.string_of_state state) (if List.mem dp x.leaked then "  ** LEAKED" else "") in
		title :: (List.map indent (List.map of_dp x.dps))
end

module Sr = struct
	(** Represents the state of an SR *)
	type t = {
		vdis: (string, Vdi.t) Hashtbl.t; (** All tracked VDIs *)
	} with rpc

	let empty () = {
		vdis = Hashtbl.create 10;
	}
	let m = Mutex.create ()
	let find vdi sr = Mutex.execute m (fun () -> try Some (Hashtbl.find sr.vdis vdi) with Not_found -> None)
	let replace vdi vdi_t sr =
		Mutex.execute m (fun () -> Hashtbl.replace sr.vdis vdi vdi_t)
	let list sr = Mutex.execute m (fun () -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) sr.vdis [])
	let remove vdi sr =
		Mutex.execute m (fun () -> Hashtbl.remove sr.vdis vdi)

	let to_string_list x =
		Hashtbl.fold (fun vdi vdi_t acc-> (Printf.sprintf "VDI %s" vdi :: (List.map indent (Vdi.to_string_list vdi_t))) @ acc) x.vdis []
end

module Host = struct
	(** Represents the state of a host *)
	type t = {
		srs: (string, Sr.t) Hashtbl.t;
	} with rpc

	let empty () = {
		srs = Hashtbl.create 10
	}
	let m = Mutex.create ()
	let find sr h = Mutex.execute m (fun () -> try Some (Hashtbl.find h.srs sr) with Not_found -> None)
	let remove sr h = Mutex.execute m (fun () -> Hashtbl.remove h.srs sr)
	let replace sr sr_t h = Mutex.execute m (fun () -> Hashtbl.replace h.srs sr sr_t)
	let list h = Mutex.execute m (fun () -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) h.srs [])

	(** All global state held here *)
	let host = ref (empty ())
end

module Errors = struct
	(** Used for remembering the last [max] errors *)
	type error = {
		dp: string; (** person who triggered the error *)
		time: float;        (** time the error happened *)
		sr: string;
		vdi: string;
		error: string
	} with rpc

	type t = error list with rpc

	let max = 100
	let errors = ref []
	let errors_m = Mutex.create ()
	let add dp sr vdi code =
		Mutex.execute errors_m
			(fun () ->
				let t = {
					dp = dp;
					time = Unix.gettimeofday ();
					sr = sr; vdi = vdi; error = code
				} in
				errors := Listext.List.take 100 (t :: !errors)
			)
	let list () = Mutex.execute errors_m (fun () -> !errors)
	let to_string x =
		Printf.sprintf "%s @ %s; sr:%s vdi:%s error:%s" x.dp
			(string_of_date x.time) x.sr x.vdi x.error
end

module Everything = struct
	type t = {
		host: Host.t;
		errors: Errors.t;
	} with rpc

	let make () = { host = !Host.host; errors = !Errors.errors }

	let to_file filename h =
		let rpc = Mutex.execute Host.m (fun () -> rpc_of_t h) in
		let s = Jsonrpc.to_string rpc in
		Unixext.write_string_to_file filename s
	let of_file filename =
		let s = Unixext.string_of_file filename in
		let rpc = Jsonrpc.of_string s in
		t_of_rpc rpc

	let set h = Host.host := h.host; Errors.errors := h.errors
end

module Wrapper = functor(Impl: Server_impl) -> struct
	type context = Smint.request

	let expect_unit dp sr vdi x = match x with
		| Success Unit -> ()
		| x -> Errors.add dp sr vdi (string_of_result x)

	let query = Impl.query

	module VDI = struct
		type vdi_locks = (string, unit) Storage_locks.t

		(** Map of SR name to vdi_locks table *)
		let locks : (string, vdi_locks) Hashtbl.t = Hashtbl.create 10

		(* This protects the 'locks' table only *)
		let locks_m = Mutex.create ()
		let locks_find sr =
			Mutex.execute locks_m
				(fun () ->
					if not(Hashtbl.mem locks sr)
					then
						let result = Storage_locks.make () in
						Hashtbl.replace locks sr result;
						result
					else Hashtbl.find locks sr)
		let locks_remove sr =
			Mutex.execute locks_m (fun () -> Hashtbl.remove locks sr)

		let with_vdi sr vdi f =
			let locks = locks_find sr in
			Storage_locks.with_instance_lock locks vdi f

		let with_all_vdis sr f =
			let locks = locks_find sr in
			Storage_locks.with_master_lock locks f

		let side_effects context task dp sr sr_t vdi vdi_t ops =
			let perform_one (_, vdi_t) (op, state_on_fail) =
				let result, vdi_t =
					try
						match op with
						| Vdi_automaton.Nothing -> Success Unit, vdi_t
						| Vdi_automaton.Attach ro_rw ->
							let read_write = (ro_rw = Vdi_automaton.RW) in
							let result = Impl.VDI.attach context ~task ~dp ~sr ~vdi ~read_write in
							let result, vdi_t = match result with
							| Success (Params x) ->
								result, { vdi_t with Vdi.params = Some x }
							| Success Unit
							| Failure _ ->
								result, vdi_t
							| Success (Stat _ | Vdi _ | Vdis _ | String _)->
								Failure (Internal_error (Printf.sprintf "VDI.attach type error, received: %s" (string_of_result result))), vdi_t in
								result, vdi_t
							| Vdi_automaton.Activate ->
								Impl.VDI.activate context ~task ~dp ~sr ~vdi, vdi_t
							| Vdi_automaton.Deactivate ->
								Impl.VDI.deactivate context ~task ~dp ~sr ~vdi, vdi_t
							| Vdi_automaton.Detach ->
								Impl.VDI.detach context ~task ~dp ~sr ~vdi, vdi_t
					with e ->
						error "dp:%s sr:%s vdi:%s op:%s error:%s backtrace:%s" dp sr vdi
							(Vdi_automaton.string_of_op op) (Printexc.to_string e) (Printexc.get_backtrace ());
						Failure (Internal_error (Printexc.to_string e)), vdi_t in
				(* If the side-effects fail then we drive the dp state back to state_on_fail *)
				let vdi_t =
					if not(success result)
					then Vdi.set_dp_state dp state_on_fail vdi_t
					else vdi_t in
				result, vdi_t in

			List.fold_left perform_one (Success Unit, vdi_t) ops

		let perform_nolock context ~task ~dp ~sr ~vdi this_op =
			match Host.find sr !Host.host with
			| None -> Failure Sr_not_attached, Vdi.empty ()
			| Some sr_t ->
				let vdi_t = Opt.default (Vdi.empty ()) (Sr.find vdi sr_t) in
				let result, vdi_t = 
				try
					(* Compute the overall state ('superstate') of the VDI *)
					let superstate = Vdi.superstate vdi_t in
					(* We first assume the operation succeeds and compute the new
					   datapath+VDI state *)
					let vdi_t = Vdi.perform (Dp.make dp) this_op vdi_t in
					(* Compute the new overall state ('superstate') *)
					let superstate' = Vdi.superstate vdi_t in
					(* Compute the real operations which would drive the system from
					   superstate to superstate'. These may fail: if so we revert the
					   datapath+VDI state to the most appropriate value. *)
					let ops = Vdi_automaton.(-) superstate superstate' in
					let result, vdi_t = side_effects context task dp sr sr_t vdi vdi_t ops in
					(* NB this_op = attach but ops = [] because the disk is already attached *)
					let result = match result, this_op with
						| Success _, Vdi_automaton.Attach _ ->
							Success (Params (Opt.unbox vdi_t.Vdi.params))
						| x, _ -> x in
					
					result, vdi_t
				with Vdi_automaton.No_operation(a, b) ->
					let result = Failure (Illegal_transition(a, b)) in
					result, vdi_t in

				if success result
				then Sr.replace vdi vdi_t sr_t
				else Errors.add dp sr vdi (string_of_result result);
				
				(* If the new VDi state is "detached" then we remove it from the table
				   altogether *)
				debug "task:%s dp:%s sr:%s vdi:%s superstate:%s" task dp sr vdi (Vdi_automaton.string_of_state (Vdi.superstate vdi_t));
				if Vdi.superstate vdi_t = Vdi_automaton.Detached
				then Sr.remove vdi sr_t;

				(* FH1: Perform the side-effect first: in the case of a failure half-way
				   through we would rather perform the side-effect twice than never at
				   all. *)
				Everything.to_file !host_state_path (Everything.make ());
				result, vdi_t

		(* Attempt to remove a possibly-active datapath associated with [vdi] *)
		let destroy_datapath_nolock context ~task ~dp ~sr ~vdi ~allow_leak =
			match Host.find sr !Host.host with
			| None -> Failure Sr_not_attached
			| Some sr_t ->
				begin match Sr.find vdi sr_t with
				| Some vdi_t ->
					let current_state = Vdi.get_dp_state dp vdi_t in
					let desired_state = Vdi_automaton.Detached in
					let ops = List.map fst (Vdi_automaton.(-) current_state desired_state) in
					let result, vdi_t =
						List.fold_left (fun (result, vdi_t) op ->
							if success result
							then perform_nolock context ~task ~dp ~sr ~vdi op
							else result, vdi_t
						) (Success Unit, vdi_t) ops in

					(* FH3: register this dp as having leaked the VDI *)
					let vdi_t = match success result, allow_leak with
					| true, _ -> vdi_t (* it worked fine *)
					| false, false -> Vdi.add_leaked dp vdi_t
					| false, true ->
						(* allow_leak means we can forget this dp *)
						info "setting dp:%s state to %s, even though operation failed because allow_leak set" dp (Vdi_automaton.string_of_state desired_state);
						Vdi.set_dp_state dp desired_state vdi_t in

						if Vdi.superstate vdi_t = Vdi_automaton.Detached
						then Sr.remove vdi sr_t
						else Sr.replace vdi vdi_t sr_t;

						Everything.to_file !host_state_path (Everything.make ());
						result
					| None -> Success Unit
				end

		(* Attempt to clear leaked datapaths associed with this vdi *)
		let remove_datapaths_andthen_nolock context ~task ~sr ~vdi which next =
			let dps = match Host.find sr !Host.host with
			| None -> []
			| Some sr_t ->
				begin match Sr.find vdi sr_t with
				| Some vdi_t ->
					List.filter (which vdi_t) (Vdi.dps vdi_t)
				| None -> []
				end in
			let failures = List.fold_left (fun acc dp ->
				info "Attempting to destroy datapath dp:%s sr:%s vdi:%s" dp sr vdi;
				match destroy_datapath_nolock context ~task ~dp ~sr ~vdi ~allow_leak:false with
				| Success _ -> acc
				| Failure f -> f :: acc
			) [] dps in
			match failures with
			| [] -> next ()
			| f :: fs -> Failure f

		let attach context ~task ~dp ~sr ~vdi ~read_write =
			info "VDI.attach task:%s dp:%s sr:%s vdi:%s read_write:%b" task dp sr vdi read_write;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~task ~sr ~vdi Vdi.leaked
						(fun () ->
							fst(perform_nolock context ~task ~dp ~sr ~vdi
								(Vdi_automaton.Attach (if read_write then Vdi_automaton.RW else Vdi_automaton.RO)))))
		let activate context ~task ~dp ~sr ~vdi =
			info "VDI.activate task:%s dp:%s sr:%s vdi:%s" task dp sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~task ~sr ~vdi Vdi.leaked
						(fun () ->
							fst(perform_nolock context ~task ~dp ~sr ~vdi Vdi_automaton.Activate)))

		let stat context ~task ~sr ~vdi () =
			info "VDI.stat task:%s sr:%s vdi:%s" task sr vdi;
			with_vdi sr vdi
				(fun () ->
					match Host.find sr !Host.host with
					| None -> Failure Sr_not_attached
					| Some sr_t ->
						let vdi_t = Opt.default (Vdi.empty ()) (Sr.find vdi sr_t) in
						Success (Stat {
							superstate = Vdi.superstate vdi_t;
							dps = List.map (fun dp -> dp, Vdi.get_dp_state dp vdi_t) (Vdi.dps vdi_t)
						})
				)

		let deactivate context ~task ~dp ~sr ~vdi =
			info "VDI.deactivate task:%s dp:%s sr:%s vdi:%s" task dp sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~task ~sr ~vdi Vdi.leaked
						(fun () ->
							fst (perform_nolock context ~task ~dp ~sr ~vdi Vdi_automaton.Deactivate)))
		let detach context ~task ~dp ~sr ~vdi =
			info "VDI.detach task:%s dp:%s sr:%s vdi:%s" task dp sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~task ~sr ~vdi Vdi.leaked
						(fun () ->
							fst (perform_nolock context ~task ~dp ~sr ~vdi Vdi_automaton.Detach)))

        let create context ~task ~sr ~vdi_info ~params =
            info "VDI.create task:%s sr:%s vdi_info:%s params:%s" task sr (string_of_vdi_info vdi_info) (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) params));
            let result = Impl.VDI.create context ~task ~sr ~vdi_info ~params in
            match result with
                | Success (Vdi { virtual_size = virtual_size' }) when virtual_size' < vdi_info.virtual_size ->
                    error "VDI.create task:%s created a smaller VDI (%Ld)" task virtual_size';
                    Failure(Backend_error("SR_BACKEND_FAILURE", ["Disk too small"; Int64.to_string vdi_info.virtual_size; Int64.to_string virtual_size']))
                | result -> result

        let destroy context ~task ~sr ~vdi =
            info "VDI.destroy task:%s sr:%s vdi:%s" task sr vdi;
            with_vdi sr vdi
                (fun () ->
                    remove_datapaths_andthen_nolock context ~task ~sr ~vdi Vdi.all
                        (fun () ->
                            Impl.VDI.destroy context ~task ~sr ~vdi
                        )
                )

	end

	module DP = struct
		let create context ~task ~id = id

		(** [destroy_sr context dp sr allow_leak vdi_already_locked] attempts to free
		    the resources associated with [dp] in [sr]. If [vdi_already_locked] then
		    it is assumed that all VDIs are already locked. *)
		let destroy_sr context ~task ~dp ~sr ~allow_leak vdi_already_locked =
			(* Every VDI in use by this session should be detached and deactivated *)
			match Host.find sr !Host.host with
			| None -> [ Sr_not_attached ]
			| Some sr_t ->
				let vdis = Sr.list sr_t in
				List.fold_left (fun acc (vdi, vdi_t) ->
					let locker =
						if vdi_already_locked
						then fun f -> f ()
						else VDI.with_vdi sr vdi in
					match (locker
						(fun () ->
							VDI.destroy_datapath_nolock context ~task ~dp ~sr ~vdi ~allow_leak
					)) with
					| Success _ -> acc
					| Failure f -> f :: acc
				) [] vdis


		let destroy context ~task ~dp ~allow_leak =
			info "DP.destroy task:%s dp:%s allow_leak:%b" task dp allow_leak;
			let failures = List.fold_left (fun acc (sr, _) -> acc @ (destroy_sr context ~task ~dp ~sr ~allow_leak false)) [] (Host.list !Host.host) in
			match failures, allow_leak with
			| [], _  -> Success Unit
			| f :: _, false ->
				error "Leaked datapath: dp: %s" dp;
				Failure f
			| _ :: _, true ->
				info "Forgetting leaked datapath: dp: %s" dp;
				Success Unit

		let diagnostics context () =
			let srs = Host.list !Host.host in
			let of_sr (sr, sr_t) =
				let title = Printf.sprintf "SR %s" sr in
				title :: (List.map indent (Sr.to_string_list sr_t)) in
			let srs = List.concat (List.map of_sr srs) in
			let errors = List.map Errors.to_string (Errors.list ()) in
			let errors = (if errors <> [] then "The following errors have been logged:" else "No errors have been logged.") :: errors in
			let lines = [ "The following SRs are attached:" ] @ (List.map indent srs) @ [ "" ] @ errors in
			Success (String (String.concat "" (List.map (fun x -> x ^ "\n") lines)))
	end

	module SR = struct
		let locks : (string, unit) Storage_locks.t = Storage_locks.make ()
		let with_sr = Storage_locks.with_instance_lock locks

		let list context ~task =
			List.map fst (Host.list !Host.host)

		let scan context ~task ~sr =
			info "SR.scan task:%s sr:%s" task sr;
			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
						| None -> Failure Sr_not_attached
						| Some _ ->
							Impl.SR.scan context ~task ~sr
				)

		let attach context ~task ~sr ~device_config =
			info "SR.attach task:%s sr:%s device_config:[%s]" task sr (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) device_config));
			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
					| None ->
						begin match Impl.SR.attach context ~task ~sr ~device_config with
						| Success Unit ->
							Host.replace sr (Sr.empty ()) !Host.host;
							(* FH1: Perform the side-effect first: in the case of a
							   failure half-way through we would rather perform the
							   side-effect twice than never at all. *)
							Everything.to_file !host_state_path (Everything.make ());
							Success Unit
						| Success _ ->
							Failure (Internal_error "SR.attach received a non-unit")
						| Failure _ as x -> x
						end
					| Some _ ->
						(* Operation is idempotent *)
						Success Unit
				)

		let detach_destroy_common context ~task ~sr f =
			let active_dps sr_t =
				(* Enumerate all active datapaths *)
				List.concat (List.map (fun (_, vdi_t) -> Vdi.dps vdi_t) (Sr.list sr_t)) in

			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
					| None -> Failure Sr_not_attached
					| Some sr_t ->
						VDI.with_all_vdis sr
							(fun () ->
								let dps = active_dps sr_t in
								List.iter
									(fun dp ->
										let (_: failure_t list) =
											DP.destroy_sr context ~task ~dp ~sr ~allow_leak:false true in
										()
									) dps;
								let dps = active_dps sr_t in
								if dps <> []
								then error "The following datapaths have leaked: %s" (String.concat "; " dps);
								begin match f context ~task ~sr with
								| Success Unit ->
									Host.remove sr !Host.host;
									Everything.to_file !host_state_path (Everything.make ());
									VDI.locks_remove sr;
									Success Unit
								| Success _ ->
									Failure (Internal_error "SR detach/destroy received a non-unit")
								| Failure _ as x -> x
								end
							)
				)
		let detach context ~task ~sr =
			info "SR.detach task:%s sr:%s" task sr;
			detach_destroy_common context ~task ~sr Impl.SR.detach

		let reset context ~task ~sr =
			info "SR.reset task:%s sr:%s" task sr;
			with_sr sr
				(fun () ->
					Host.remove sr !Host.host;
					Everything.to_file !host_state_path (Everything.make ());
					VDI.locks_remove sr;
					Success Unit			
				)

		let destroy context ~task ~sr = 
			info "SR.destroy task:%s sr:%s" task sr;
			detach_destroy_common context ~task ~sr Impl.SR.destroy			
	end
end

let initialise () =
	Unixext.mkdir_safe (Filename.dirname !host_state_path) 0o700;
	if Sys.file_exists !host_state_path then begin
		info "Loading storage state from: %s" !host_state_path;
		try
			let state = Everything.of_file !host_state_path in
			Everything.set state
		with e ->
			error "Failed to load storage state from: %s; creating blank database (error: %s)" !host_state_path (Printexc.to_string e)
	end else info "No storage state is persisted in %s; creating blank database" !host_state_path

module Local_domain_socket = struct
	(** Code to create a standalone process listening on a Unix domain socket. *)
	let server = Http_svr.Server.empty ()

	let socket = ref None

	let path = Filename.concat Fhs.vardir "storage"

	let xmlrpc_handler process req bio _ =
		let body = Http_svr.read_body req bio in
		let s = Buf_io.fd_of bio in
		let rpc = Xmlrpc.call_of_string body in
		(* Printf.fprintf stderr "Request: %s %s\n%!" rpc.Rpc.name (Rpc.to_string (List.hd rpc.Rpc.params)); *)
		let result = process (Some req.Http.Request.uri) rpc in
		(* Printf.fprintf stderr "Response: %s\n%!" (Rpc.to_string result.Rpc.contents); *)
		let str = Xmlrpc.string_of_response result in
		Http_svr.response_str req s str

	let start path process =
		Http_svr.Server.add_handler server Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));
		Unixext.mkdir_safe (Filename.dirname path) 0o700;
		Unixext.unlink_safe path;
		let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(path)) "storage_unix" in
		Http_svr.start server domain_sock;
		socket := Some(domain_sock)

	let shutdown () =
		Opt.iter Http_svr.stop !socket;
		socket := None
end
