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
open Storage_task

let print_debug = ref false
let log_to_stdout prefix (fmt: ('a , unit, string, unit) format4) =
	let time_of_float x =
		let time = Unix.gmtime x in
		Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ (%d)"
			(time.Unix.tm_year+1900)
			(time.Unix.tm_mon+1)
			time.Unix.tm_mday
			time.Unix.tm_hour
			time.Unix.tm_min
			time.Unix.tm_sec
	        (Thread.id (Thread.self ()))
	in
	Printf.kprintf
		(fun s ->
			Printf.printf "%s %s %s\n" (time_of_float (Unix.gettimeofday ())) prefix s;
			flush stdout) fmt

module D=Debug.Debugger(struct let name="storage_impl" end)
let debug (fmt: ('a, unit, string, unit) format4) = if !print_debug then log_to_stdout "debug" fmt else D.debug fmt
let error (fmt: ('a, unit, string, unit) format4) = if !print_debug then log_to_stdout "error" fmt else D.error fmt
let info  (fmt: ('a, unit, string, unit) format4) = if !print_debug then log_to_stdout "info" fmt else D.info fmt

let host_state_path = ref "/var/run/nonpersistent/xapi/storage.db"

module Dp = struct
	type t = string with rpc
	let make username = username
end

let indent x = "    " ^ x

let string_of_date x = Date.to_string (Date.of_float x)

module Vdi = struct
	(** Represents the information known about a VDI *)
	type t = {
		attach_info :  attach_info option;    (** Some path when attached; None otherwise *)
		dps: (Dp.t * Vdi_automaton.state) list; (** state of the VDI from each dp's PoV *)
		leaked: Dp.t list;                        (** "leaked" dps *)
	} with rpc
	let empty () = {
		attach_info = None;
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
		let title = Printf.sprintf "%s (device=%s)" (Vdi_automaton.string_of_state (superstate x)) (Opt.default "None" (Opt.map (fun x -> "Some " ^ Jsonrpc.to_string (rpc_of_attach_info x)) x.attach_info)) in
		let of_dp (dp, state) = Printf.sprintf "DP: %s: %s%s" dp (Vdi_automaton.string_of_state state) (if List.mem dp x.leaked then "  ** LEAKED" else "") in
		title :: (List.map indent (List.map of_dp x.dps))
end

module Sr = struct
	(** Represents the state of an SR *)
	type vdis = (string, Vdi.t) Hashtbl.t with rpc

	type t = {
		vdis: vdis; (** All tracked VDIs *)
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

	module Query = struct
		let query = Impl.Query.query
		let diagnostics = Impl.Query.diagnostics
	end

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

		let side_effects context dbg dp sr sr_t vdi vdi_t ops =
			let perform_one vdi_t (op, state_on_fail) =
				try
					let vdi_t = Vdi.perform (Dp.make dp) op vdi_t in
					let new_vdi_t = match op with
						| Vdi_automaton.Nothing -> vdi_t
						| Vdi_automaton.Attach ro_rw ->
							let read_write = (ro_rw = Vdi_automaton.RW) in
							let x = Impl.VDI.attach context ~dbg ~dp ~sr ~vdi ~read_write in
							{ vdi_t with Vdi.attach_info = Some x }	
						| Vdi_automaton.Activate ->
							Impl.VDI.activate context ~dbg ~dp ~sr ~vdi; vdi_t
						| Vdi_automaton.Deactivate ->
							Storage_migrate.pre_deactivate_hook ~dbg ~dp ~sr ~vdi;
							Impl.VDI.deactivate context ~dbg ~dp ~sr ~vdi; vdi_t
						| Vdi_automaton.Detach ->
							Impl.VDI.detach context ~dbg ~dp ~sr ~vdi;
							Storage_migrate.post_detach_hook ~sr ~vdi ~dp;
							vdi_t
					in
					Sr.replace vdi new_vdi_t sr_t;
					new_vdi_t
				with e ->
					error "Storage_impl: dp:%s sr:%s vdi:%s op:%s error:%s backtrace:%s" dp sr vdi
						(Vdi_automaton.string_of_op op) (Printexc.to_string e) (Printexc.get_backtrace ());
					raise e
			in
			List.fold_left perform_one vdi_t ops

		let perform_nolock context ~dbg ~dp ~sr ~vdi this_op =
			match Host.find sr !Host.host with
			| None -> raise (Sr_not_attached sr)
			| Some sr_t ->
				let vdi_t = Opt.default (Vdi.empty ()) (Sr.find vdi sr_t) in
				let vdi_t' = 
					try
						(* Compute the overall state ('superstate') of the VDI *)
						let superstate = Vdi.superstate vdi_t in
						(* We first assume the operation succeeds and compute the new
						   datapath+VDI state *)
						let new_vdi_t = Vdi.perform (Dp.make dp) this_op vdi_t in
						(* Compute the new overall state ('superstate') *)
						let superstate' = Vdi.superstate new_vdi_t in
						(* Compute the real operations which would drive the system from
						   superstate to superstate'. These may fail: if so we revert the
						   datapath+VDI state to the most appropriate value. *)
						let ops = Vdi_automaton.(-) superstate superstate' in
						side_effects context dbg dp sr sr_t vdi vdi_t ops
					with e ->
						let e = match e with Vdi_automaton.No_operation(a, b) -> Illegal_transition(a,b) | e -> e in
						Errors.add dp sr vdi (Printexc.to_string e);
						raise e
				in

				(* Even if there were no side effects on the underlying VDI, we still need
				   to update the SR to update this DP's view of the state.
				   However if nothing changed (e.g. because this was the detach of a DP
				   which had not attached this VDI) then we won't need to update our on-disk state *)
				let vdi_t' = Vdi.perform (Dp.make dp) this_op vdi_t' in
				if vdi_t <> vdi_t' then begin
					Sr.replace vdi vdi_t' sr_t;
					(* If the new VDI state is "detached" then we remove it from the table
					   altogether *)
					debug "dbg:%s dp:%s sr:%s vdi:%s superstate:%s" dbg dp sr vdi (Vdi_automaton.string_of_state (Vdi.superstate vdi_t'));
					if Vdi.superstate vdi_t' = Vdi_automaton.Detached
					then Sr.remove vdi sr_t;

					(* FH1: Perform the side-effect first: in the case of a failure half-way
					   through we would rather perform the side-effect twice than never at
					   all. *)
					Everything.to_file !host_state_path (Everything.make ());
				end;
				vdi_t'

		(* Attempt to remove a possibly-active datapath associated with [vdi] *)
		let destroy_datapath_nolock context ~dbg ~dp ~sr ~vdi ~allow_leak =
			match Host.find sr !Host.host with
			| None -> raise (Sr_not_attached sr)
			| Some sr_t ->
				Opt.iter (fun vdi_t -> 
					let current_state = Vdi.get_dp_state dp vdi_t in
					let desired_state = Vdi_automaton.Detached in
					let ops = List.map fst (Vdi_automaton.(-) current_state desired_state) in
					begin 
						try
							ignore(List.fold_left (fun _ op ->
								perform_nolock context ~dbg ~dp ~sr ~vdi op
							) vdi_t ops)
						with e -> 
							if not allow_leak 
							then (ignore(Vdi.add_leaked dp vdi_t); raise e)
							else begin
								(* allow_leak means we can forget this dp *)
								info "setting dp:%s state to %s, even though operation failed because allow_leak set" dp (Vdi_automaton.string_of_state desired_state);
								let vdi_t = Vdi.set_dp_state dp desired_state vdi_t in
								
								if Vdi.superstate vdi_t = Vdi_automaton.Detached
								then Sr.remove vdi sr_t
								else Sr.replace vdi vdi_t sr_t;
								
								Everything.to_file !host_state_path (Everything.make ());
							end
					end) (Sr.find vdi sr_t)

		(* Attempt to clear leaked datapaths associed with this vdi *)
		let remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi which next =
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
				try 
					destroy_datapath_nolock context ~dbg ~dp ~sr ~vdi ~allow_leak:false;
					acc
				with e -> e :: acc
			) [] dps in
			match failures with
			| [] -> next ()
			| f :: fs -> raise f

		let epoch_begin context ~dbg ~sr ~vdi =
			info "VDI.epoch_begin dbg:%s sr:%s vdi:%s" dbg sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.leaked
						(fun () ->
							Impl.VDI.epoch_begin context ~dbg ~sr ~vdi
						))

		let attach context ~dbg ~dp ~sr ~vdi ~read_write =
			info "VDI.attach dbg:%s dp:%s sr:%s vdi:%s read_write:%b" dbg dp sr vdi read_write;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.leaked
						(fun () ->
							let state = perform_nolock context ~dbg ~dp ~sr ~vdi
								(Vdi_automaton.Attach (if read_write then Vdi_automaton.RW else Vdi_automaton.RO)) in
							Opt.unbox state.Vdi.attach_info							
						))

		let activate context ~dbg ~dp ~sr ~vdi =
			info "VDI.activate dbg:%s dp:%s sr:%s vdi:%s" dbg dp sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.leaked
						(fun () ->
							ignore(perform_nolock context ~dbg ~dp ~sr ~vdi Vdi_automaton.Activate)))

		let deactivate context ~dbg ~dp ~sr ~vdi =
			info "VDI.deactivate dbg:%s dp:%s sr:%s vdi:%s" dbg dp sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.leaked
						(fun () ->
							ignore (perform_nolock context ~dbg ~dp ~sr ~vdi Vdi_automaton.Deactivate)))

		let detach context ~dbg ~dp ~sr ~vdi =
			info "VDI.detach dbg:%s dp:%s sr:%s vdi:%s" dbg dp sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.leaked
						(fun () ->
							ignore (perform_nolock context ~dbg ~dp ~sr ~vdi Vdi_automaton.Detach)))

		let epoch_end context ~dbg ~sr ~vdi =
			info "VDI.epoch_end dbg:%s sr:%s vdi:%s" dbg sr vdi;
			with_vdi sr vdi
				(fun () ->
					remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.leaked
						(fun () ->
							Impl.VDI.epoch_end context ~dbg ~sr ~vdi
						))

        let create context ~dbg ~sr ~vdi_info =
            info "VDI.create dbg:%s sr:%s vdi_info:%s" dbg sr (string_of_vdi_info vdi_info);
            let result = Impl.VDI.create context ~dbg ~sr ~vdi_info in
            match result with
                | { virtual_size = virtual_size' } when virtual_size' < vdi_info.virtual_size ->
                    error "VDI.create dbg:%s created a smaller VDI (%Ld)" dbg virtual_size';
                    raise (Backend_error("SR_BACKEND_FAILURE", ["Disk too small"; Int64.to_string vdi_info.virtual_size; Int64.to_string virtual_size']))
                | result -> result

		let snapshot_and_clone call_name call_f context ~dbg ~sr ~vdi_info =
			info "%s dbg:%s sr:%s vdi_info:%s" call_name dbg sr (string_of_vdi_info vdi_info);
			with_vdi sr vdi_info.vdi
				(fun () ->
					call_f context ~dbg ~sr ~vdi_info
				)

		let snapshot = snapshot_and_clone "VDI.snapshot" Impl.VDI.snapshot
		let clone = snapshot_and_clone "VDI.clone" Impl.VDI.clone

        let destroy context ~dbg ~sr ~vdi =
            info "VDI.destroy dbg:%s sr:%s vdi:%s" dbg sr vdi;
            with_vdi sr vdi
                (fun () ->
                    remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.all
                        (fun () ->
                            Impl.VDI.destroy context ~dbg ~sr ~vdi
                        )
                )

		let stat context ~dbg ~sr ~vdi =
			info "VDI.stat dbg:%s sr:%s vdi:%s" dbg sr vdi;
			Impl.VDI.stat context ~dbg ~sr ~vdi

		let set_persistent context ~dbg ~sr ~vdi ~persistent =
			info "VDI.set_persistent dbg:%s sr:%s vdi:%s persistent:%b" dbg sr vdi persistent;
			with_vdi sr vdi
				(fun () ->
					Impl.VDI.set_persistent context ~dbg ~sr ~vdi ~persistent
				)

		let get_by_name context ~dbg ~sr ~name =
			info "VDI.get_by_name dbg:%s sr:%s name:%s" dbg sr name;
			Impl.VDI.get_by_name context ~dbg ~sr ~name

		let set_content_id context ~dbg ~sr ~vdi ~content_id =
			info "VDI.set_content_id dbg:%s sr:%s vdi:%s content_id:%s" dbg sr vdi content_id;
			Impl.VDI.set_content_id context ~dbg ~sr ~vdi ~content_id

		let similar_content context ~dbg ~sr ~vdi =
			info "VDI.similar_content dbg:%s sr:%s vdi:%s" dbg sr vdi;
			Impl.VDI.similar_content context ~dbg ~sr ~vdi

		let compose context ~dbg ~sr ~vdi1 ~vdi2 =
			info "VDI.compose dbg:%s sr:%s vdi1:%s vdi2:%s" dbg sr vdi1 vdi2;
			Impl.VDI.compose context ~dbg ~sr ~vdi1 ~vdi2

		let add_to_sm_config context ~dbg ~sr ~vdi ~key ~value =
			info "VDI.add_to_other_config dbg:%s sr:%s vdi:%s key:%s valu:%s" dbg sr vdi key value;
			Impl.VDI.add_to_sm_config context ~dbg ~sr ~vdi ~key ~value

		let remove_from_sm_config context ~dbg ~sr ~vdi ~key =
			info "VDI.remove_from_sm_config dbg:%s sr:%s vdi:%s key:%s" dbg sr vdi key;
			Impl.VDI.remove_from_sm_config context ~dbg ~sr ~vdi ~key

		let get_url context ~dbg ~sr ~vdi =
			info "VDI.get_url dbg:%s sr:%s vdi:%s" dbg sr vdi;
			Impl.VDI.get_url context ~dbg ~sr ~vdi

	end

	let get_by_name context ~dbg ~name =
		debug "get_by_name dbg:%s name:%s" dbg name;
		Impl.get_by_name context ~dbg ~name

	module DATA = struct
		let copy_into context ~dbg ~sr ~vdi ~url ~dest =
			info "DATA.copy_into dbg:%s sr:%s vdi:%s url:%s dest:%s" dbg sr vdi url dest;
			Impl.DATA.copy_into context ~dbg ~sr ~vdi ~url ~dest

		let copy context ~dbg ~sr ~vdi ~dp ~url ~dest =
			info "DATA.copy dbg:%s sr:%s vdi:%s url:%s dest:%s" dbg sr vdi url dest;
			Impl.DATA.copy context ~dbg ~sr ~vdi ~dp ~url ~dest

		module MIRROR = struct
			let start context ~dbg ~sr ~vdi ~dp ~url ~dest =
				info "DATA.MIRROR.start dbg:%s sr:%s vdi:%s url:%s dest:%s" dbg sr vdi url dest;
				Impl.DATA.MIRROR.start context ~dbg ~sr ~vdi ~dp ~url ~dest
					
			let stop context ~dbg ~id =
				info "DATA.MIRROR.stop dbg:%s id:%s" dbg id;
				Impl.DATA.MIRROR.stop context ~dbg ~id
					
			let list context ~dbg =
				info "DATA.MIRROR.active dbg:%s" dbg;
				Impl.DATA.MIRROR.list context ~dbg

			let stat context ~dbg ~id =
				info "DATA.MIRROR.stat dbg:%s id:%s" dbg id;
				Impl.DATA.MIRROR.stat context ~dbg ~id
					
			let receive_start context ~dbg ~sr ~vdi_info ~id ~similar =
				info "DATA.MIRROR.receive_start dbg:%s sr:%s id:%s similar:[%s]" 
					dbg sr id (String.concat "," similar);
				Impl.DATA.MIRROR.receive_start context ~dbg ~sr ~vdi_info ~id ~similar
					
			let receive_finalize context ~dbg ~id =
				info "DATA.MIRROR.receive_finalize dbg:%s id:%s" dbg id;
				Impl.DATA.MIRROR.receive_finalize context ~dbg ~id
					
			let receive_cancel context ~dbg ~id =
				info "DATA.MIRROR.receive_cancel dbg:%s id:%s" dbg id;
				Impl.DATA.MIRROR.receive_cancel context ~dbg ~id

		end
				
	end

	module DP = struct
		let create context ~dbg ~id = id

		(** [destroy_sr context dp sr allow_leak vdi_already_locked] attempts to free
		    the resources associated with [dp] in [sr]. If [vdi_already_locked] then
		    it is assumed that all VDIs are already locked. *)
		let destroy_sr context ~dbg ~dp ~sr ~allow_leak vdi_already_locked =
			(* Every VDI in use by this session should be detached and deactivated *)
			match Host.find sr !Host.host with
			| None -> [ Sr_not_attached sr ]
			| Some sr_t ->
				let vdis = Sr.list sr_t in
				List.fold_left (fun acc (vdi, vdi_t) ->
					let locker =
						if vdi_already_locked
						then fun f -> f ()
						else VDI.with_vdi sr vdi in
					locker
						(fun () ->
							try
								VDI.destroy_datapath_nolock context ~dbg ~dp ~sr ~vdi ~allow_leak;
								acc
							with e -> e::acc
						)) [] vdis


		let destroy context ~dbg ~dp ~allow_leak =
			info "DP.destroy dbg:%s dp:%s allow_leak:%b" dbg dp allow_leak;
			let failures = List.fold_left (fun acc (sr, _) -> acc @ (destroy_sr context ~dbg ~dp ~sr ~allow_leak false)) [] (Host.list !Host.host) in
			match failures, allow_leak with
			| [], _  -> ()
			| f :: _, false ->
				error "Leaked datapath: dp: %s" dp;
				raise f
			| _ :: _, true ->
				info "Forgetting leaked datapath: dp: %s" dp;
				()

		let diagnostics context () =
			let srs = Host.list !Host.host in
			let of_sr (sr, sr_t) =
				let title = Printf.sprintf "SR %s" sr in
				title :: (List.map indent (Sr.to_string_list sr_t)) in
			let srs = List.concat (List.map of_sr srs) in
			let errors = List.map Errors.to_string (Errors.list ()) in
			let errors = (if errors <> [] then "The following errors have been logged:" else "No errors have been logged.") :: errors in
			let lines = [ "The following SRs are attached:" ] @ (List.map indent srs) @ [ "" ] @ errors in
			String.concat "" (List.map (fun x -> x ^ "\n") lines)

		let attach_info context ~dbg ~sr ~vdi ~dp =
			let srs = Host.list !Host.host in
			let sr_state = List.assoc sr srs in
			let vdi_state = Hashtbl.find sr_state.Sr.vdis vdi in
			let dp_state = Vdi.get_dp_state dp vdi_state in
			debug "Looking for dp: %s" dp;
			match dp_state,vdi_state.Vdi.attach_info with
				| Vdi_automaton.Activated _, Some attach_info ->
					attach_info
				| _ -> 
					raise (Internal_error (Printf.sprintf "sr: %s vdi: %s Datapath %s not attached" sr vdi dp))


		let stat_vdi context ~dbg ~sr ~vdi () =
			info "DP.stat_vdi dbg:%s sr:%s vdi:%s" dbg sr vdi;
			VDI.with_vdi sr vdi
				(fun () ->
					match Host.find sr !Host.host with
					| None -> raise (Sr_not_attached sr)
					| Some sr_t ->
						let vdi_t = Opt.default (Vdi.empty ()) (Sr.find vdi sr_t) in
						{
							superstate = Vdi.superstate vdi_t;
							dps = List.map (fun dp -> dp, Vdi.get_dp_state dp vdi_t) (Vdi.dps vdi_t)
						}
				)

	end

	module SR = struct
		let locks : (string, unit) Storage_locks.t = Storage_locks.make ()
		let with_sr sr f = Storage_locks.with_instance_lock locks sr f

		let list context ~dbg =
			List.map fst (Host.list !Host.host)

		let scan context ~dbg ~sr =
			info "SR.scan dbg:%s sr:%s" dbg sr;
			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
						| None -> raise (Sr_not_attached sr)
						| Some _ ->
							Impl.SR.scan context ~dbg ~sr
				)

		let create context ~dbg ~sr ~device_config ~physical_size =
			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
						| None ->
							Impl.SR.create context ~dbg ~sr ~device_config ~physical_size
						| Some _ ->
							error "SR %s is already attached" sr;
							raise (Sr_attached sr)
				)

		let attach context ~dbg ~sr ~device_config =
			info "SR.attach dbg:%s sr:%s device_config:[%s]" dbg sr (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) device_config));
			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
					| None ->
						Impl.SR.attach context ~dbg ~sr ~device_config;
						Host.replace sr (Sr.empty ()) !Host.host;
						(* FH1: Perform the side-effect first: in the case of a
						   failure half-way through we would rather perform the
						   side-effect twice than never at all. *)
						Everything.to_file !host_state_path (Everything.make ())
					| Some _ ->
						(* Operation is idempotent *)
						()
				)

		let detach_destroy_common context ~dbg ~sr f =
			let active_dps sr_t =
				(* Enumerate all active datapaths *)
				List.concat (List.map (fun (_, vdi_t) -> Vdi.dps vdi_t) (Sr.list sr_t)) in

			with_sr sr
				(fun () ->
					match Host.find sr !Host.host with
					| None -> raise (Sr_not_attached sr)
					| Some sr_t ->
						VDI.with_all_vdis sr
							(fun () ->
								let dps = active_dps sr_t in
								List.iter
									(fun dp ->
										let ( _ : exn list) = DP.destroy_sr context ~dbg ~dp ~sr ~allow_leak:false true in ()
									) dps;
								let dps = active_dps sr_t in
								if dps <> []
								then error "The following datapaths have leaked: %s" (String.concat "; " dps);
								f context ~dbg ~sr;
								Host.remove sr !Host.host;
								Everything.to_file !host_state_path (Everything.make ());
								VDI.locks_remove sr
							)
				)

		let detach context ~dbg ~sr =
			info "SR.detach dbg:%s sr:%s" dbg sr;
			detach_destroy_common context ~dbg ~sr Impl.SR.detach

		let reset context ~dbg ~sr =
			info "SR.reset dbg:%s sr:%s" dbg sr;
			with_sr sr
				(fun () ->
					Host.remove sr !Host.host;
					Everything.to_file !host_state_path (Everything.make ());
					VDI.locks_remove sr
				)

		let destroy context ~dbg ~sr = 
			info "SR.destroy dbg:%s sr:%s" dbg sr;
			detach_destroy_common context ~dbg ~sr Impl.SR.destroy			

		let update_snapshot_info_src context ~dbg ~sr ~vdi ~url
				~dest ~dest_vdi ~snapshot_pairs=
			info
				"SR.update_snapshot_info_src dbg:%s sr:%s vdi:%s url:%s dest:%s dest_vdi:%s snapshot_pairs:%s"
				dbg sr vdi url dest dest_vdi
				(List.map
					(fun (local_snapshot, dest_snapshot) ->
						Printf.sprintf "local:%s, dest:%s" local_snapshot dest_snapshot)
					snapshot_pairs
					|> String.concat "; "
					|> Printf.sprintf "[%s]");
			Impl.SR.update_snapshot_info_src context ~dbg ~sr ~vdi ~url
				~dest ~dest_vdi ~snapshot_pairs

		let update_snapshot_info_dest context ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs =
			info
				"SR.update_snapshot_info_dest dbg:%s sr:%s vdi:%s ~src_vdi:%s snapshot_pairs:%s"
				dbg sr vdi src_vdi.vdi
				(List.map
					(fun (local_snapshot, src_snapshot_info) ->
						Printf.sprintf "local:%s, src:%s" local_snapshot src_snapshot_info.vdi)
					snapshot_pairs
					|> String.concat "; "
					|> Printf.sprintf "[%s]");
			Impl.SR.update_snapshot_info_dest context ~dbg ~sr ~vdi
				~src_vdi ~snapshot_pairs
	end

	module Policy = struct
		let get_backend_vm context ~dbg ~vm ~sr ~vdi =
			Impl.Policy.get_backend_vm context ~dbg ~vm ~sr ~vdi
	end

	module TASK = struct
		open Storage_task
		let t x = {
			Task.id = x.id;
			debug_info = x.dbg;
			ctime = x.ctime;
			state = x.state;
			subtasks = x.subtasks;
		}

		let cancel _ ~dbg ~task =
			Storage_task.cancel tasks task
		let stat' task =
			Mutex.execute tasks.m
				(fun () ->
					find_locked tasks task |> t
				)
		let stat _ ~dbg ~task = stat' task
		let destroy' ~task = 
			destroy tasks task;
			Updates.remove (Dynamic.Task task) updates
		let destroy _ ~dbg ~task = destroy' ~task
		let list _ ~dbg = list tasks |> List.map t
	end

	module UPDATES = struct
		let get _ ~dbg ~from ~timeout =
			let from = try Some (int_of_string from) with _ -> None in
			let _, ids, next = Updates.get dbg from timeout updates in
			(ids, string_of_int next)
	end

end

let initialise () =
	Unixext.mkdir_rec (Filename.dirname !host_state_path) 0o700;
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

open Xmlrpc_client
let local_url = Http.Url.(File { path = Filename.concat Fhs.vardir "storage" }, { uri = "/"; query_params = [] })

let rpc ~srcstr ~dststr url call =
	XMLRPC_protocol.rpc ~transport:(transport_of_url url)
		~srcstr ~dststr ~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) ~query:(Http.Url.get_query_params url) (Http.Url.get_uri url)) call

module Local = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"smapiv2" local_url end)
