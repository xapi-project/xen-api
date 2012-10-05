open Hashtblext
open Listext
open Threadext
open Rrdd_shared
open Rrd
open Ds
open Monitor_types

module D = Debug.Debugger(struct let name = "rrdd_monitor" end)
open D

let create_rras use_min_max =
	(* Create archives of type min, max and average and last *)
	Array.of_list (List.flatten
		(List.map (fun (n,ns) ->
			if ns > 1 && use_min_max then [
				Rrd.rra_create Rrd.CF_Average n ns 1.0;
				Rrd.rra_create Rrd.CF_Min n ns 1.0;
				Rrd.rra_create Rrd.CF_Max n ns 1.0;
			] else [Rrd.rra_create Rrd.CF_Average n ns 0.5]
		) timescales)
	)

let step = 5

(** Create a rrd *)
let create_fresh_rrd use_min_max dss =
	let rras = create_rras use_min_max in
	let dss = Array.of_list (List.filter_map (fun ds ->
		if ds.ds_default then
			Some (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 ~max:ds.ds_max ~min:ds.ds_min Rrd.VT_Unknown)
		else None) dss)
	in
	let rrd = Rrd.rrd_create dss rras (Int64.of_int step) (Unix.gettimeofday()) in
	rrd

(* Updates all of the hosts rrds. We are passed a list of uuids that
 * is used as the primary source for which VMs are resident on us.
 * When a new uuid turns up that we haven't got an RRD for in our
 * hashtbl, we create a new one. When a uuid for which we have an RRD
 * for doesn't appear to have any stats this update, we assume that the
 * domain has gone and we stream the RRD to the master. We also have a
 * list of the currently rebooting VMs to ensure we don't accidentally
 * archive the RRD. *)
let update_rrds timestamp dss (uuid_domids : (string * int) list) rebooting_vms paused_vms =
	(* Here we do the synchronising between the dom0 view of the world
		 and our Hashtbl. By the end of this execute block, the Hashtbl
		 correctly represents the world *)
	let to_send_back = Mutex.execute mutex (fun _ ->
		let out_of_date, by_how_much =
			match !host_rrd with
			| None -> false, 0.
			| Some rrdi -> rrdi.rrd.Rrd.last_updated > timestamp, abs_float (timestamp -. rrdi.rrd.Rrd.last_updated)
		in
		if out_of_date then
			error "Clock just went backwards by %.0f seconds: RRD data may now be unreliable" by_how_much;
		let registered = Hashtbl.fold_keys vm_rrds in
		let gone_vms = List.filter (fun vm -> not (List.mem_assoc vm uuid_domids)) registered in
		let to_send_back = List.map (fun uuid -> uuid, Hashtbl.find vm_rrds uuid) gone_vms in
		(* Don't send back rebooting VMs! *)
		let to_send_back = List.filter (fun (uuid, _) ->
			let rebooting = (List.exists (fun uuid' -> uuid = uuid') rebooting_vms) in
			if rebooting then debug "Ignoring disappeared VM which is rebooting";
			not rebooting
		) to_send_back in
		List.iter (fun (uuid, _) -> Hashtbl.remove vm_rrds uuid) to_send_back;
		let do_vm (vm_uuid, domid) =
			try
				let dss = List.filter_map (fun (ty, ds) -> match ty with | VM x -> if x = vm_uuid then Some ds else None | _ -> None) dss in
				begin
					try
						(* First, potentially update the rrd with any new default dss *)
						let rrdi = Hashtbl.find vm_rrds vm_uuid in
						let default_dss = List.filter (fun ds -> ds.ds_default) dss in
						let current_dss = Rrd.ds_names rrdi.rrd in
						let new_defaults = List.filter (fun ds -> not (List.mem ds.ds_name current_dss)) default_dss in
						let rrd =
							if List.length new_defaults > 0 then (
								let rrd = List.fold_left (fun rrd ds -> Rrd.rrd_add_ds rrd (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)) rrdi.rrd new_defaults in
								Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid};
								rrd
							) else rrdi.rrd
						in
						(* CA-34383:
						 * Memory updates from paused domains serve no useful purpose.
						 * During a migrate such updates can also cause undesirable
						 * discontinuities in the observed value of memory_actual.
						 * Hence, we ignore changes from paused domains:
						 *)
						if not (List.mem vm_uuid paused_vms) then (
							Rrd.ds_update_named rrd timestamp ~new_domid:(domid <> rrdi.domid)
								(List.map (fun ds -> (ds.ds_name, (ds.ds_value, ds.ds_pdp_transform_function))) dss);
							rrdi.dss <- dss;
							rrdi.domid <- domid;
						)
					with
					| Not_found ->
						debug "Creating fresh RRD for VM uuid=%s" vm_uuid;
						let rrd = create_fresh_rrd (!use_min_max) dss in
						Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid}
					| e -> raise e
				end
			with e ->
				(*debug "Error: caught exception %s" (ExnHelper.string_of_exn e);*)
				log_backtrace ()
		in
		List.iter do_vm uuid_domids;
		let host_dss = List.filter_map (fun (ty, ds) -> match ty with | Host -> Some ds | _ -> None) dss in
		begin
			match !host_rrd with
			| None ->
				begin
					debug "Creating fresh RRD for localhost";
					let rrd = create_fresh_rrd true host_dss in (* Always always create localhost rrds with min/max enabled *)
					host_rrd := Some {rrd; dss = host_dss; domid = 0}
				end
			| Some rrdi ->
				rrdi.dss <- host_dss;
				let default_dss = List.filter (fun ds -> ds.ds_default) host_dss in
				let current_dss = Rrd.ds_names rrdi.rrd in
				let new_defaults = List.filter (fun ds -> not (List.mem ds.ds_name current_dss)) default_dss in
				let rrd =
					if List.length new_defaults > 0 then
						let rrd = List.fold_left (fun rrd ds -> Rrd.rrd_add_ds rrd (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)) rrdi.rrd new_defaults in
						host_rrd := Some {rrd; dss = host_dss; domid = 0};
						rrd
					else
						rrdi.rrd
				in
				Rrd.ds_update_named rrd timestamp ~new_domid:false
					(List.map (fun ds -> (ds.ds_name, (ds.ds_value,ds.ds_pdp_transform_function))) host_dss)
		end;
		to_send_back
	)
	in List.iter (fun (uuid, rrdi) ->
		debug "Sending back RRD for VM uuid=%s" uuid;
		archive_rrd ~uuid ~rrd:rrdi.rrd ()) to_send_back
