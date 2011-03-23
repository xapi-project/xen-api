open Listext

module D = Debug.Debugger(struct let name="xapi" end)
open D

(* This function uses the VM export functionality to *)
(* create the objects required to reimport a list of VMs *)
let create_import_objects ~__context ~vms =
	let table = Export.create_table () in
	List.iter (Export.update_table ~__context ~include_snapshots:true ~preserve_power_state:false ~include_vhd_parents:false ~table) vms;
	Export.make_all ~with_snapshot_metadata:true ~preserve_power_state:false table __context

let recover_vms ~__context ~vms ~session_to ~force =
	let config = {
		Import.sr = Ref.null;
		Import.full_restore = true;
		Import.vm_metadata_only = true;
		Import.force = force;
	} in
	let objects = create_import_objects ~__context ~vms in
	Server_helpers.exec_with_new_task ~session_id:session_to "Importing VMs"
		(fun __context_to ->
			(* Check that session_to has at least pool admin permissions. *)
			let permission = Rbac_static.role_pool_admin in
			if not(Rbac.has_permission ~__context:__context_to ~permission) then begin
				let permission_name = permission.Db_actions.role_name_label in
				raise (Api_errors.Server_error(Api_errors.rbac_permission_denied,
					[permission_name; "The supplied session does not have the required permissions for VM recovery."]))
			end;
			let rpc = Helpers.make_rpc ~__context:__context_to in
			let state = Import.handle_all __context_to
				config rpc session_to objects
			in
			let vmrefs = List.setify
				(List.map
					(fun (cls, id, r) -> Ref.of_string r)
					state.Import.created_vms)
			in
			try
				Import.complete_import ~__context:__context_to vmrefs;
				vmrefs
			with e ->
				if force then
					debug "%s" "VM recovery failed - not cleaning up as action was forced."
				else begin
					debug "%s" "VM recovery failed - cleaning up.";
					Importexport.cleanup state.Import.cleanup
				end;
				raise e)
