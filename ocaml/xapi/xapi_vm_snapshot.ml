(* 
 * Copyright (c) Citrix Systems 2008. All rights reserved 
 * Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>
 *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

(*************************************************************************************************)
(* Crash-consistant snapshot                                                                     *)
(*************************************************************************************************)
let snapshot ~__context ~vm ~new_name =
	debug "Snapshot: begin";
	TaskHelper.set_cancellable ~__context;
	let res = Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_snapshot ~__context ~vm ~new_name in
	debug "Snapshot: end"; 
	res

(*************************************************************************************************)
(* Quiesced snapshot                                                                             *)
(*************************************************************************************************)
let vss_mutex = Mutex.create ()

(* xenstore paths *)
let control_path ~xs ~domid x =
	xs.Xs.getdomainpath domid ^ "/control/" ^ x

let snapshot_path ~xs ~domid x =
	xs.Xs.getdomainpath domid ^ "/snapshot/" ^ x

(* check if [flag] is set in the control_path of the VM [vm]. This looks like this code is a kind  *)
(* of duplicate of the one in {!xal.ml}, {!events.ml} and {!xapi_guest_agent.ml} which are looking *)
(* dynamically if there is a change in this part of the VM's xenstore tree. However, at the moment *)
(* always allowing the operation and checking if it is enabled when it is triggered is sufficient. *)
let is_flag_set ~xs ~flag ~domid ~vm =
	try
		xs.Xs.read (control_path ~xs ~domid flag) = "1"
	with e ->
		debug "Exception while reading %s flag of VM %s (domain %i): %s"
			flag (Ref.string_of vm) domid (Printexc.to_string e);
		false

let quiesce_enabled ~xs ~domid ~vm =
	let aux x = is_flag_set ~xs ~domid ~vm ~flag:x in
	aux "feature-snapshot" || aux "feature-quiesce"

(* we want to compare the integer at the end of a common string, ie. strings as x="/local/..../3" *)
(* and y="/local/.../12". The result should be x < y.                                             *)
let compare_snapid_chunks s1 s2 =
	if String.length s1 <> String.length s2
	then String.length s1 - String.length s2
	else compare s1 s2

(* wait for the VSS provider (or similar piece of software running inside the guest) to quiesce *)
(* the applications of the VM and to call VM.snapshot. After that, the VSS provider is supposed *)
(* to tell us if everything happened nicely.                                                    *)
(* The vss_mutex lock is already taken when this function is called.                            *)
let wait_for_snapshot ~__context ~xs ~domid ~new_name =
	let value = Watch.value_to_appear (snapshot_path ~xs ~domid "status") in
	match Watch.wait_for ~xs ~timeout:(5.*.60.) value with
	| "snapshot-created" ->
		(* Get the transportable snap ID *)
		debug "Snapshot_with_quiesce: get the transportable ID";
		let snapid = xs.Xs.directory (snapshot_path ~xs ~domid "snapid") in
		let snapid = List.sort compare_snapid_chunks snapid in
		let read_chunk x = xs.Xs.read (snapshot_path ~xs ~domid ("snapid/" ^ x)) in
		let snapid = String.concat "" (List.map read_chunk snapid) in

		(* Get the uuid of the snapshot VM *)
		debug "Snapshot_with_quiesce: get uuid of the snapshot VM";
		let snapshot_uuid = xs.Xs.read (snapshot_path ~xs ~domid "snapuuid") in
		let snapshot_ref = Db.VM.get_by_uuid ~__context ~uuid:snapshot_uuid in

		Db.VM.set_transportable_snapshot_id ~__context ~self:snapshot_ref ~value:snapid;
		Db.VM.set_name_label ~__context ~self:snapshot_ref ~value:new_name;  
		snapshot_ref

	| "snapshot-error" ->
		(* If an error was occured we get the error type and return *)
		debug "Snapshot_with_quiesce: get an error";
		raise (Api_errors.Server_error
			(Api_errors.vm_snapshot_failed, [ xs.Xs.read (snapshot_path ~xs ~domid "error") ]))

	| _ -> 
		raise (Api_errors.Server_error(Api_errors.vm_snapshot_failed, [ "unexpected result" ]))

(* We fail if the guest does not support quiesce mode. It would be possible to dynamicaly detect *)
(* if snapshot-with-quiesce is allowed or not, but it seems to be useless as the GUI won't need  *)
(* to display that information to the userm, apparently.                                         *)
let snapshot_with_quiesce ~__context ~vm ~new_name =
	debug "Snapshot_with_quiesce: begin";
	let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
	let result = Vmopshelpers.with_xs (fun xs ->
		(* 1. We first check if the VM supports quiesce-mode *)
		if quiesce_enabled ~xs ~domid ~vm
		then begin Pervasiveext.finally
			(fun () ->
				Mutex.lock vss_mutex;
				(* 2. if it the case, we can trigger a VSS snapshot *)
				xs.Xs.rm (xs.Xs.getdomainpath domid ^ "snapshot");
				xs.Xs.write (snapshot_path ~xs ~domid "action") "create-snapshot";

				try 
					debug "Snapshot_with_quiesce: wait for the VSS agent to take the hand";
					let value = Watch.key_to_disappear (snapshot_path ~xs ~domid "action") in
					Watch.wait_for ~xs ~timeout:(60.) value;
					debug "Snapshot_with_quiesce: wait for the VSS agent to return a snapshot";
					try wait_for_snapshot ~__context ~xs ~domid ~new_name
					with Xs.Timeout ->
						raise (Api_errors.Server_error
							(Api_errors.vm_snapshot_failed, [ "Timed out waiting for VSS snapshot" ]))

				with Xs.Timeout ->
					raise (Api_errors.Server_error
						(Api_errors.vm_snapshot_failed, [ "VSS plugin does not respond" ])))
			(fun () ->
				xs.Xs.rm (xs.Xs.getdomainpath domid ^ "snapshot");
				Mutex.unlock vss_mutex)
		end else
			raise (Api_errors.Server_error
				(Api_errors.vm_snapshot_failed, ["Quiesce snapshot not supported"])))
	in
	debug "Snapshot_with_quiesce: end";
	result
