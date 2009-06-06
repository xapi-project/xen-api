(* 
---------------------------------------------------------------------------------

   Provides MTC-specific code to integrate with Citrix's XAPI Code

---------------------------------------------------------------------------------
*)

(* 
 * -----------------------------------------------------------------------------
 *  Include other modules here.
 * -----------------------------------------------------------------------------
 *)
open Pervasiveext
open Printf
open Vmopshelpers

module DD=Debug.Debugger(struct let name="MTC:" end)
open DD



(* 
 * -----------------------------------------------------------------------------
 *  Put global constants here.
 * -----------------------------------------------------------------------------
 *)


(* 
 * -----------------------------------------------------------------------------
 *  Functions related to MTC peer and enabled feature.
 * -----------------------------------------------------------------------------
 *)
(* 
 * MTC: Newly added value in a VM's other-config field that specifies (when true) 
 * that this VM is protected.  By protection we mean high-availability or fault 
 * tolerant protection.
 *)
let vm_protected_key = "vm_protected"
let vm_peer_uuid_key = "vm_peer_uuid"

(*
* This function looks at the 'other-config' field in the VM's configuration
* database to determine if the UUID of its peer VM is specified.  If it is,
* then it returns that value, otherwise, it returns None.
*)
let get_peer_vm_uuid ~__context ~self =
   try Some (List.assoc vm_peer_uuid_key (Db.VM.get_other_config ~__context ~self)) with _ -> None


(*
 * This function looks in the configuration database and examines
 * the record of the provided VM to see if a peer VM is specified.
 * If a peer VM is specified, it returns its VM reference object 
 * representation.  Otherwise, it returns a null VM.
 *)
let get_peer_vm ~__context ~self =

  (* Extract the UUID from the configuration. Returns None if not available *)
  let uuid_str_op = get_peer_vm_uuid ~__context ~self in

  (* If a VM peer was found, then look up in the database the VM's record
   * using the VM's UUID field as a key.
   *)
  match uuid_str_op with
      Some uuid ->
          (* debug "VM %s has a peer VM UUID of %s" (Db.VM.get_uuid ~__context ~self) uuid; *)
          Db.VM.get_by_uuid ~__context ~uuid
      | None ->
          Ref.null


(*
 * This function looks at the 'other-config' field in the VM's configuration
 * database to determine if the 'vm_protected' key is present AND set to true.
 * It will return true if both of these conditions exist.
 *)
let is_this_vm_protected ~__context ~self =
  try  
    let other_config = Db.VM.get_other_config ~__context ~self in
    let protected = ((List.mem_assoc vm_protected_key other_config) &&
                     (List.assoc vm_protected_key other_config)="true") in
    protected
  with _ -> false
  
(*
 * This routine is invoke when a request for a migration is received
 * at the destination side.  It figures out the correct VM configuration 
 * to be used to instantiate a VM to receive the migrated data.  The 
 * logic says that if the source VM is a protected VM, then we'll
 * look up its peer VM (the destination) and return that VM to be instantiated.
 * If it's not protected, then the VM reference returned is that of the
 * source as this is a normal XenSource migration.
*)
let get_peer_vm_or_self ~__context ~self =
  try
    if (is_this_vm_protected ~__context ~self) then (
      let peer_vm = get_peer_vm ~__context ~self in
      if peer_vm <> Ref.null then
        peer_vm
      else (
        error "MTC: VM %s was found to be protected but it lacked its peer VM specification" 
           (Db.VM.get_uuid ~__context ~self);
        self
      )
    )
    else self;
  with _ -> self

(*
 * This routine is used to determine if the specified VM is a protected
 * VM and its domain has already been previously instantiated.  If both
 * cases are true, then it returns its domain ID.  Otherwise, it returns
 * -1 to signal the caller that it should create its own domain.
 *)
let use_protected_vm ~__context ~self =
  if (is_this_vm_protected ~__context ~self) then 
    begin 
      let domid = Helpers.domid_of_vm ~__context ~self in
      debug "This VM (%s) is protected and its currently running in domID = %d"
         (Db.VM.get_uuid ~__context ~self) domid;
      domid;
    end
  else
    begin
      debug "This VM (%s) is NOT protected" (Db.VM.get_uuid ~__context ~self);
      -1
    end


(* 
 * -----------------------------------------------------------------------------
 *  External Event Related Functions
 * -----------------------------------------------------------------------------
 *)

(* This is the base migration key on which sub-keys will be added to provide 
   and receive external events *)
let migration_key                             = "/migration" 
let migration_task_status_key                 =            "/status" 
let migration_task_progress_key               =            "/progress" 
let migration_task_error_info_key             =            "/error_info" 
let migration_event_entered_suspend_key       =            "/entering_fg" 
let migration_event_entered_suspend_acked_key =            "/entering_fg_acked" 
let migration_event_abort_req_key             =            "/abort"

(* The base xenstore path + the migration key in which events will be added.
   This function currently returns /local/domain/<domid>/migration.
   Therefore, it is imperative that the ~vm be associated with a domain ID *)
let migration_base_path ~xs ~__context ~vm =
      let domid = Helpers.domid_of_vm ~__context ~self:vm in
      xs.Xs.getdomainpath domid ^ migration_key

(* Converts the Task object's status into a string.  Any new states that
   this code does not recognize will return "unknown" *)  
let string_of_task_status status =
      match status with
        | `pending -> "pending"
        | `success -> "success"
        | `failure -> "failure"
        | `cancelled -> "cancelled"
        | _ -> "unknown"

(* Initializes event notification keys.  Currently it just removes all previous
   entry under the 'migration_base_path' *)
let event_notify_init ~__context ~vm =
  if (is_this_vm_protected ~__context ~self:vm) then (
    with_xc_and_xs
      (fun xc xs ->
        let path = migration_base_path ~xs ~__context ~vm in
        debug "Event notification: initializing. Path: %s" path;
	try
          (* We don't remove the base path because there may be a request to
             abort a migration that we want to catch before the migration gets
             too far along.  We saw this problem when dom0 was really busy, our
             AM issues the migrate command to XAPI, but because XAPI doesn't get
             to run in time, our AM times out the migration request and sets the
             abort key so that when XAPI gets around to running, it knows not to
             start the migration. *)
          (* xs.Xs.rm path; *)
          let key = (migration_base_path ~xs ~__context ~vm) ^ migration_event_entered_suspend_key in
          xs.Xs.rm key;
          let key = (migration_base_path ~xs ~__context ~vm) ^ migration_event_entered_suspend_acked_key in
          xs.Xs.rm key;
          let key = (migration_base_path ~xs ~__context ~vm) ^ migration_task_error_info_key in
          xs.Xs.rm key;
          let key = (migration_base_path ~xs ~__context ~vm) ^ migration_task_progress_key in
          xs.Xs.rm key;
          let key = (migration_base_path ~xs ~__context ~vm) ^ migration_task_status_key in
          xs.Xs.rm key;
        with _ ->
           debug "Ignoring failure while deleting migration keys. Path: %s" path;

      )
  )

(* Writes out fields from task to xenstore keys. Note that we write the 
   'status' field last to guarantee any listeners that the rest of the
   field values are accurate *)
let event_notify_task_status ~__context ~vm ~status ?(str="no error info") progress  =
  if (is_this_vm_protected ~__context ~self:vm) then (
    with_xc_and_xs
      (fun xc xs ->
  
        debug "Event: Updating MTC task status: %s." (string_of_task_status status);
  
        (* Write out the task's progress indicator field *)
        let key = (migration_base_path ~xs ~__context ~vm) ^ migration_task_progress_key in
        let adjusted = int_of_float (progress *. 100.) in
        let value = string_of_int adjusted in
        debug "progress = %s" value;
        xs.Xs.write key value;
  
        let key = (migration_base_path ~xs ~__context ~vm) ^ migration_task_error_info_key in
  	xs.Xs.write key str;
  
        (* Write out the status field.  It should be written last so anyone watching
           on it can be certain that the other fields are accurate. *)
        let key = (migration_base_path ~xs ~__context ~vm) ^ migration_task_status_key in
        let value = string_of_task_status status in
        xs.Xs.write key value;  
      )
  )  

(* Provides event via xenstore that the migration process has now entered
   the foreground phase (ie, domain has been suspended).
   Called only by the source of a migration. *)
let event_notify_entering_suspend ~__context ~self =
  if (is_this_vm_protected ~__context ~self) then (
    with_xc_and_xs
      (fun xc xs ->
        let key = (migration_base_path ~xs ~__context ~vm:self) ^ 
                   migration_event_entered_suspend_key in
        debug "Entering suspend. Key: %s" key;
        xs.Xs.write key "1";
      )
  )

(* A blocking wait.  Wait for external party to acknowlege the suspend
   stage has been entered. If no response is received within timeout,
   routine simply returns `ACKED to simulate a response. *)
let event_wait_entering_suspend_acked ?timeout ~__context ~self =
  if (is_this_vm_protected ~__context ~self) then (
    with_xc_and_xs
      (fun xc xs ->
        let ack_key = (migration_base_path ~xs ~__context ~vm:self) ^ 
                       migration_event_entered_suspend_acked_key in
        let abort_key = (migration_base_path ~xs ~__context ~vm:self) ^ 
                         migration_event_abort_req_key in
        debug "Waiting for suspend ack. Key: %s or abort key: %s" ack_key abort_key;

        try
	  (* Allow an abort event to be triggered while waiting for the ack *)
          let abort_req = Watch.value_to_become abort_key "1" in 
	  let ack_watch = Watch.value_to_become ack_key "1" in
	  match Watch.wait_for ~xs ?timeout (Watch.any_of [ `ACKED, ack_watch; `ABORT, abort_req]) with
 	    | `ACKED, _ ->
	         debug "Suspend was acked on key: %s" ack_key;

                 (* Give precendence to the abort key if both are found to be set *)
                let value = try xs.Xs.read abort_key with _ -> "" in
                 if (value = "1") then (   
	           debug "A suspend ack and abort event were detected. Abort taking precedence";
                   `ABORT
                 ) else
                   `ACKED
	    | `ABORT, _ ->
	         debug "Abort detected while waiting for suspend ack: %s" ack_key;
                 `ABORT
            | _,_ -> `TIMED_OUT            
        with 
          Watch.Timeout _ ->
              debug "Timed-out waiting for suspend ack on key: %s" ack_key;
              `TIMED_OUT            
      )
  ) else `ACKED 

(* Check to see if an abort request has been made through XenStore *)
let event_check_for_abort_req ~__context ~self =
  if (is_this_vm_protected ~__context ~self) then (
    with_xc_and_xs
      (fun xc xs ->
        let abort_key = (migration_base_path ~xs ~__context ~vm:self) ^ 
                       migration_event_abort_req_key in

        debug "Checking if abort was requested. Key: %s" abort_key;

        let value = try xs.Xs.read abort_key with _ -> "not set" in
        debug "Value of abort key: %s" value;
        value = "1"
      )
  ) else false 

(* 
 * -----------------------------------------------------------------------------
 *  Miscellaneous Functions
 * -----------------------------------------------------------------------------
 *)
(*
 * This routine is invoked to update the state of a VM at the end of a migration
 * receive cycle.  For MTC VM's, we may be migrating into a stopped VM and
 * we need to then update its state.  Normal XenMotion migration does not 
 * change the VM's state since they expect the source VM (which is the same
 * as the destination VM) to already be running (otherwise, you couldn't be.
 * doing a migration to begin with).
*)
let update_vm_state_if_necessary ~__context ~vm =
  if (is_this_vm_protected ~__context ~self:vm) then begin
    try
      Db.VM.set_power_state ~__context ~self:vm ~value:`Running ;
      Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm;
    with e ->
      debug "Failed to change the VM's power state to running";
      raise e
  end

