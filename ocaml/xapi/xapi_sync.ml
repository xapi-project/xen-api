(* Synchronise the locally stored objects between the hosts in a pool *)

module D = Debug.Debugger(struct let name="sync" end)
open D

open Threadext

let sync_lock = Mutex.create ()

let post_sync_hook __context host =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
    try
      let result = Client.Client.Host.call_plugin rpc session_id host "post-blob-sync" "run" [] in
      debug "Result of sync: '%s'" result
    with e ->
      warn "Post sync hook failed: exception: %s" (ExnHelper.string_of_exn e)
  )

let sync_host ~__context host =
  Mutex.execute sync_lock (fun () ->
    try
      let localhost = host = !Xapi_globs.localhost_ref in
      let host_has_storage = not (List.mem_assoc Xapi_globs.host_no_local_storage (Db.Host.get_other_config ~__context ~self:host)) in
      if (not localhost) && host_has_storage then begin
	let address = Db.Host.get_address ~__context ~self:host in
	debug "Beginning sync with host at address: %s" address;
	let localpath = Printf.sprintf "%s/" Xapi_globs.xapi_blob_location in
	let remotepath = Printf.sprintf "%s:%s" address Xapi_globs.xapi_blob_location in
	let session = Xapi_session.slave_login ~__context ~host:(Helpers.get_localhost ~__context) ~psecret:!Xapi_globs.pool_secret in
	Unix.putenv "XSH_SESSION" (Ref.string_of session);
	let output,log = Forkhelpers.execute_command_get_output "/usr/bin/rsync" ["--delete";"-avz";localpath;remotepath;"-e";"/opt/xensource/bin/xsh"] in
	debug "sync output: '%s' log: '%s'" output log;
	post_sync_hook __context host
      end else begin
	debug "Ignoring host synchronise: localhost=%b host_has_storage=%b" localhost host_has_storage
      end; 
      if host_has_storage && localhost then post_sync_hook __context host
    with Forkhelpers.Spawn_internal_error(log,output,status) ->
      error "Error in rsyncing: log='%s' output='%s'" log output;
      (* CA-20574: Supress the alert if we're in rolling upgrade mode -- we expect this to fail during rolling upgrade and we don't want
	 the user to see a scary error message *)
      if not (Helpers.rolling_upgrade_in_progress ~__context) then
	begin
	  let uuid = Db.Host.get_uuid ~__context ~self:host in
	  let name = Db.Host.get_name_label ~__context ~self:host in
	  ignore(Xapi_message.create ~__context ~name:Api_messages.host_sync_data_failed ~priority:2L ~cls:`Host ~obj_uuid:uuid
		   ~body:(Printf.sprintf "Failed to synchonise data with host '%s'. Rsync reported '%s'" name log))
	end
      else
	debug "Not generating HOST_SYNC_DATA_FAILED_ALERT because we are in rolling upgrade mode"
  )

let do_sync () =
  Server_helpers.exec_with_new_task "blob sync" (fun __context ->
      let hosts = Db.Host.get_all ~__context in
      List.iter (sync_host ~__context) hosts)

