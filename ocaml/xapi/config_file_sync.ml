module D = Debug.Debugger(struct let name="xapi" end)
open D

open Config_file_io

(** URL used by slaves to fetch dom0 config files (currently /etc/passwd) *)
let config_file_sync_handler (req: Http.request) s =
  debug "received request to write out dom0 config files";
  Xapi_http.with_context "Syncing dom0 config files over HTTP" req s
    (fun __context ->
      debug "sending headers";
      Http_svr.headers s (Http.http_200_ok ~keep_alive:false ());
      debug "writing dom0 config files";
      transmit_config_files s;
      debug "finished writing dom0 config files"
    )

let fetch_config_files ~master_address ~pool_secret =
  let headers = Xapi_http.http_request ~cookie:[ "pool_secret", pool_secret ]
    Http.Get master_address Constants.config_sync_uri in
  let st_proc = Xmlrpcclient.get_reusable_stunnel
    ~write_to_log:Xmlrpcclient.write_to_log master_address Xapi_globs.default_ssl_port in
  Pervasiveext.finally
    (fun () ->
       debug "Requesting config files from master";
       let fd = st_proc.Stunnel.fd in
       (* no content length since it's streaming *)
       let _, _ = Xmlrpcclient.http_rpc_fd fd headers "" in
       let inchan = Unix.in_channel_of_descr fd in (* never read from fd again! *)
       let config_files = Unixext.read_whole_file 1024 1024 fd in
       config_files
    )
    (fun () -> Stunnel.disconnect st_proc)
    
  (* Invoked on slave as a notification that config files may have changed. Slaves can use
     this to decide whether to sync the new config files if the hash is different from the
     files they currently have. We do the hash thing to minimize flash writes on OEM build.. *)
let maybe_fetch_config_files ~master_address ~pool_secret ~hash =
  if compute_hash()<>hash then
    (* fetch new config files from master and write them *)
    let config_files = fetch_config_files ~master_address ~pool_secret in
    rewrite_config_files config_files
  
(* Called by slave on each startup to see if master has updated dom0 config files whilst
   slave has been off-line. This way round we request config files from master regardless
   but only write them to disk if the hash is different from the ones we already have.
   (Again, hash is present to reduce unnecessary writes for OEM flash vsn) *)
let fetch_config_files_on_slave_startup () =
  Server_helpers.exec_with_new_task "checking no other known hosts are masters"
    (fun __context ->
      let master_address = Helpers.get_main_ip_address __context in
      let pool_secret = !Xapi_globs.pool_secret in
      let config_files = fetch_config_files ~master_address ~pool_secret in
      let hash_of_my_current_files = compute_hash() in
      let hash_of_masters_files = hash_fn config_files in
      if hash_of_my_current_files<>hash_of_masters_files then
	begin
	  info "Master's dom0 config files differ from mine; resyncing now.";
	  rewrite_config_files config_files
	end)
