(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module D = Debug.Make(struct let name="xapi" end)
open D

open Stdext.Xstringext

let superuser = "root"

type config = { password : string } [@@deriving rpc]

(* Increment this if config type changes *)
let config_sync_version = 2

let config_sync_uri =
  Filename.concat Constants.config_sync_uri (string_of_int config_sync_version)

let read_config_file () =
  { password = Unixpwd.get superuser }

let parse_config_string config = Jsonrpc.of_string config |> config_of_rpc

let write_config config =
  if Sys.file_exists "/etc/shadow" then begin
    debug "Updating shadow password entry for %s (%s)" superuser __LOC__;
    Unixpwd.setspw superuser config.password
  end else begin
    debug "Updating password entry for %s (%s)" superuser __LOC__;
    Unixpwd.setpwd superuser config.password
  end

let rewrite_config_files config = parse_config_string config |> write_config

let write_to_fd s msg =
  Unix.write_substring s msg 0 (String.length msg) |> ignore

let transmit_config_files s =
  read_config_file () |> rpc_of_config |> Jsonrpc.to_string |> write_to_fd s

(* We still need to respect older XenServer hosts which are expecting
   the entire /etc/password file. We need to make sure we send the
   "un-shadowed" passwd file, so that slaves don't overwrite root's
   password with an 'x'. *)

(* This was introduced in 8c4d3d93f8c0e069b8b0de9c2e2df02a391aa2ef xapi
 * version 1.9.41 and can be removed when during rolling pool
 * upgrade the config_sync_version is always passed from slave to
 * master. We believe that Dundee is the first version where this code
 * shipped. Hence, it can be removed once RPU from pre-Dundee is no
 * longer supported.
 *)

let legacy_transmit_passwd s =
  debug "Updating /etc/passwd (%s)" __LOC__;
  Unixpwd.unshadow () |> write_to_fd s

(** URL used by slaves to fetch dom0 config files (currently just root's password) *)
let config_file_sync_handler (req: Http.Request.t) s _ =
  let current version =
    let version = try int_of_string version with _ -> -1 in
    version >= config_sync_version in
  debug "received request to write out dom0 config files";
  Xapi_http.with_context "Syncing dom0 config files over HTTP" req s
    (fun __context ->
       let uri = String.split '/' (req.Http.Request.uri) |> List.filter (fun x -> x <> "") in
       req.Http.Request.close <- true;
       debug "sending headers";
       Http_svr.headers s (Http.http_200_ok ~keep_alive:false ());
       match uri with
       | [path; version] when current version ->
         debug "writing dom0 config files";
         transmit_config_files s;
         debug "finished writing dom0 config files"
       | _ ->
         debug "writing legacy dom0 config files";
         legacy_transmit_passwd s;
         debug "finished writing legacy dom0 config files")

let fetch_config_files_internal ~master_address ~pool_secret =
  Server_helpers.exec_with_new_task "fetch_config_files"
    (fun __context ->
       Helpers.call_api_functions ~__context
         (fun rpc session_id ->
            let request = Xapi_http.http_request
                ~cookie:[ "session_id", Ref.string_of session_id ]
                Http.Get config_sync_uri in
            let open Xmlrpc_client in
            let transport = SSL (SSL.make (), master_address, !Xapi_globs.https_port) in
            with_transport transport
              (with_http request
                 (fun (response, fd) ->
                    Stdext.Unixext.string_of_fd fd))))

(* Invoked on slave as a notification that config files may have changed. Slaves can use
   this to decide whether to sync the new config files if the hash is different from the
   files they currently have. We do the hash thing to minimize flash writes on OEM build.. *)
let fetch_config_files ~master_address ~pool_secret =
  (* fetch new config files from master and write them *)
  let config_files = fetch_config_files_internal ~master_address ~pool_secret in
  rewrite_config_files config_files

(* Called by slave on each startup to sync master's config files. *)
let fetch_config_files_on_slave_startup () =
  Server_helpers.exec_with_new_task "checking no other known hosts are masters"
    (fun __context ->
       let master_address = Helpers.get_main_ip_address () in
       let pool_secret = !Xapi_globs.pool_secret in
       let config_files = fetch_config_files_internal ~master_address ~pool_secret in
       rewrite_config_files config_files)
