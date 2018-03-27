(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)
(**
 * @group Performance Monitoring
*)

(* This module is used for easier interaction of xapi with rrdd. Mainly, it
 * looks up the required information that is available to xapi, and calls
 * same-named methods in rrdd.
*)

module D = Debug.Make(struct let name="rrdd_proxy" end)
open D

module Rrdd = Rrd_client.Client

(* Helper methods. Should probably be moved to the Http.Request module. *)
let get_query_string_from_query ~(query : (string * string) list) : string =
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) query)

let make_url_from_query ~(address : string) ~(uri : string)
    ~(query : (string * string) list) : string =
  let query_string = get_query_string_from_query query in
  Printf.sprintf "https://%s%s?%s" address uri query_string

let make_url ~(address : string) ~(req : Http.Request.t) : string =
  let open Http.Request in
  make_url_from_query ~address ~uri:req.uri ~query:req.query

let fail_req_with (s : Unix.file_descr) msg (http_err : unit -> string list) =
  error msg;
  Http_svr.headers s (http_err ())
(* End of helper methods. *)

(* If the host contains the RRD for the requested VM then simply forward the
 * HTTP request to rrdd_http_handler. Otherwise, we redirect to the host that
 * contains the corresponding VM. The last resort is to unarchive the RRD on
 * the master. The exact logic can be seen under "The logic." below.
*)
let get_vm_rrd_forwarder (req : Http.Request.t) (s : Unix.file_descr) _ =
  debug "put_rrd_forwarder: start";
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  let vm_uuid = List.assoc "uuid" query in
  if not (List.mem_assoc "ref" query) && not (List.mem_assoc "uuid" query) then
    fail_req_with s "get_vm_rrd: missing the 'uuid' parameter"
      Http.http_400_badrequest
  else if Rrdd.has_vm_rrd ~vm_uuid then (
    ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path))
  ) else (
    Xapi_http.with_context ~dummy:true "Get VM RRD." req s
      (fun __context ->
         let open Http.Request in
         (* List of possible actions. *)
         let read_at_owner owner =
           let address = Db.Host.get_address ~__context ~self:owner in
           let url = make_url ~address ~req in
           Http_svr.headers s (Http.http_302_redirect url) in
         let unarchive_at_master () =
           let address = Pool_role.get_master_address () in
           let query = (Constants.rrd_unarchive, "") :: query in
           let url = make_url_from_query ~address ~uri:req.uri ~query in
           Http_svr.headers s (Http.http_302_redirect url) in
         let unarchive () =
           let req = {req with uri = Constants.rrd_unarchive_uri} in
           ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path)) in
         (* List of conditions involved. *)
         let is_unarchive_request = List.mem_assoc Constants.rrd_unarchive query in
         let is_master = Pool_role.is_master () in
         let is_owner_online owner = Db.is_valid_ref __context owner in
         let is_xapi_initialising = List.mem_assoc "dbsync" query in
         (* The logic. *)
         if is_unarchive_request then unarchive ()
         else (
           let localhost_uuid = Helpers.get_localhost_uuid () in
           let vm_ref = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
           let owner = Db.VM.get_resident_on ~__context ~self:vm_ref in
           let owner_uuid = Ref.string_of owner in
           let is_owner_localhost = (owner_uuid = localhost_uuid) in
           if is_owner_localhost then (
             if is_master then unarchive () else unarchive_at_master ()
           ) else (
             if is_owner_online owner && not is_xapi_initialising
             then read_at_owner owner
             else unarchive_at_master ()
           )
         )
      )
  )

(* Forward the request for host RRD data to the RRDD HTTP handler. If the host
 * is initialising, send the unarchive command to the host instead.
*)
let get_host_rrd_forwarder (req: Http.Request.t) (s : Unix.file_descr) _ =
  debug "get_host_rrd_forwarder";
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  Xapi_http.with_context ~dummy:true "Get Host RRD." req s
    (fun __context ->
       debug "get_host_rrd_forwarder: obtained context";
       if List.mem_assoc "dbsync" query then ( (* Host initialising. *)
         debug "get_host_rrd_forwarder: dbsync";
         if not (List.mem_assoc "uuid" query) then (
           fail_req_with s "get_host_rrd: missing the 'uuid' parameter"
             Http.http_400_badrequest
         ) else (
           debug "get_host_rrd_forwarder: forward to unarchive";
           let req = {req with Http.Request.uri = Constants.rrd_unarchive_uri} in
           ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path))
         )
       ) else ( (* Normal request. *)
         debug "get_host_rrd_forwarder: normal";
         ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path))
       )
    )

(* Forward the request for SR RRD data to the RRDD HTTP handler. *)
let get_sr_rrd_forwarder (req: Http.Request.t) (s: Unix.file_descr) _ =
  debug "get_sr_rrd_forwarder";
  let query  = req.Http.Request.query in
  req.Http.Request.close <- true;
  Xapi_http.with_context ~dummy:true "Get SR RRD." req s
    (fun __context ->
       debug "get_sr_rrd_forwarder: obtained context";
       if not (List.mem_assoc "uuid" query) then
         fail_req_with s "get_sr_rrd: missing the 'uuid' parameter"
           Http.http_400_badrequest
       else
         ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path))
    )

(* Forward the request for obtaining RRD data updates to the RRDD HTTP handler. *)
let get_rrd_updates_forwarder (req: Http.Request.t) (s : Unix.file_descr) _ =
  (* Do not log this event, since commonly called. *)
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  Xapi_http.with_context ~dummy:true "Get RRD updates." req s
    (fun __context ->
       if List.mem_assoc "start" query then
         ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path))
       else
         fail_req_with s "get_rrd_updates: missing the 'start' parameter"
           Http.http_400_badrequest
    )

let vm_uuid_to_domid ~__context ~(uuid : string) : int =
  let vm = Db.VM.get_by_uuid ~__context ~uuid in
  Int64.to_int (Db.VM.get_domid ~__context ~self:vm)

(* Given a uuid, this function returns None if uuid is not recognised;
 * otherwise, it returns (false, domid) for a VM, and (true, 0) for a host. *)
let uuid_to_domid ~__context ~(uuid : string) : (bool * int) option =
  try
    ignore (Db.VM.get_by_uuid ~__context ~uuid);
    Some (false, vm_uuid_to_domid ~__context ~uuid)
  with _ -> (
      try ignore (Db.Host.get_by_uuid ~__context ~uuid); Some (true, 0)
      with _ -> None
    )

(* Forward the request for storing RRD. In case an archive is required, the
 * request is redirected to the master. See
 * Rrdd_http_handler.put_rrd_handler. *)
let put_rrd_forwarder (req : Http.Request.t) (s : Unix.file_descr) _ =
  debug "put_rrd_forwarder";
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  let has_uuid = List.mem_assoc "uuid" query in
  let should_archive = List.mem_assoc "archive" query in
  let is_master = Pool_role.is_master () in
  if not has_uuid then (
    fail_req_with s "put_rrd: missing the 'uuid' parameter"
      Http.http_400_badrequest;
  ) else if should_archive && not is_master then (
    let address = Pool_role.get_master_address () in
    let url = make_url ~address ~req in
    Http_svr.headers s (Http.http_302_redirect url)
  ) else Xapi_http.with_context ~dummy:true "Put VM RRD." req s
      (fun __context ->
         let uuid = List.assoc "uuid" query in
         match uuid_to_domid ~__context ~uuid, should_archive with
         | None, _ ->
           fail_req_with s "put_rrd: invalid 'uuid' parameter"
             Http.http_404_missing
         | Some (true, _), false ->
           fail_req_with s "put_rrd: cannot archive host RRD"
             Http.http_400_badrequest
         | Some (is_host, domid), _ ->
           let is_host_key_val = "is_host", string_of_bool is_host in
           let domid_key_val = "domid", string_of_int domid in
           let req = {req with
                      Http.Request.query = is_host_key_val::domid_key_val::query
                     } in
           ignore (Xapi_services.hand_over_connection req s !(Rrd_interface.forwarded_path))
      )

let host_for_vm ~__context ~vm_uuid =
  let vm = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
  Db.VM.get_resident_on ~__context ~self:vm

let push_rrd ~__context ~(vm_uuid : string) : unit =
  let vm_host = host_for_vm ~__context ~vm_uuid in
  if vm_host = Ref.null then
    warn "push_rrd: VM not running, so not pushing its RRD"
  else if vm_host = (Helpers.get_localhost ~__context) then
    let domid = vm_uuid_to_domid ~__context ~uuid:vm_uuid in
    log_and_ignore_exn (fun () -> Rrdd.push_rrd_local ~vm_uuid ~domid)
  else
    let remote_address = Db.Host.get_address ~__context ~self:vm_host in
    log_and_ignore_exn (fun () -> Rrdd.push_rrd_remote ~vm_uuid ~remote_address)

let migrate_rrd ~__context ?remote_address ?session_id ~vm_uuid ~host_uuid () =
  let remote_address = match remote_address with
    | None -> Db.Host.get_address ~__context ~self:(Ref.of_string host_uuid)
    | Some a -> a
  in
  log_and_ignore_exn (fun () ->
      Rrdd.migrate_rrd ~remote_address ?session_id ~vm_uuid ~host_uuid
    )

module Deprecated = struct
  let get_timescale ~__context =
    let host = Helpers.get_localhost ~__context in
    let other_config = Db.Host.get_other_config ~__context ~self:host in
    try int_of_string (List.assoc Constants.rrd_update_interval other_config)
    with _ -> 0

  let load_rrd ~__context ~uuid =
    let master_address = try Some (Pool_role.get_master_address ()) with _ -> None in
    let timescale = get_timescale ~__context in
    log_and_ignore_exn (fun () -> Rrdd.Deprecated.load_rrd ~uuid ~master_address ~timescale)
end
