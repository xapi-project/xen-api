(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open OUnit
open Test_common
open Stdext
open Unixext
open Client


let test_xe_update_upload_ca226886 () =
  let __context = Test_common.make_test_database () in
  let arg str x = Opt.default [] (Opt.map (fun x -> [ str, x ]) x) in
  let session_id = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.Session.create ~__context ~ref:session_id ~uuid
    ~this_user:Ref.null ~this_host:(Helpers.get_localhost ~__context) ~pool:false
    ~last_active:(Stdext.Date.of_float (Unix.time ())) ~other_config:[]
    ~subject:(Ref.null) ~is_local_superuser:true
    ~auth_user_sid:"" ~validation_time:(Stdext.Date.of_float (Unix.time ()))
    ~auth_user_name:"root" ~rbac_permissions:[] ~parent:Ref.null ~originator:"test";
  let request_origin = Xmlrpc_client.xmlrpc ~version:"1.1" "/" in
  let rpc = Api_server.Server.dispatch_call request_origin Unix.stdout in
  let task_id = Client.Task.create ~rpc:rpc ~session_id
      ~label:"test import raw vdi"
      ~description:"" in
  let cookie = Some "fake_sr" |> arg "sr_id" in
  let query = Some (Ref.string_of task_id) |> arg "task_id" in
  let request = Xmlrpc_client.xmlrpc ~version:"1.1" ~cookie ~query "/" in
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_default_SR ~__context ~self:pool ~value:Ref.null;
  Db.Task.set_status ~__context ~self:task_id ~value:`pending;
  try begin
    let _ = Import_raw_vdi.import None request fd () rpc session_id in
    ()
  end
  with e ->
    let status = Db.Task.get_status ~__context ~self:task_id in
    OUnit.assert_equal `failure status;
    ()


let test =
  "test_xe_update_upload_ca226886" >:: test_xe_update_upload_ca226886