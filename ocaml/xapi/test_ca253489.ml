
(** CA-226886 - test that an error is thrown from "xe update-upload" if no SR
    is specified, and the pool does not have a valid default SR. *)
let test_xe_update_upload_ca253489 () =
  let __context = Test_common.make_test_database () in
  let req = Xmlrpc_client.xmlrpc ~version:"1.1" "/" in
  let rpc = Api_server.Server.dispatch_call req Unix.stdout in
  let session =
    let session_id = Ref.make () in
    let uuid = Uuid.to_string (Uuid.make_uuid ()) in
    Db.Session.create ~__context ~ref:session_id ~uuid
      ~this_user:Ref.null ~this_host:(Helpers.get_localhost ~__context) ~pool:false
      ~last_active:(Stdext.Date.of_float (Unix.time ())) ~other_config:[]
      ~subject:(Ref.null) ~is_local_superuser:true
      ~auth_user_sid:"" ~validation_time:(Stdext.Date.of_float (Unix.time ()))
      ~auth_user_name:"root" ~rbac_permissions:[] ~parent:Ref.null ~originator:"test";
    session_id
  in
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_default_SR ~__context ~self:pool ~value:Ref.null;
  (* omit the sr-uuid parameter *)
  let params = [ ("file-name","test") ] in
  try
    Cli_operations.update_upload Unix.stdout () rpc session params;
    OUnit.assert_failure "A Cli_failure exception should be thrown, because the sr-uuid parameter is not specified, and there is no default SR."
  with
  | Cli_util.Cli_failure _ -> ()
  | _ -> OUnit.assert_failure "Got an unexpected exception."

let test =
  let open OUnit in
  "test_xe_update_upload_ca253489" >:: test_xe_update_upload_ca253489
