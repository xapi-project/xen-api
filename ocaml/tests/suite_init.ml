
let handlers = [
  "get_services", Http_svr.FdIO Xapi_services.get_handler;
  "post_services", Http_svr.FdIO Xapi_services.post_handler;
  "put_services", Http_svr.FdIO Xapi_services.put_handler;
  "post_root", Http_svr.BufIO (Api_server.callback false);
  "post_json", Http_svr.BufIO (Api_server.callback true);
  "post_jsonrpc", Http_svr.BufIO Api_server.jsoncallback;
  "post_remote_db_access",
  Http_svr.BufIO Db_remote_cache_access_v1.handler;
  "post_remote_db_access_v2",
  Http_svr.BufIO Db_remote_cache_access_v2.handler;
]

let start_server handlers =
  List.iter Xapi_http.add_handler handlers;
  Xapi.listen_unix_socket "/tmp/xapi-test/xapi-unit-test-socket"

let harness_init () =
  Debug.log_to_stdout ();
  Printexc.record_backtrace true;
  Inventory.inventory_filename :=
    Filename.concat Test_common.working_area "xapi-inventory";
  Xcp_client.use_switch := false;
  Pool_role.set_pool_role_for_test ();
  Xapi.register_callback_fns ();
  start_server handlers
