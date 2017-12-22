module XenAPI = Client.Client

let rpc xml =
  let open Xmlrpc_client in
  XMLRPC_protocol.rpc
    ~srcstr:"daily-license-check"
    ~dststr:"xapi"
    ~transport:(Unix "/var/xapi/xapi")
    ~http:(xmlrpc ~version:"1.0" "/")
    xml

let _ =
  let session = XenAPI.Session.login_with_password
      ~rpc:rpc
      ~uname:""
      ~pwd:""
      ~version:"1.0"
      ~originator:"daily-license-check"
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       let now = Unix.time () in
       let pool, pool_license_state, all_license_params = Daily_license_check.get_info_from_db rpc session in
       let result = Daily_license_check.check_license now pool_license_state all_license_params in
       Daily_license_check.execute rpc session pool result
    )
    (fun () -> XenAPI.Session.logout rpc session)
