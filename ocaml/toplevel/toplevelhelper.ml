
let host = ref ""
let port = ref 0

let rpc xml =
  Xmlrpcclient.do_secure_xml_rpc ~host:!host ~version:"1.1" ~port:!port ~path:"/" xml

open Client

let init_session username password =
  Client.Session.login_with_password ~rpc ~uname:username ~pwd:password ~version:"1.2"
