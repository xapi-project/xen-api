open Create_storage
open Client

let _ = 
  let host = ref "localhost"
  and port = ref Xapi_globs.default_cleartext_port
  and username = ref "root"
  and password = ref "" in
  Arg.parse 
    [ "-h", Arg.Set_string host, "hostname to configure";
      "-p", Arg.Set_int port, "port to talk to";
      "-u", Arg.Set_string username, "username to log in as";
      "-pw", Arg.Set_string password, "password to use"
    ] (fun _ -> ())
    "Create the default set of SRs on a server";

  let rpc xml = Xmlrpcclient.do_xml_rpc ~version:"1.0" 
    ~host:!host ~port:!port ~path:"/" xml in
  let session_id = Client.Session.login_with_password ~rpc 
    ~uname:!username ~pwd:!password ~version:Xapi_globs.api_version_string in
  create_storage_localhost rpc session_id;
  Client.Session.logout ~rpc ~session_id
