open Lwt

let req = "<?xml version=\"1.0\"?><methodCall><methodName>session.login_with_password</methodName><params><param><value><string>root</string></value></param><param><value><string>xenroot</string></value></param></params></methodCall>"

module Client=Client.ClientF(Lwt)

let testrpc x = 
	let open XmlHttpRequest in
		Post.post_xml "http://st11.uk.xensource.com/" (Xml.to_string x) >>= fun r ->
			Lwt.return (Xml.parse_string r.content)

let _ =
	Client.Session.login_with_password testrpc "root" "xenroot" "1.0" >>= fun x ->
	Client.VM.get_all_records testrpc x >>= fun l ->
	Firebug.console##log (Js.string (Printf.sprintf "Length=%d" (List.length l)));
	Lwt.return ()

