let json_url () = "file:" ^ Cluster_interface.json_path

let json_http_rpc = Xcp_client.http_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

let rpc url call =
  if !Xcp_client.use_switch
  then Xcp_client.json_switch_rpc Cluster_interface.queue_name call
  else json_http_rpc ~srcstr:"clusterd" ~dststr:"clusterd" url call

(* There is also a Remote API between clustering daemons on different hosts.
 * Call this a Local API because it is an API inside a host *)
module LocalClient = Cluster_interface.LocalAPI(Idl.GenClient ())
