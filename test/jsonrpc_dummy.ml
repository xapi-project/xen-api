let path = Sys.argv.(1)

let _ =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> Unix.unlink path; exit 0));
  Unix.establish_server (fun fin fout ->
      let rec loop () =
        let json = Jsonrpc_client.input_json_object fin in
        Printf.printf "Received: %s\n" json;
        let response = Jsonrpc.string_of_response ~version:Jsonrpc.V2 (Rpc.success (Rpc.String "Thanks!")) in
        Printf.printf "Response: %s\n" response;
        output_string fout response
      in
      loop ()
    ) (Unix.ADDR_UNIX path)
