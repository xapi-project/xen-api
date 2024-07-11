module Client = Client.Client
module Date = Xapi_stdext_date.Date

let test_host_get_server_localtime rpc session_id () =
  let host = Client.Host.get_by_uuid ~rpc ~session_id ~uuid:Qt.localhost_uuid in
  let (_ : Date.t) = Client.Host.get_server_localtime ~rpc ~session_id ~host in
  ()

let test_message_get_since rpc session_id () =
  let test_with_format format' =
    let stdout, _ =
      Forkhelpers.execute_command_get_output "/bin/date"
        [Printf.sprintf "+%s" format'; "-d"; "yesterday"]
    in
    let yesterday = String.trim stdout |> Date.of_iso8601 in
    let (_ : ('a API.Ref.t * API.message_t) list) =
      Client.Message.get_since ~rpc ~session_id ~since:yesterday
    in
    ()
  in
  [
    "%Y-%m-%dT%H:%M:%SZ"
  ; "%Y-%m-%dT%H:%M:%S"
  ; "%Y%m%dT%H:%M:%SZ"
  ; "%Y%m%dT%H:%M:%S"
  ]
  |> List.iter test_with_format

let tests () =
  let open Qt_filter in
  [
    [("host.get_server_localtime", `Quick, test_host_get_server_localtime)]
    |> conn
  ; [("message.get_since", `Quick, test_message_get_since)] |> conn
  ]
  |> List.concat
