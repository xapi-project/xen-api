let _ =
  let module Daemon = Xapi_stdext_unix.Unixext.Daemon in
  let notify_test () =
    if Daemon.systemd_notify Daemon.State.Ready then
      exit 0
    else
      match Sys.getenv_opt "NOTIFY_SOCKET" with
      | Some _ ->
          exit 4
      | None ->
          print_endline "NOTIFY_SOCKET not set, notification couldn't be sent" ;
          exit 1
  in
  let server () =
    let temp_path =
      match Sys.getenv_opt "NOTIFY_SOCKET" with Some a -> a | None -> exit 4
    in
    let socket_path =
      if String.starts_with ~prefix:"@" temp_path then (
        print_endline temp_path ;
        "\x00" ^ String.sub temp_path 1 (String.length temp_path - 1)
      ) else
        temp_path
    in
    Unix.(
      let sock = socket PF_UNIX SOCK_DGRAM 0 ~cloexec:true in
      bind sock (ADDR_UNIX socket_path) ;
      let b = Bytes.create 1024 in
      let i, _ = recvfrom sock b 0 1024 [] in
      print_endline (Bytes.sub_string b 0 i) ;
      close sock
    )
  in
  let booted_test () =
    if Daemon.systemd_booted () then (
      print_endline "Booted with systemd" ;
      exit 0
    ) else (
      print_endline "Booted without systemd" ;
      exit 2
    )
  in
  Arg.parse
    [
      ("--notify", Arg.Unit notify_test, "Test systemd_notify function")
    ; ("--server", Arg.Unit server, "Listen for the notifications")
    ; ("--booted", Arg.Unit booted_test, "Test systemd_booted function")
    ]
    (fun _ -> raise (Arg.Bad "Specify a valid option"))
    "Unit test for the Daemon module in Xapi_stdext_unix\nUsage:\n"
