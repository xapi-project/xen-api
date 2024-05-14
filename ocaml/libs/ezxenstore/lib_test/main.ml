(* A place to put client library tests *)

open Cmdliner

let set_socket_path path = Xs_transport.xenstored_socket := path

let test socket =
  set_socket_path socket ;
  let open Ezxenstore_core.Xenstore in
  if Unix.geteuid () <> 0 then (* non-root won't have access to xenstore *)
    `Ok 0
  else
    try
      let result = with_xs (fun xs -> xs.write "/foo" "bar" ; xs.read "/foo") in
      if result = "bar" then
        `Ok 0
      else
        `Error (false, "argh")
    with Xs_transport.Could_not_find_xenstore ->
      (* Do not fail on systems that don't have xenstore running *)
      `Ok 0

let socket =
  let doc = "Set the path to the xenstored socket" in
  Arg.(value & opt string "/var/run/xenstored/socket" & info ["s"] ~doc)

let cmd =
  let doc = "Test the ezxenstore library" in
  let man =
    [
      `P
        "Requires a running xenstored to work. See \
         github.com:mirage/ocaml-xenstore-server"
    ]
  in
  Cmd.v (Cmd.info "test_ezxenstore" ~doc ~man) Term.(ret (const test $ socket))

let () = exit (Cmd.eval' cmd)
