(** Module for parsing and accessing the legacy quicktest command line arguments *)

let xe_path = ref "/opt/xensource/bin/xe"

let use_default_sr = ref false

let use_colour = ref true

let username = ref ""

let password = ref ""

let host = ref ""

let sr = ref ""

let using_unix_domain_socket = ref true

let http = Xmlrpc_client.xmlrpc ~version:"1.1" "/"

let rpc_remote xml =
  Xmlrpc_client.XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:
      (SSL
         ( Xmlrpc_client.SSL.make ~verify_cert:(Stunnel_client.pool ()) ()
         , !host
         , 443
         )
      )
    ~http xml

let rpc_unix_domain xml =
  Xmlrpc_client.XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:(Unix Xapi_globs.unix_domain_socket) ~http xml

let rpc = ref rpc_unix_domain

let alcotest_args = ref [||]

let set_alcotest_args l = alcotest_args := Array.of_list l

let skip_xapi = ref false

(** Parse the legacy quicktest command line args. This is used instead of
    invoking Alcotest directly, for backwards-compatibility with clients who
    run the quicktest binary. *)
let parse () =
  Arg.parse
    [
      ( "-xe-path"
      , Arg.String (fun x -> xe_path := x)
      , "Path to xe command line executable"
      )
    ; ( "-default-sr"
      , Arg.Unit (fun () -> use_default_sr := true)
      , "Only run SR tests on the pool's default SR, mutually exclusive with \
         -sr"
      )
    ; ("-nocolour", Arg.Clear use_colour, "Don't use colour in the output")
    ; ( "-sr"
      , Arg.String (fun x -> sr := x)
      , "Only run SR tests on the specified SR, mutually exclusive with \
         -default-sr"
      )
    ; ("-skip-xapi", Arg.Set skip_xapi, "SKIP tests that require XAPI")
    ; ("--", Arg.Rest_all set_alcotest_args, "Supply alcotest arguments")
    ]
    (fun x ->
      match (!host, !username, !password) with
      | "", _, _ ->
          host := x ;
          rpc := rpc_remote ;
          using_unix_domain_socket := false
      | _, "", _ ->
          username := x
      | _, _, "" ->
          password := x
      | _, _, _ ->
          Printf.fprintf stderr "Skipping unrecognised argument: %s" x
    )
    "Perform some quick functional tests. The default is to test localhost \
     over a Unix socket. For remote server supply <hostname> <username> and \
     <password> arguments." ;
  if !use_default_sr && !sr <> "" then
    raise (Arg.Bad "-default-sr and -sr are mutually exclusive") ;
  if !host = "" then host := "localhost" ;
  if !username = "" then username := "root"

(** Translate from legacy quicktest command line args to Alcotest's args *)
let get_alcotest_args () =
  let name = [|Sys.argv.(0)|] in
  let colour = if not !use_colour then [|"--color=never"|] else [||] in
  Array.concat [name; colour; !alcotest_args]
