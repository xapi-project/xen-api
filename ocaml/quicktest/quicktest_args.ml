(** Module for parsing and accessing the legacy quicktest command line arguments *)

let xe_path = ref "/opt/xensource/bin/xe"
let iso_path = ref "/opt/xensource/packages/iso"
let use_default_sr = ref false

let use_colour = ref true

let username = ref ""
let password = ref ""

let host = ref ""

let using_unix_domain_socket = ref true

let http = Xmlrpc_client.xmlrpc ~version:"1.1" "/"

let rpc_remote xml =
  Xmlrpc_client.XMLRPC_protocol.rpc
    ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:(SSL(Xmlrpc_client.SSL.make(), !host, 443))
    ~http xml

let rpc_unix_domain xml =
  Xmlrpc_client.XMLRPC_protocol.rpc
    ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:(Unix Xapi_globs.unix_domain_socket)
    ~http xml

let rpc = ref rpc_unix_domain

(** Parse the legacy quicktest command line args. This is used instead of
    invoking Alcotest directly, for backwards-compatibility with clients who
    run the quicktest binary. *)
let parse () =
  Arg.parse [
    "-xe-path", Arg.String (fun x -> xe_path := x), "Path to xe command line executable";
    "-iso-sr-path", Arg.String (fun x -> iso_path := x), "Path to ISO SR";
    "-default-sr", Arg.Unit (fun () -> use_default_sr := true), "Only run SR tests on the pool's default SR";
    "-nocolour", Arg.Clear use_colour, "Don't use colour in the output" ]
    (fun x -> match !host, !username, !password with
       | "", _, _ -> host := x; rpc := rpc_remote; using_unix_domain_socket := false;
       | _, "", _ -> username := x
       | _, _, "" -> password := x
       | _, _, _ -> Printf.fprintf stderr "Skipping unrecognised argument: %s" x)
    "Perform some quick functional tests. The default is to test localhost over a Unix socket. For remote server supply <hostname> <username> and <password> arguments.";
  if !host = "" then host := "localhost";
  if !username = "" then username := "root"

(** Translate from legacy quicktest command line args to Alcotest's args *)
let get_alcotest_args () =
  let name = [|Array.get Sys.argv 0|] in
  let colour =
    if not !use_colour then [|"--color=never"|] else [||]
  in
  Array.concat [name; colour]
