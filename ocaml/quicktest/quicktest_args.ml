(** Module for parsing and accessing the legacy quicktest command line arguments *)

let xe_path = ref "/opt/xensource/bin/xe"

let use_default_sr = ref false

let use_colour = ref true

let run_only = ref None

let list_tests = ref false

let username = ref ""

let password = ref ""

let host = ref ""

let sr = ref ""

let using_unix_domain_socket = ref true

let http = Xmlrpc_client.xmlrpc ~version:"1.1" "/"

let update_http http =
  let headers = Quicktest_trace_rpc.RPC.http_headers () in
  Http.Request.
    {
      http with
      additional_headers= List.rev_append headers http.additional_headers
    }

let rpc_remote xml =
  let http = update_http http in
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
  let http = update_http http in
  Xmlrpc_client.XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:(Unix Xapi_globs.unix_domain_socket) ~http xml

let rpc = ref rpc_unix_domain

let alcotest_args = ref [||]

let set_alcotest_args l = alcotest_args := Array.of_list l

let skip_xapi = ref false

let skip_stress = ref false

(** Whether a test suite depends on the SR, or runs identically regardless
    of it. Used by [-with-tag]/[-without-tag] to select suites. A suite
    that only ever exercises the pool's default SR (rather than the SR
    passed via [-sr]) should not carry [Sr]: its result cannot change when
    [-sr] is varied, even though it still touches storage. *)
type tag = Sr

let tag_to_str = function Sr -> "sr"

let tag_of_str = function
  | "sr" ->
      Sr
  | s ->
      raise (Arg.Bad (Printf.sprintf "unknown tag %S" s))

let with_tags = ref []

let without_tags = ref []

(** All tags, listed by -list-tags *)
let all_tags = [Sr]

let tag_to_description = function
  | Sr ->
      "Suite's result depends on the SR passed via -sr"

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
      , Arg.Set use_default_sr
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
    ; ("-skip-stress", Arg.Set skip_stress, "SKIP randomized stress tests")
    ; ("--", Arg.Rest_all set_alcotest_args, "Supply alcotest arguments")
    ; ( "-run-only"
      , Arg.String (fun x -> run_only := Some x)
      , "Only run specified tests, skip all others. Several tests can be \
         specified, separated by commas"
      )
    ; ( "-with-tag"
      , Arg.String (fun s -> with_tags := tag_of_str s :: !with_tags)
      , "Only run test suites carrying this tag. May be repeated. Can be \
         combined with -run-only. Recognised tags: sr"
      )
    ; ( "-without-tag"
      , Arg.String (fun s -> without_tags := tag_of_str s :: !without_tags)
      , "Exclude test suites carrying this tag. May be repeated. Can be \
         combined with -run-only. Recognised tags: sr"
      )
    ; ( "-list-tags"
      , Arg.Unit
          (fun () ->
            List.iter
              (fun t ->
                Printf.printf "%s: %s\n" (tag_to_str t) (tag_to_description t)
              )
              all_tags ;
            exit 0
          )
      , "List all recognised tags"
      )
    ; ( "-list-tests"
      , Arg.Set list_tests
      , "Lists test names as they are consumed by -run-only"
      )
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
  let colour =
    if not !use_colour then
      [|"--color=never"|]
    else
      [||]
  in
  Array.concat [name; colour; !alcotest_args]
