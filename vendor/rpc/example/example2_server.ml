open Idl
open Example2_idl

module Server=API(GenServer ())

(* Implementations of the methods *)
let query () =
  let open Datatypes.Query in
  Printf.printf "Received query API call\n%!";
  let result = {
    name = "Example2 server";
    vendor = "This is the example server showing how to use the ocaml-rpc IDL";
    version = "2.0.0";
    features = ["defaults";"upgradability"];
    instance_id = string_of_int (Random.int 1000)
  } in
  Result.Ok result

let diagnostics () =
  Result.Ok "This should be the diagnostics of the server"

let test i s1 s2 =
  Printf.printf "%Ld %s %s\n%!" i s1 s2;
  query ()

(* Utility and general non-specific server bits and bobs *)
let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let mkdir_rec dir perm =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <> "/" && p_name <> "."
    then p_mkdir p_name;
    (try Unix.mkdir dir perm  with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
  p_mkdir dir

let binary_handler process s =
  let ic = Unix.in_channel_of_descr s in
  let oc = Unix.out_channel_of_descr s in
  (* Read a 16 byte length encoded as a string *)
  let len_buf = Bytes.make 16 '\000' in
  really_input ic len_buf 0 (Bytes.length len_buf);
  let len = int_of_string (Bytes.unsafe_to_string len_buf) in
  let msg_buf = Bytes.make len '\000' in
  really_input ic msg_buf 0 (Bytes.length msg_buf);
  let result = process msg_buf in
  let len_buf = Printf.sprintf "%016d" (String.length result) in
  output_string oc len_buf;
  output_string oc result;
  flush oc

let serve_requests rpcfn path =
  (try Unix.unlink path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  mkdir_rec (Filename.dirname path) 0o0755;
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_UNIX path);
  Unix.listen sock 5;
  Printf.fprintf stdout "Listening on %s" path;
  while true do
    let this_connection, _ = Unix.accept sock in
    let (_: Thread.t) = Thread.create
        (fun () ->
          finally
            (fun () -> binary_handler rpcfn this_connection)
            (fun () -> Unix.close this_connection)
        ) () in
    ()
  done

let start_server () =
  let open Rresult in

  Server.query query;
  Server.diagnostics diagnostics;
  Server.test test;

  let rpc_fn = server Server.implementation in

  let process x =
    Jsonrpc.string_of_response (rpc_fn (Jsonrpc.call_of_string (Bytes.unsafe_to_string x))) in

  serve_requests process sockpath
