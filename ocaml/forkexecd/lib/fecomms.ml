module Unixext = Xapi_stdext_unix.Unixext

let open_unix_domain_sock () = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0

let open_unix_domain_sock_server path =
  Unixext.mkdir_rec (Filename.dirname path) 0o755 ;
  Unixext.unlink_safe path ;
  let sock = open_unix_domain_sock () in
  try
    Unix.bind sock (Unix.ADDR_UNIX path) ;
    Unix.listen sock 5 ;
    sock
  with e -> Unix.close sock ; raise e

let open_unix_domain_sock_client path =
  let sock = open_unix_domain_sock () in
  try
    Unix.connect sock (Unix.ADDR_UNIX path) ;
    sock
  with e -> Unix.close sock ; raise e

let read_raw_rpc sock =
  let buffer = Bytes.make 12 '\000' in
  Unixext.really_read sock buffer 0 12 ;
  let header = Bytes.unsafe_to_string buffer in
  match int_of_string_opt header with
  | Some len ->
      let body = Unixext.really_read_string sock len in
      Ok (Fe.ferpc_of_rpc (Jsonrpc.of_string body))
  | None ->
      Unix.(shutdown sock SHUTDOWN_ALL) ;
      Error ("Header is not an integer: " ^ header)

let write_raw_rpc sock ferpc =
  let body = Jsonrpc.to_string (Fe.rpc_of_ferpc ferpc) in
  let len = String.length body in
  let buffer = Printf.sprintf "%012d%s" len body in
  Unixext.really_write_string sock buffer

exception Connection_closed

let receive_named_fd sock =
  let buffer = Bytes.make 36 '\000' in
  let len, _from, newfd = Unixext.recv_fd sock buffer 0 36 [] in
  let buffer = Bytes.unsafe_to_string buffer in
  if len = 0 then raise Connection_closed ;
  (newfd, buffer)

let send_named_fd sock uuid fd =
  ignore (Unixext.send_fd_substring sock uuid 0 (String.length uuid) [] fd)
