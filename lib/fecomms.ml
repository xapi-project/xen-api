open Xapi_stdext_unix
open Fe

let open_unix_domain_sock () =
  Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0

let open_unix_domain_sock_server path =
  Unixext.mkdir_rec (Filename.dirname path) 0o755;
  Unixext.unlink_safe path;
  let sock = open_unix_domain_sock () in
  try
    Unix.bind sock (Unix.ADDR_UNIX path);
    Unix.listen sock 5;
    sock
  with e ->
    Unix.close sock;
    raise e

let open_unix_domain_sock_client path =
  let sock = open_unix_domain_sock () in
  try 
    Unix.connect sock (Unix.ADDR_UNIX path);
    sock
  with e ->
    Unix.close sock;
    raise e

let read_raw_rpc sock =
  let buffer = String.make 12 '\000' in
  Unixext.really_read sock buffer 0 12;
  let len = int_of_string buffer in
  let body = Unixext.really_read_string sock len in
  ferpc_of_rpc (Jsonrpc.of_string body)

let write_raw_rpc sock ferpc =
  let body = Jsonrpc.to_string (rpc_of_ferpc ferpc) in
  let len = String.length body in
  let buffer = Printf.sprintf "%012d%s" len body in
  Unixext.really_write_string sock buffer

exception Connection_closed

let receive_named_fd sock =
  let buffer = String.make 36 '\000' in
  let (len,_from,newfd) = Unixext.recv_fd sock buffer 0 36 [] in  
  if len=0 then raise Connection_closed;
  (newfd,buffer)

let send_named_fd sock uuid fd =
  ignore(Unixext.send_fd sock uuid 0 (String.length uuid) [] fd)


