open Rfb
open Rfb_randomtest

let _ = 
  let port = 5902 in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.handle_unix_error (Unix.setsockopt s Unix.SO_REUSEADDR) true;
  Unix.handle_unix_error (Unix.bind s) (Unix.ADDR_INET (Unix.inet_addr_any, port));
  let port = begin match Unix.getsockname s with
    | Unix.ADDR_INET(_, port) -> port
    | _ -> failwith "Failed to discover local port"
  end in
  Printf.printf "Listening on local port %d\n" port; flush stdout;
  Unix.handle_unix_error (Unix.listen s) 5;
  let fd, _ = Unix.accept s in
  server fd
