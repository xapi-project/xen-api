let _ =
  let inet_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 9411) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock inet_addr ;
  Unix.listen sock 1 ;
  let rec read_write_all ic file =
    match input_line ic with
    | line ->
        Printf.fprintf file "\n%s" line ;
        read_write_all ic file
    | exception End_of_file ->
        ()
  in
  match Unix.fork () with
  | 0 ->
      let fd, _ = Unix.accept sock in
      let ic = Unix.in_channel_of_descr fd in
      let file = open_out "test-http-server.out" in
      read_write_all ic file
  | _ ->
      ()
