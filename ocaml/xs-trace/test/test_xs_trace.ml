let _ =
  let inet_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 9411) in
  let rec read_write_all ic file =
    match input_line ic with
    | line ->
        Printf.fprintf file "\n%s" line ;
        read_write_all ic file
    | exception End_of_file ->
        ()
  in
  Unix.establish_server
    (fun ic _ ->
      let file = open_out "test-http-server.out" in
      read_write_all ic file
    )
    inet_addr
