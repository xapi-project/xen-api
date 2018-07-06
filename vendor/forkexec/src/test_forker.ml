
let _ = 
  let die_at = int_of_string Sys.argv.(1) in
  let sock = Fecomms.open_unix_domain_sock_client "/var/xapi/forker/main" in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Printf.fprintf stderr "About to write raw rpc\n%!";
  Fecomms.write_raw_rpc sock (Fe.Setup {Fe.cmdargs=["/bin/fecho";"hello";"test"]; id_to_fd_map = [(uuid,Some (Unixext.int_of_file_descr Unix.stdout))]; env=[]});
  if die_at=1 then exit(1);
  Printf.fprintf stderr "Done write raw rpc\n%!";
  let response = Fecomms.read_raw_rpc sock in
  if die_at=2 then exit(1);
  Printf.fprintf stderr "Got response\n%!";
  match response with
  | Fe.Setup_response s ->
    Printf.fprintf stderr "Got response: fd_sock_path=%s\n%!" s.Fe.fd_sock_path;
    let (rd,wr) = Unix.pipe () in
    let fd_sock = Fecomms.open_unix_domain_sock_client s.Fe.fd_sock_path in
    if die_at=3 then exit(1);
    (try
       Fecomms.send_named_fd fd_sock uuid wr;
     with e -> 
       Printf.fprintf stderr "Failed to send named fd: %s%!" (Printexc.to_string e));
    if die_at=4 then exit(1);
    Unix.close wr;
    Unix.close fd_sock;
    Fecomms.write_raw_rpc sock Fe.Exec;
    if die_at=5 then exit(1);
    (match Fecomms.read_raw_rpc sock with
     | Fe.Execed pid ->
       Printf.fprintf stderr "Got pid: %d\n%!" pid);
    if die_at=6 then exit(1);
    let buffer = Buffer.create 1000 in
    let str = String.make 1000 '\000' in
    let rec consume () = 
      let len = Unix.read rd str 0 (String.length str) in
      if len=0 
      then () 
      else
        begin 
          Buffer.add_substring buffer str 0 len;
          consume ()
        end
    in 
    consume ();
    Printf.fprintf stderr "Received: %s\n%!" (Buffer.contents buffer);
    match Fecomms.read_raw_rpc sock with
    | Fe.Finished res ->
      Printf.fprintf stderr "Got finished\n%!";

