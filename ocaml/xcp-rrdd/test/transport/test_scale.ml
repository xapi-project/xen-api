open Test_common

let shared_file_count = ref 4096

let sync_string = "ready"

let send_ready sock =
  let chan = Unix.out_channel_of_descr sock in
  Printf.fprintf chan "%s\n" sync_string ;
  flush chan

let sync sock =
  let chan = Unix.in_channel_of_descr sock in
  if input_line chan = sync_string then
    ()
  else
    failwith "Didn't get the synchronisation string from the child"

type delivery = {shared_file: string; payload: Rrd_protocol.payload}

let read_payloads deliveries protocol sock =
  try
    let open Rrd_reader in
    sync sock ;
    print_endline "Reading payloads" ;
    print_int 0 ;
    let readers =
      List.mapi
        (fun index {shared_file; payload} ->
          let reader = FileReader.create shared_file protocol in
          let received_payload = reader.read_payload () in
          assert_payloads_equal payload received_payload ;
          Printf.printf "\r%d%!" (index + 1) ;
          reader
        )
        deliveries
    in
    print_newline () ;
    print_endline "Payloads read" ;
    print_endline "Cleaning up readers" ;
    List.iter (fun reader -> reader.cleanup ()) readers ;
    print_endline "Readers cleaned up" ;
    send_ready sock ;
    sync sock ;
    print_endline "Reader process done"
  with e ->
    List.iter
      (fun delivery ->
        try Unix.unlink delivery.shared_file
        with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      )
      deliveries ;
    raise e

let write_payloads deliveries protocol sock =
  let open Rrd_writer in
  print_endline "Writing payloads" ;
  print_int 0 ;
  let writers =
    List.mapi
      (fun index {shared_file; payload} ->
        let id = {path= shared_file; shared_page_count= 1} in
        let _, writer = FileWriter.create id protocol in
        writer.write_payload payload ;
        Printf.printf "\r%d%!" (index + 1) ;
        writer
      )
      deliveries
  in
  print_newline () ;
  print_endline "Payloads written" ;
  send_ready sock ;
  sync sock ;
  print_endline "Cleaning up writers" ;
  List.iter (fun writer -> writer.cleanup ()) writers ;
  print_endline "Writers cleaned up" ;
  send_ready sock ;
  print_endline "Writer process done"

let run_tests shared_file_count protocol =
  Random.self_init () ;
  let timestamp = Unix.gettimeofday () in
  let deliveries =
    List.init shared_file_count (fun k ->
        {
          shared_file= make_shared_file ~k ()
        ; payload= make_random_payload timestamp (Random.int 4)
        }
    )
  in
  let reader_sock, writer_sock = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  match Unix.fork () with
  | 0 ->
      (* Child - we will be the writer. *)
      Unix.close reader_sock ;
      write_payloads deliveries protocol writer_sock
  | _child_pid ->
      (* Parent - we will be the reader. *)
      Unix.close writer_sock ;
      read_payloads deliveries protocol reader_sock

let tests =
  Test_common.tests_for_all_protos
    [("Write and read", run_tests !shared_file_count)]

let () = Alcotest.run "Metrics scalability" tests
