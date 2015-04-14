open OUnit
open Test_common

let get_shared_file_names shared_file_count =
	let rec aux acc = function
		| 0 -> acc
		| shared_file_count ->
			aux (make_shared_file () :: acc) (shared_file_count - 1)
	in
	aux [] shared_file_count

let sync_string = "ready"

let send_ready sock =
	let chan = Unix.out_channel_of_descr sock in
	Printf.fprintf chan "%s\n" sync_string;
	flush chan

let sync sock =
	let chan = Unix.in_channel_of_descr sock in
	if input_line chan = sync_string
	then ()
	else failwith "Didn't get the synchronisation string from the child"

let read_payloads shared_file_names protocol sock =
	let open Rrd_reader in
	sync sock;
	print_endline "Reading payloads";
	print_int 0;
	let readers =
		List.mapi
			(fun index file_name ->
				Printf.printf "\r%d%!" (index + 1);
				let reader = FileReader.create file_name protocol in
				let received_payload = reader.read_payload() in
				assert_payloads_equal test_payload received_payload;
				reader)
		shared_file_names
	in
	print_newline ();
	print_endline "Payloads read";
	print_endline "Cleaning up readers";
	List.iter (fun reader -> reader.cleanup ()) readers;
	print_endline "Readers cleaned up";
	send_ready sock;
	print_endline "Reader process done"

let write_payloads shared_file_names protocol sock =
	let open Rrd_writer in
	print_endline "Writing payloads";
	print_int 0;
	let writers =
		List.mapi
			(fun index file_name ->
				Printf.printf "\r%d%!" (index + 1);
				let id = {path = file_name; shared_page_count = 1} in
				let _, writer = FileWriter.create id protocol in
				writer.write_payload test_payload;
				writer)
			shared_file_names
	in
	print_newline ();
	print_endline "Payloads written";
	send_ready sock;
	sync sock;
	print_endline "Cleaning up writers";
	List.iter (fun writer -> writer.cleanup ()) writers;
	print_endline "Writers cleaned up";
	print_endline "Writer process done"

let run_tests shared_file_count protocol =
	let shared_file_names = get_shared_file_names shared_file_count in
	let reader_sock, writer_sock = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
	match Unix.fork () with
	| 0 ->
		(* Child - we will be the writer. *)
		Unix.close reader_sock;
		write_payloads shared_file_names protocol writer_sock
	| child_pid ->
		(* Parent - we will be the reader. *)
		Unix.close writer_sock;
		read_payloads shared_file_names protocol reader_sock

let () =
	let open Rrd_interface in
	let shared_file_count = ref 4096 in
	let protocol = ref V2 in
	Arg.parse
		[
			"-n", Arg.Set_int shared_file_count, "Number of shared files to use";
			"-p", Arg.Int (function
				| 1 -> protocol := V1
				| 2 -> protocol := V2
				| _ -> failwith "Unrecognised protocol"),
				"Protocol to use";
		]
		(fun _ -> ())
		((Filename.basename Sys.executable_name) ^ " [-n <shared-file-count>]");
	if !shared_file_count < 0
	then failwith "I cannot use fewer than 0 shared files!";
	print_endline "------ Scale tests ------";
	Printf.printf "Shared files: %d\n" !shared_file_count;
	Printf.printf "Protocol: V%d\n" (match !protocol with | V1 -> 1 | V2 -> 2);
	print_newline ();
	run_tests
		!shared_file_count
		(match !protocol with
			| V1 -> Rrd_protocol_v1.protocol
			| V2 -> Rrd_protocol_v2.protocol)
