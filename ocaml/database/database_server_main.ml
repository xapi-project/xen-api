open Threadext

type mode = 
	| Slave of string (* master IP *)
	| Master of string (* database filename *)

let mode = ref None

let finished = ref false
let m = Mutex.create ()
let c = Condition.create ()

(** Handler for the remote database access URL *)
let remote_database_access_handler_v1 req bio = 
	try
		Db_remote_cache_access_v1.handler req bio
	with e ->
		Printf.printf "Caught: %s\n" (Printexc.to_string e);
		Printexc.print_backtrace stdout;
		flush stdout;
		raise e

(** Handler for the remote database access URL *)
let remote_database_access_handler_v2 req bio = 
	try
		Db_remote_cache_access_v2.handler req bio
	with e ->
		Printf.printf "Caught: %s\n" (Printexc.to_string e);
		Printexc.print_backtrace stdout;
		flush stdout;
		raise e

let _ = 
	let listen_path = ref "./database" in
	Printexc.record_backtrace true;

	Arg.parse [ 
		"--slave-of", Arg.String (fun master -> mode := Some(Slave master)), "run as a slave of a remote db";
		"--master", Arg.String (fun db -> mode := Some(Master db)), "run as a master from the given db filename";
		"--listen-on", Arg.Set_string listen_path, Printf.sprintf "listen for requests on path (default %s)" !listen_path;
		] (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n%!" x)
		"run a stand-alone database server";

	match !mode with
		| None -> failwith "Requires either --slave-of or --master arguments"
		| Some mode ->
			begin match mode with
				| Slave _ -> failwith "unimplemented"
				| Master db_filename ->
					Printf.printf "Database path: %s\n%!" db_filename;
					let db = { Parse_db_conf.dummy_conf with
						Parse_db_conf.path = db_filename
					} in
					Db_conn_store.initialise_db_connections [ db ];
					Printf.printf "About to create new dbs\n%!";
					List.iter (Db_connections.maybe_create_new_db (0,0)) (Db_conn_store.read_db_connections());
					Printf.printf "dbs created\n%!";
					Db_cache.set_master true;
					Db_dirty.make_blank_dirty_records();

					Db_cache_impl.initialise ();

					Unixext.unlink_safe !listen_path;
					let sockaddr = Unix.ADDR_UNIX !listen_path in
					let socket = Http_svr.bind sockaddr in

					Http_svr.add_handler Http.Post "/post_remote_db_access" (Http_svr.BufIO remote_database_access_handler_v1);
					Http_svr.add_handler Http.Post "/post_remote_db_access_v2" (Http_svr.BufIO remote_database_access_handler_v2);
					let server = Http_svr.start (socket, "http") in
					Printf.printf "server listening\n%!";
				    (* Wait for either completion *)
					Mutex.execute m
						(fun () ->
							while not (!finished) do
                                Condition.wait c m
							done
						);
					Http_svr.stop server
			end
	
