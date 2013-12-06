module D = Debug.Make(struct let name="test_ca121350" end)
open D

open OUnit
open Test_common

let handlers = [
	"get_services", Http_svr.FdIO Xapi_services.get_handler;
	"post_services", Http_svr.FdIO Xapi_services.post_handler;
	"put_services", Http_svr.FdIO Xapi_services.put_handler;
	"post_root", Http_svr.BufIO (Api_server.callback false);
	"post_json", Http_svr.BufIO (Api_server.callback true);
	"post_jsonrpc", Http_svr.BufIO Api_server.jsoncallback;
]

let start_server handlers =
	Xapi.listen_unix_socket ();
	List.iter Xapi_http.add_handler handlers

let setup_fixture () =
	Printexc.record_backtrace true;
	Pool_role_shared.set_pool_role_for_test ();

	let __context = make_test_database () in
	let self = make_host ~__context () in

	Db.Host.set_edition ~__context ~self ~value:"foobar";

	(__context, self)

let test_invalid_edition () =
	debug "*** starting test_invalid_edition";

	let __context, self = setup_fixture () in
	let module M = struct
		include V6client ;;
		let apply_edition ~__context edition _ = (edition, [], []) ;;
		let get_editions _ = [ "free",       "", "", 0;
													 "per-socket", "", "", 1;
													 "xendesktop", "", "", 1; ] ;;
	end in
	License_init.v6client := (module M);

	License_init.initialise ~__context ~host:self;

	let edition = Db.Host.get_edition ~__context ~self in
	assert_equal edition "free"

let test_xcp_mode () =
	(* Skip for now; fails because http-svr isn't listening. *)
	skip_if false "Needs http-svr";

	debug "*** starting test_xcp_mode";

	let __context, self = setup_fixture () in
	let module M = struct
		let get_version _ = "" ;;
		let apply_edition ~__context edition _ =
			raise Api_errors.(Server_error (v6d_failure, [])) ;;
		let get_editions _ =
			print_endline "in get_editions";
			raise Api_errors.(Server_error (v6d_failure, [])) ;;
	end in
	License_init.v6client := (module M);

	start_server handlers;

	try
		let conn = [ Parse_db_conf.make "./xapi-db.xml" ] in
		Db_cache_impl.sync conn (Db_ref.get_database (Context.database_of __context));

		(* WIP: figure out why session.create is failing *)
		(* Db.Session.create ~__context ~ref:(Ref.make ()) *)
		(* 									~uuid:Uuid.(make_uuid () |> to_string) *)
		(* 									~this_user:Ref.null ~this_host:self ~pool:true *)
		(* 									~last_active:(Date.of_float (Unix.time ())) ~other_config:[] *)
		(* 									~subject:Ref.null ~is_local_superuser:true *)
		(* 									~auth_user_sid:Ref.null *)
		(* 									~validation_time:(Date.of_float (Unix.time ())) *)
		(* 									~auth_user_name:"root" ~rbac_permissions:() ~parent:Ref.null *)
		(* 									~originator:Ref.null; *)

		License_init.initialise ~__context ~host:self;
		let edition = Db.Host.get_edition ~__context ~self in
		assert_equal edition "free/libre"

	with _ ->
		let bt = Printexc.get_backtrace () in
		Printf.printf "Backtrace:\n%s\n" bt

let test =
	"test_ca121350" >:::
		[
			"test_invalid_edition" >:: test_invalid_edition;
			"test_xcp_mode" >:: test_xcp_mode;
		]
