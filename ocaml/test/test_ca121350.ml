open OUnit
open Test_common

let setup_fixture () =
	Printexc.record_backtrace true;
	Pool_role_shared.set_pool_role_for_test ();

	let __context = make_test_database () in
	let self = make_host ~__context () in

	Db.Host.set_edition ~__context ~self ~value:"foobar";

	(__context, self)

let test_invalid_edition () =
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
	skip_if true "Needs http-svr";

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

	try
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
