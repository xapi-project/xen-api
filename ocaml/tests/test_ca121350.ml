module D = Debug.Make(struct let name="test_ca121350" end)
open D

open OUnit
open Test_common

let setup_fixture () =
  let __context = make_test_database () in
  let self = make_host ~__context () in
  (__context, self)

let test_invalid_edition () =
  debug "*** starting test_invalid_edition";

  let __context, self = setup_fixture () in
  let module M = struct
    include V6_client ;;
    let apply_edition _ edition _ = V6_interface.{
        edition = edition;
        xapi_params = [];
        additional_params = [];
        experimental_features = [];
      } ;;
    let get_editions _ = [
      "free", ("", "", 0);
      "per-socket", ("", "", 1);
      "xendesktop", ("", "", 1);
    ] ;;
  end in
  License_init.v6client := (module M);

  Db.Host.set_edition ~__context ~self ~value:"foobar";

  License_init.initialise ~__context ~host:self;

  let edition = Db.Host.get_edition ~__context ~self in
  assert_equal edition "free"

let test_xcp_mode () =
  debug "*** starting test_xcp_mode";

  let __context, self = setup_fixture () in
  let module M = struct
    include V6_client ;;
    let get_version _ = "" ;;
    let apply_edition _ =
      raise Api_errors.(Server_error (v6d_failure, [])) ;;
    let get_editions _ =
      raise Api_errors.(Server_error (v6d_failure, [])) ;;
  end in
  License_init.v6client := (module M);

  try

    Server_helpers.exec_with_new_task "test_ca121350"
      (fun __context ->
         License_init.initialise ~__context ~host:self;
         let edition = Db.Host.get_edition ~__context ~self in
         assert_equal edition "free/libre");

    Mock.Database.flush __context

  with e ->
    let bt = Printexc.get_backtrace () in
    Printf.printf "Backtrace:\n%s\n" bt;
    raise e

let test =
  "test_ca121350" >:::
  [
    "test_invalid_edition" >:: test_invalid_edition;
    "test_xcp_mode" >:: test_xcp_mode;
  ]
