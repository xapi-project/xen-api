
(** This module tests more layers of xapi than other tests by using the Client
    module to make XenAPI calls. It tests many of the auto-generated files as
    these XenAPI calls go through the client, server.ml, message forwarding,
    and database layers. *)

let setup_test () =
  let __context = Test_common.make_test_database () in
  Test_common.make_client_params ~__context

(* Here we should have a unit test for each different type of method, such as
   X.create, X.destroy, getters, and setters, to ensure that these are
   generated correctly.
   We use the Task class for these tests because it is simple enough that we do
   not have to mock the storage or any other layers. *)

let test_create () =
  let rpc, session_id = setup_test () in
  let self = Client.Client.Task.create ~rpc ~session_id ~label:"task_label" ~description:"task_description" in
  Alcotest.(check string) "task label"
    "task_label"
    (Client.Client.Task.get_name_label ~rpc ~session_id ~self)

let test_get_alls () =
  let rpc, session_id = setup_test () in
  let _t1 = Client.Client.Task.create ~rpc ~session_id ~label:"t1" ~description:"" in
  let _t2 = Client.Client.Task.create ~rpc ~session_id ~label:"t2" ~description:"" in
  let _t3 = Client.Client.Task.create ~rpc ~session_id ~label:"t3" ~description:"" in
  let _t4 = Client.Client.Task.create ~rpc ~session_id ~label:"t4" ~description:"" in
  let labels =
    Client.Client.Task.get_all ~rpc ~session_id
    |> List.map (fun self -> Client.Client.Task.get_name_label ~rpc ~session_id ~self)
  in
  Alcotest.(check (slist string String.compare)) "task labels from get_all"
    ["t1";"t2";"t3";"t4"]
    labels;
  let labels =
    Client.Client.Task.get_all_records ~rpc ~session_id
    |> List.map (fun (_, r) -> r.API.task_name_label)
  in
  Alcotest.(check (slist string String.compare)) "task labels from get_all_records"
    ["t1";"t2";"t3";"t4"]
    labels

let test =
  [ "test_create", `Quick, test_create
  ; "test_get_alls", `Quick, test_get_alls
  ]
