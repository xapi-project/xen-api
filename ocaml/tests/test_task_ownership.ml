(*
 * Copyright (C) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let rbac_denied = Api_errors.rbac_permission_denied

let http_action = "put_import"

let http_permission = Datamodel.rbac_http_permission_prefix ^ http_action

let destroy_any_permission = Datamodel.Task.extra_permission_task_destroy_any

let make_session ~__context ?(rbac_permissions = []) ~auth_user_sid () =
  Test_common.make_session ~__context ~is_local_superuser:false ~auth_user_sid
    ~rbac_permissions ()

let make_task ~__context ~session_id label =
  let self = Ref.make () in
  Db.Task.create ~__context ~ref:self ~uuid:(Test_common.make_uuid ())
    ~name_label:label ~name_description:"" ~created:API.Date.epoch
    ~finished:API.Date.epoch ~status:`pending ~session:session_id
    ~resident_on:!Xapi_globs.localhost_ref ~progress:0. ~_type:"<none/>"
    ~result:"" ~error_info:[] ~allowed_operations:[] ~current_operations:[]
    ~subtask_of:Ref.null ~other_config:[] ~stunnelpid:(-1L) ~forwarded:false
    ~forwarded_to:Ref.null ~externalpid:(-1L) ~backtrace:"" ;
  self

let request ?task_id ~session_id () =
  let query =
    ("session_id", Ref.string_of session_id)
    ::
    ( match task_id with
    | None ->
        []
    | Some task_id ->
        [("task_id", Ref.string_of task_id)]
    )
  in
  Http.Request.make ~user_agent:"test_task_ownership" ~query Http.Put "/import"

let with_remote_fd f =
  (* When the [ic] parameter of [assert_credentials_ok] is a UNIX socket, it is
     trusted always. There will be no check against the request. So a TCP socket
     is created here to simulate requests from network. *)
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f fd)
    (fun () -> Unix.close fd)

let assert_credentials_ok ?task_id ?fn ~session_id () =
  with_remote_fd (fun fd ->
      Xapi_http.assert_credentials_ok "test_realm" ~http_action ?fn
        (request ?task_id ~session_id ())
        fd
  )

let expect_forbidden msg f =
  match f () with
  | () ->
      Alcotest.failf "%s: expected Http.Forbidden but the call succeeded" msg
  | exception Http.Forbidden ->
      ()
  | exception e ->
      Alcotest.failf "%s: expected Http.Forbidden but got %s" msg
        (ExnHelper.string_of_exn e)

let expect_rbac_denied msg f =
  match f () with
  | () ->
      Alcotest.failf "%s: expected %s but the call succeeded" msg
        Api_errors.rbac_permission_denied
  | exception Api_errors.Server_error (code, _)
    when code = Api_errors.rbac_permission_denied ->
      ()
  | exception e ->
      Alcotest.failf "%s: expected %s but got %s" msg
        Api_errors.rbac_permission_denied
        (ExnHelper.string_of_exn e)

let check_task_status ~__context ~task ~expected msg =
  Alcotest.(check string)
    (msg ^ ": task status")
    (TaskHelper.status_to_string expected)
    (TaskHelper.status_to_string (Db.Task.get_status ~__context ~self:task))

let check_task_untouched ~__context ~task msg =
  check_task_status ~__context ~task ~expected:`pending msg ;
  Alcotest.(check (list string))
    (msg ^ ": task error_info")
    []
    (Db.Task.get_error_info ~__context ~self:task)

let check_task_failed ~__context ~task msg =
  check_task_status ~__context ~task ~expected:`failure msg ;
  match Db.Task.get_error_info ~__context ~self:task with
  | code :: _ ->
      Alcotest.(check string) (msg ^ ": task error code") rbac_denied code
  | [] ->
      Alcotest.failf "%s: nothing recorded in the error_info of the task" msg

let test_fails_own_task () =
  let __context = Test_common.make_test_database () in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  let task = make_task ~__context ~session_id "own task" in
  expect_forbidden "own task" (fun () ->
      assert_credentials_ok ~session_id ~task_id:task ()
  ) ;
  check_task_failed ~__context ~task "own task"

let test_fails_task_of_same_user () =
  let __context = Test_common.make_test_database () in
  let owner = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  let task = make_task ~__context ~session_id:owner "task of the same user" in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  expect_forbidden "task of the same user" (fun () ->
      assert_credentials_ok ~session_id ~task_id:task ()
  ) ;
  check_task_failed ~__context ~task "task of the same user"

let test_denies_task_of_other_user () =
  let __context = Test_common.make_test_database () in
  let victim = make_session ~__context ~auth_user_sid:"S-1-victim" () in
  let task = make_task ~__context ~session_id:victim "task of the victim" in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-attacker" () in
  expect_forbidden "task of another user" (fun () ->
      assert_credentials_ok ~session_id ~task_id:task ()
  ) ;
  check_task_untouched ~__context ~task "task of another user"

let test_denies_unknown_task () =
  let __context = Test_common.make_test_database () in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  let task_id =
    Ref.of_string "OpaqueRef:12345678-1234-1234-1234-123456789012"
  in
  expect_forbidden "unknown task" (fun () ->
      assert_credentials_ok ~session_id ~task_id ()
  )

let test_allows_permitted_caller () =
  let __context = Test_common.make_test_database () in
  let session_id =
    Test_common.make_session ~__context ~is_local_superuser:true ()
  in
  let task = make_task ~__context ~session_id "own task" in
  let called = ref false in
  ( match
      assert_credentials_ok ~session_id ~task_id:task
        ~fn:(fun () -> called := true)
        ()
    with
  | () ->
      ()
  | exception e ->
      Alcotest.failf "permitted caller: expected success but got %s"
        (ExnHelper.string_of_exn e)
  ) ;
  Alcotest.(check bool) "permitted caller: handler called" true !called ;
  check_task_untouched ~__context ~task "permitted caller"

let test_denies_others_task_before_handler () =
  let __context = Test_common.make_test_database () in
  let victim = make_session ~__context ~auth_user_sid:"S-1-victim" () in
  let task = make_task ~__context ~session_id:victim "task of the victim" in
  let session_id =
    make_session ~__context ~auth_user_sid:"S-1-attacker"
      ~rbac_permissions:[http_permission] ()
  in
  let called = ref false in
  expect_forbidden "permitted caller, task of another user" (fun () ->
      assert_credentials_ok ~session_id ~task_id:task
        ~fn:(fun () -> called := true)
        ()
  ) ;
  Alcotest.(check bool)
    "permitted caller, task of another user: handler not called" false !called ;
  check_task_untouched ~__context ~task "permitted caller, task of another user"

let test_allows_others_task_for_destroy_any_holder () =
  let __context = Test_common.make_test_database () in
  let stranded = make_session ~__context ~auth_user_sid:"S-1-stranded" () in
  let task =
    make_task ~__context ~session_id:stranded "task of a stranded user"
  in
  let session_id =
    make_session ~__context ~auth_user_sid:"S-1-operator"
      ~rbac_permissions:[destroy_any_permission; http_permission]
      ()
  in
  let called = ref false in
  ( match
      assert_credentials_ok ~session_id ~task_id:task
        ~fn:(fun () -> called := true)
        ()
    with
  | () ->
      ()
  | exception e ->
      Alcotest.failf "holder of task.destroy/any: expected success but got %s"
        (ExnHelper.string_of_exn e)
  ) ;
  Alcotest.(check bool)
    "holder of task.destroy/any: handler called" true !called ;
  check_task_untouched ~__context ~task "holder of task.destroy/any"

let test_fails_others_task_for_destroy_any_holder () =
  let __context = Test_common.make_test_database () in
  let stranded = make_session ~__context ~auth_user_sid:"S-1-stranded" () in
  let task =
    make_task ~__context ~session_id:stranded "task of a stranded user"
  in
  let session_id =
    make_session ~__context ~auth_user_sid:"S-1-operator"
      ~rbac_permissions:[destroy_any_permission] ()
  in
  expect_forbidden "holder of task.destroy/any, RBAC-denied" (fun () ->
      assert_credentials_ok ~session_id ~task_id:task ()
  ) ;
  check_task_failed ~__context ~task "holder of task.destroy/any, RBAC-denied"

let check_can_own_task ?session_id ~expected ~task msg =
  Alcotest.(check bool)
    (msg ^ ": can_own_task") expected
    (Server_helpers.can_own_task ?session_id task)

let test_own_task_of_same_user () =
  let __context = Test_common.make_test_database () in
  let owner = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  let task = make_task ~__context ~session_id:owner "task of the same user" in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  check_can_own_task ~session_id ~expected:true ~task "task of the same user"

let test_not_own_task_of_other_user () =
  let __context = Test_common.make_test_database () in
  let victim = make_session ~__context ~auth_user_sid:"S-1-victim" () in
  let task = make_task ~__context ~session_id:victim "task of the victim" in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-attacker" () in
  check_can_own_task ~session_id ~expected:false ~task "task of another user"

let test_not_own_task_without_session () =
  (* The Http.Unauthorised path of Xapi_http.with_context has no session at all. *)
  let __context = Test_common.make_test_database () in
  let victim = make_session ~__context ~auth_user_sid:"S-1-victim" () in
  let task = make_task ~__context ~session_id:victim "task of the victim" in
  check_can_own_task ~expected:false ~task "no session" ;
  check_task_untouched ~__context ~task "no session"

let test_not_own_task_when_unknown () =
  let __context = Test_common.make_test_database () in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  let task = Ref.of_string "OpaqueRef:12345678-1234-1234-1234-123456789012" in
  check_can_own_task ~session_id ~expected:false ~task "unknown task"

let test_own_task_of_local_superuser () =
  let __context = Test_common.make_test_database () in
  let stranded = make_session ~__context ~auth_user_sid:"S-1-stranded" () in
  let task =
    make_task ~__context ~session_id:stranded "task of a stranded user"
  in
  let session_id =
    Test_common.make_session ~__context ~is_local_superuser:true ()
  in
  check_can_own_task ~session_id ~expected:true ~task "local superuser"

let test_own_task_of_destroy_any_holder () =
  let __context = Test_common.make_test_database () in
  let stranded = make_session ~__context ~auth_user_sid:"S-1-stranded" () in
  let task =
    make_task ~__context ~session_id:stranded "task of a stranded user"
  in
  let session_id =
    make_session ~__context ~auth_user_sid:"S-1-operator"
      ~rbac_permissions:[destroy_any_permission] ()
  in
  check_can_own_task ~session_id ~expected:true ~task
    "holder of task.destroy/any"

let request_with_task_header ?task ~session_id () =
  let req = request ~session_id () in
  match task with
  | None ->
      req
  | Some task ->
      {req with Http.Request.task= Some (Ref.string_of task)}

let dispatch ?task ?(supports_async = true) ~session_id () =
  let ran = ref false in
  let (_ : Rpc.response) =
    Server_helpers.do_dispatch ~session_id supports_async "Test.call"
      (fun ~__context:_ -> ran := true)
      (fun () -> Rpc.Null)
      None
      (request_with_task_header ?task ~session_id ())
      "test_do_dispatch" `Sync true
  in
  !ran

let check_dispatch_ran ~expected ~session_id ?task ?supports_async msg =
  Alcotest.(check bool)
    (msg ^ ": operation ran") expected
    (dispatch ?task ?supports_async ~session_id ())

let test_dispatch_denies_task_of_other_user () =
  let __context = Test_common.make_test_database () in
  let victim = make_session ~__context ~auth_user_sid:"S-1-victim" () in
  let task = make_task ~__context ~session_id:victim "task of the victim" in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-attacker" () in
  expect_rbac_denied "task-id header naming another user's task" (fun () ->
      ignore (dispatch ~task ~session_id ())
  ) ;
  check_task_untouched ~__context ~task "task-id header, another user"

let test_dispatch_allows_own_task () =
  let __context = Test_common.make_test_database () in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  let task = make_task ~__context ~session_id "task of the caller" in
  check_dispatch_ran ~expected:true ~session_id ~task
    "task-id header naming the caller's own task"

let test_dispatch_allows_local_superuser () =
  let __context = Test_common.make_test_database () in
  let originator = make_session ~__context ~auth_user_sid:"S-1-originator" () in
  let task = make_task ~__context ~session_id:originator "forwarded task" in
  let session_id =
    Test_common.make_session ~__context ~pool:true ~is_local_superuser:true ()
  in
  check_dispatch_ran ~expected:true ~session_id ~task
    "task-id header on a forwarded call"

let test_dispatch_allows_without_task_header () =
  let __context = Test_common.make_test_database () in
  let session_id = make_session ~__context ~auth_user_sid:"S-1-caller" () in
  check_dispatch_ran ~expected:true ~session_id ~supports_async:false
    "no task-id header"

let tests =
  [
    ( "Xapi_http.assert_credentials_ok: RBAC-denied task"
    , [
        ("fails the task of the caller", `Quick, test_fails_own_task)
      ; ("fails the task of the same user", `Quick, test_fails_task_of_same_user)
      ; ( "leaves the task of another user untouched"
        , `Quick
        , test_denies_task_of_other_user
        )
      ; ("rejects an unknown task", `Quick, test_denies_unknown_task)
      ; ( "leaves the task of a permitted caller untouched"
        , `Quick
        , test_allows_permitted_caller
        )
      ; ( "refuses the task of another user before running the handler"
        , `Quick
        , test_denies_others_task_before_handler
        )
      ; ( "runs the handler when a holder of task.destroy/any supplies the \
           task of another user"
        , `Quick
        , test_allows_others_task_for_destroy_any_holder
        )
      ; ( "fails the task of another user when a holder of task.destroy/any is \
           RBAC-denied"
        , `Quick
        , test_fails_others_task_for_destroy_any_holder
        )
      ]
    )
  ; ( "Server_helpers.can_own_task"
    , [
        ("owns the task of the same user", `Quick, test_own_task_of_same_user)
      ; ( "does not own the task of another user"
        , `Quick
        , test_not_own_task_of_other_user
        )
      ; ( "does not own any task without a session"
        , `Quick
        , test_not_own_task_without_session
        )
      ; ("does not own an unknown task", `Quick, test_not_own_task_when_unknown)
      ; ( "allows a local superuser to own the task of another user"
        , `Quick
        , test_own_task_of_local_superuser
        )
      ; ( "allows a holder of task.destroy/any to own the task of another user"
        , `Quick
        , test_own_task_of_destroy_any_holder
        )
      ]
    )
  ; ( "Server_helpers.do_dispatch: the task-id header"
    , [
        ( "refuses a task named by the header that belongs to another user"
        , `Quick
        , test_dispatch_denies_task_of_other_user
        )
      ; ( "runs when the header names the caller's own task"
        , `Quick
        , test_dispatch_allows_own_task
        )
      ; ( "runs when a forwarded call names the originator's task"
        , `Quick
        , test_dispatch_allows_local_superuser
        )
      ; ( "runs when no task is named by the request"
        , `Quick
        , test_dispatch_allows_without_task_header
        )
      ]
    )
  ]
