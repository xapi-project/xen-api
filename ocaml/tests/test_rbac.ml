(*
 * Copyright (c) Cloud Software Group, Inc.
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

let first_disallowed ~__context ~session_id permissions =
  Rbac.find_first_disallowed_permission ~__context ~session_id ~permissions

let check_decision msg expected actual =
  Alcotest.(check (option string)) msg expected actual

let test_superuser () =
  let __context = Test_common.make_test_database () in
  let session_id =
    Test_common.make_session ~__context ~is_local_superuser:true
      ~rbac_permissions:[] ()
  in
  check_decision "local superuser is allowed everything" None
    (first_disallowed ~__context ~session_id ["vm.start"; "pool.destroy"])

let test_permissions () =
  let __context = Test_common.make_test_database () in
  let session_id =
    Test_common.make_session ~__context
      ~rbac_permissions:["vm.start"; "vm.clean_shutdown"]
      ()
  in
  for i = 1 to 2 do
    let msg = Printf.sprintf "%s (check %d)" in
    check_decision
      (msg "granted permission" i)
      None
      (first_disallowed ~__context ~session_id ["vm.start"]) ;
    check_decision
      (msg "first missing permission is reported" i)
      (Some "vm.destroy")
      (first_disallowed ~__context ~session_id
         ["vm.start"; "vm.destroy"; "vm.clean_shutdown"; "pool.destroy"]
      ) ;
    Alcotest.(check bool)
      (msg "is_access_allowed denies" i)
      false
      (Rbac.is_access_allowed ~__context ~session_id ~permission:"pool.destroy")
  done

let test_no_permissions () =
  let __context = Test_common.make_test_database () in
  let session_id =
    Test_common.make_session ~__context ~rbac_permissions:[] ()
  in
  check_decision "session without permissions is denied" (Some "vm.start")
    (first_disallowed ~__context ~session_id ["vm.start"])

let test_destroyed_session () =
  let __context = Test_common.make_test_database () in
  let ordinary =
    Test_common.make_session ~__context ~rbac_permissions:["vm.start"] ()
  in
  let superuser =
    Test_common.make_session ~__context ~is_local_superuser:true ()
  in
  check_decision "allowed before destroy" None
    (first_disallowed ~__context ~session_id:ordinary ["vm.start"]) ;
  List.iter
    (fun (name, session_id) ->
      Db.Session.destroy ~__context ~self:session_id ;
      match first_disallowed ~__context ~session_id ["vm.start"] with
      | exception _ ->
          ()
      | result ->
          Alcotest.failf "destroyed %s session: expected an exception, got %s"
            name
            (Option.value ~default:"None" result)
    )
    [("ordinary", ordinary); ("superuser", superuser)]

let tests =
  [
    ( "Test_rbac"
    , [
        ("superuser", `Quick, test_superuser)
      ; ("permissions", `Quick, test_permissions)
      ; ("no_permissions", `Quick, test_no_permissions)
      ; ("destroyed_session", `Quick, test_destroyed_session)
      ]
    )
  ]
