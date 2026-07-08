(*
 * Copyright (C) 2026 Cloud Software Group
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

module T = Test_common

let vm_ref : [`VM] Ref.t Alcotest.testable = Alcotest.testable Ref.pp ( = )

(* A freshly created PBD should default to a null storage_driver_domain
   (which callers interpret as dom0). *)
let test_default_is_null () =
  let __context = T.make_test_database () in
  let pbd = T.make_pbd ~__context () in
  Alcotest.check vm_ref "default storage_driver_domain is null" Ref.null
    (Db.PBD.get_storage_driver_domain ~__context ~self:pbd)

(* Setting the field via Xapi_pbd.set_storage_driver_domain (the implementation
   behind the PBD.set_storage_driver_domain API) must persist a valid VM ref and
   be returned by the getter, and must allow clearing back to null. *)
let test_set_get_roundtrip () =
  let __context = T.make_test_database () in
  let pbd = T.make_pbd ~__context () in
  let vm = T.make_vm ~__context () in
  Xapi_pbd.set_storage_driver_domain ~__context ~self:pbd ~value:vm ;
  Alcotest.check vm_ref "storage_driver_domain roundtrips" vm
    (Db.PBD.get_storage_driver_domain ~__context ~self:pbd) ;
  (* clearing back to null works too *)
  Xapi_pbd.set_storage_driver_domain ~__context ~self:pbd ~value:Ref.null ;
  Alcotest.check vm_ref "storage_driver_domain cleared to null" Ref.null
    (Db.PBD.get_storage_driver_domain ~__context ~self:pbd)

(* A non-null value that does not reference an existing object is rejected with
   INVALID_VALUE rather than silently stored (the hardening). *)
let test_set_rejects_invalid_ref () =
  let __context = T.make_test_database () in
  let pbd = T.make_pbd ~__context () in
  let bogus = Ref.of_string "OpaqueRef:does-not-exist" in
  Alcotest.check_raises "expected INVALID_VALUE for a non-existent VM ref"
    (Api_errors.Server_error
       (Api_errors.invalid_value, ["value"; Ref.string_of bogus])
    )
    (fun () ->
      Xapi_pbd.set_storage_driver_domain ~__context ~self:pbd ~value:bogus
    ) ;
  (* the bogus value must not have been persisted *)
  Alcotest.check vm_ref "field unchanged after rejected set" Ref.null
    (Db.PBD.get_storage_driver_domain ~__context ~self:pbd)

let test =
  [
    ("default_is_null", `Quick, test_default_is_null)
  ; ("set_get_roundtrip", `Quick, test_set_get_roundtrip)
  ; ("set_rejects_invalid_ref", `Quick, test_set_rejects_invalid_ref)
  ]
