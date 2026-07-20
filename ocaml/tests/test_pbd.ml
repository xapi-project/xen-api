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

(* --- lifecycle / placement constraints for storage driver domains --- *)

let get_op_error ~__context ~vm ~op =
  Xapi_vm_lifecycle.get_operation_error ~__context ~self:vm ~op ~strict:true

(* the exact errors our guards produce *)
let serving_block =
  ( Api_errors.operation_not_allowed
  , ["VM is a storage driver domain with plugged PBDs"]
  )

let migrate_block =
  (Api_errors.operation_not_allowed, ["VM is a storage driver domain"])

let err_testable = Alcotest.(option (pair string (list string)))

let not_sdd_blocked e = e <> Some serving_block && e <> Some migrate_block

(* Xapi_vm.create always makes a Halted VM, so force the power state explicitly. *)
let running_vm ~__context =
  let vm = T.make_vm ~__context () in
  Db.VM.set_power_state ~__context ~self:vm ~value:`Running ;
  vm

(* ops that must be refused while a storage driver domain is serving storage.
   NB: a bare test VM has no baseline for e.g. clean_shutdown (it lacks the
   guest-agent feature), so tests assert against our exact errors rather than
   None. *)
let disruptive_ops =
  [
    `clean_shutdown
  ; `hard_shutdown
  ; `clean_reboot
  ; `hard_reboot
  ; `suspend
  ; `checkpoint
  ; `pause
  ]

(* A running storage driver domain with a plugged PBD must refuse disruptive
   lifecycle ops. *)
let test_serving_blocks_disruptive_ops () =
  let __context = T.make_test_database () in
  let vm = running_vm ~__context in
  let (_ : API.ref_PBD) =
    T.make_pbd ~__context ~storage_driver_domain:vm ~currently_attached:true ()
  in
  List.iter
    (fun op ->
      Alcotest.check err_testable "disruptive op refused while serving"
        (Some serving_block)
        (get_op_error ~__context ~vm ~op)
    )
    disruptive_ops

(* With no PBD plugged the VM is not serving storage, so the same ops are not
   blocked by our rule. *)
let test_unplugged_allows_disruptive_ops () =
  let __context = T.make_test_database () in
  let vm = running_vm ~__context in
  let (_ : API.ref_PBD) =
    T.make_pbd ~__context ~storage_driver_domain:vm ~currently_attached:false ()
  in
  List.iter
    (fun op ->
      Alcotest.(check bool)
        "op not blocked by our rule when no PBD is plugged" true
        (not_sdd_blocked (get_op_error ~__context ~vm ~op))
    )
    disruptive_ops

(* A storage driver domain must never migrate, even when shut down and with no
   PBD plugged. *)
let test_migrate_blocked_even_when_halted () =
  let __context = T.make_test_database () in
  let vm = T.make_vm ~__context ~power_state:`Halted () in
  let (_ : API.ref_PBD) =
    T.make_pbd ~__context ~storage_driver_domain:vm ~currently_attached:false ()
  in
  Alcotest.check err_testable "migrate_send refused for a storage driver domain"
    (Some migrate_block)
    (get_op_error ~__context ~vm ~op:`migrate_send)

(* A VM that is not a storage driver domain is unaffected by our rules. *)
let test_non_sdd_unaffected () =
  let __context = T.make_test_database () in
  let vm = running_vm ~__context in
  List.iter
    (fun op ->
      Alcotest.(check bool)
        "op not blocked by our rule for a normal VM" true
        (not_sdd_blocked (get_op_error ~__context ~vm ~op))
    )
    (`migrate_send :: `pool_migrate :: disruptive_ops)

(* A storage driver domain may only boot on the host owning its PBD(s). *)
let test_start_host_pin () =
  let __context = T.make_test_database () in
  let vm = T.make_vm ~__context () in
  let host = T.make_host ~__context () in
  let other = T.make_host ~__context () in
  let (_ : API.ref_PBD) =
    T.make_pbd ~__context ~storage_driver_domain:vm ~host ()
  in
  Xapi_vm_helpers.assert_matches_storage_driver_domain_host ~__context ~self:vm
    ~host ;
  Alcotest.check_raises "boot on wrong host refused"
    (Api_errors.Server_error
       ( Api_errors.operation_not_allowed
       , [
           "Cannot boot a storage driver domain on a host that does not own \
            its PBD(s)"
         ]
       )
    )
    (fun () ->
      Xapi_vm_helpers.assert_matches_storage_driver_domain_host ~__context
        ~self:vm ~host:other
    )

(* A VM must not be destroyed while a PBD still names it as its storage driver
   domain; the destroy is refused so the PBDs are not left orphaned. *)
let test_destroy_blocked_while_referenced () =
  let __context = T.make_test_database () in
  let vm = T.make_vm ~__context () in
  let (_ : API.ref_PBD) = T.make_pbd ~__context ~storage_driver_domain:vm () in
  Alcotest.check_raises "destroy refused while a PBD references the VM"
    (Api_errors.Server_error
       ( Api_errors.operation_not_allowed
       , ["VM is a storage driver domain with PBDs referencing it"]
       )
    )
    (fun () -> Xapi_vm_helpers.destroy ~__context ~self:vm)

let test =
  [
    ("default_is_null", `Quick, test_default_is_null)
  ; ("set_get_roundtrip", `Quick, test_set_get_roundtrip)
  ; ("set_rejects_invalid_ref", `Quick, test_set_rejects_invalid_ref)
  ; ("serving_blocks_disruptive_ops", `Quick, test_serving_blocks_disruptive_ops)
  ; ( "unplugged_allows_disruptive_ops"
    , `Quick
    , test_unplugged_allows_disruptive_ops
    )
  ; ( "migrate_blocked_even_when_halted"
    , `Quick
    , test_migrate_blocked_even_when_halted
    )
  ; ("non_sdd_unaffected", `Quick, test_non_sdd_unaffected)
  ; ("start_host_pin", `Quick, test_start_host_pin)
  ; ( "destroy_blocked_while_referenced"
    , `Quick
    , test_destroy_blocked_while_referenced
    )
  ]
