(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

let ops =
  [ `suspend
  ; `checkpoint
  ; `pool_migrate
  ; `migrate_send
  ]

let op_string = function
  | `suspend      -> "suspend"
  | `checkpoint   -> "checkpoint"
  | `pool_migrate -> "pool_migrate"
  | `migrate_send -> "migrate_send"
  | _             -> "other"

let testcases =
  (* nest , nomig, force, permitted *)
  [ false, false, false, true
  ; false, false, true , true
  ; false, true , false, false
  ; false, true , true , true
  ; true , false, false, false
  ; true , false, true , true
  ; true , true , false, false
  ; true , true , true , true
  ]

(* NB, we choose a PV guest here for testing even though some of these options
   make no sense for PV (e.g. nested virt). The logic's all the same though and
   it means we can avoid making up a VM_guest_metrics record with the feature
   flags set *)
let run_test (nv, nm, force, permitted) op =
  let __context = Test_common.make_test_database () in
  let vm        = Test_common.make_vm ~__context ~hVM_boot_policy:"" ~domain_type:`pv () in
  let metrics   = Db.VM.get_metrics ~__context ~self:vm in
  let strict    = not force in
  ( Db.VM.set_power_state ~__context ~self:vm ~value:`Running
  ; Db.VM_metrics.set_current_domain_type ~__context ~self:metrics ~value:(Db.VM.get_domain_type ~__context ~self:vm)
  ; Db.VM_metrics.set_nested_virt ~__context ~self:metrics ~value:nv
  ; Db.VM_metrics.set_nomigrate   ~__context ~self:metrics ~value:nm
  ; Xapi_vm_lifecycle.get_operation_error ~__context ~self:vm ~op ~strict
    |> function
    | None        when permitted     -> ()
    | None                           -> Alcotest.fail (Printf.sprintf "nv=%b nm=%b force=%b permitted=%b op=%s" nv nm force permitted (op_string op))
    | Some (x,xs) when not permitted -> ()
    | Some (x,xs)                    -> Alcotest.fail (Printf.sprintf "nv=%b nm=%b force=%b permitted=%b op=%s error was=%s" nv nm force permitted (op_string op) x)
  )

let test' op =
  List.iter (fun t -> run_test t op) testcases

let test =
  [ "test_no_migrate_00", `Quick, (fun () -> test' `suspend)
  ; "test_no_migrate_01", `Quick, (fun () -> test' `checkpoint)
  ; "test_no_migrate_02", `Quick, (fun () -> test' `pool_migrate)
  ; "test_no_migrate_03", `Quick, (fun () -> test' `migrate_send)
  ]

