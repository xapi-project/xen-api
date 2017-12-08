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

open OUnit
open Test_common

module LC = Xapi_vm_lifecycle

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
  (*nest , nomig, force, permitted *)
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
let test (nv, nm, force, permitted) op =
  let __context = make_test_database () in
  let vm        = make_vm ~__context ~hVM_boot_policy:"" ~domain_type:`pv () in
  let metrics   = Db.VM.get_metrics ~__context ~self:vm in
  let strict    = not force in
  ( Db.VM.set_power_state ~__context ~self:vm ~value:`Running
  ; Db.VM_metrics.set_current_domain_type ~__context ~self:metrics ~value:(Db.VM.get_domain_type ~__context ~self:vm)
  ; Db.VM_metrics.set_nested_virt ~__context ~self:metrics ~value:nv
  ; Db.VM_metrics.set_nomigrate   ~__context ~self:metrics ~value:nm
  ; LC.get_operation_error ~__context ~self:vm ~op ~strict
    |> function
    | None        when permitted     -> assert_bool "success" true
    | None                           -> assert_failure (Printf.sprintf "nv=%b nm=%b force=%b permitted=%b op=%s" nv nm force permitted (op_string op))
    | Some (x,xs) when not permitted -> assert_bool "success" true
    | Some (x,xs)                    -> assert_failure (Printf.sprintf "nv=%b nm=%b force=%b permitted=%b op=%s error was=%s" nv nm force permitted (op_string op) x)
  )

let test' op =
  testcases |> List.iter (fun t -> test t op)

let test = "test_no_migrate" >:::
           [ "test_no_migrate_00" >:: (fun () -> test' `suspend)
           ; "test_no_migrate_01" >:: (fun () -> test' `checkpoint)
           ; "test_no_migrate_02" >:: (fun () -> test' `pool_migrate)
           ; "test_no_migrate_03" >:: (fun () -> test' `migrate_send)
           ]

