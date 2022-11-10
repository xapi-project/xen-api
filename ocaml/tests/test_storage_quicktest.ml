(*
 * Copyright (C) Citrix Systems Inc.
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

module Cb = Crowbar
open Storage_impl

let dpvs_cb_t =
  let one_dpvs =
    let state_of_int j =
      match j with
      | 1 ->
          Vdi_automaton.Attached Vdi_automaton.RW
      | 2 ->
          Vdi_automaton.Attached Vdi_automaton.RO
      | _ ->
          Vdi_automaton.Detached
    in
    Cb.map
      [Cb.(range 10); Cb.(range 2)]
      (fun i j ->
        ("dp" ^ string_of_int i, vm_of_s (string_of_int i), state_of_int j)
      )
  in
  Cb.(list one_dpvs)

let test_equal_dp dpvs =
  let vdi = Vdi.empty () in
  let vdi =
    List.fold_left
      (fun vdi (dp, vm, state) -> Vdi.add_or_update_dp dp vm state vdi)
      vdi dpvs
  in
  let dps = List.map fst (Vdi.dps vdi) in
  let dpv = List.map fst (Vdi.dpv vdi) in
  Cb.check @@ (dps = dpv)

let () =
  Cb.add_test ~name:"dp equality between dpv and dps" [dpvs_cb_t] test_equal_dp
