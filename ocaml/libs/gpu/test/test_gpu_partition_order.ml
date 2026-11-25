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

open Gpu.Gpu_partition_order

(* The state that a plan starts from, or why there is none *)
let start = function
  | Ok (Vfs_disabled (Undivided _)) ->
      "Vfs_disabled Undivided"
  | Ok (Vfs_disabled (Divided _)) ->
      "Vfs_disabled Divided"
  | Ok (Vfs_enabled (Undivided _)) ->
      "Vfs_enabled Undivided"
  | Ok (Vfs_enabled (Divided _)) ->
      "Vfs_enabled Divided"
  | Error refusal ->
      string_of_refusal refusal

let observes name ~vfs ~mode found expected =
  Alcotest.test_case name `Quick @@ fun () ->
  observe ~vfs ~mode found |> start |> Alcotest.(check string) "start" expected

let observing =
  [
    observes "a GPU without SR-IOV is refused" ~vfs:None ~mode:(Some Disabled)
      Absent "No_sriov"
  ; observes "a negative VF count is a contradiction" ~vfs:(Some (-1))
      ~mode:(Some Disabled) Absent "Contradiction"
  ; observes "a GPU whose mode was not read is refused" ~vfs:(Some 0) ~mode:None
      Absent "Mode_unreadable"
  ; observes "a GPU that cannot be divided is refused" ~vfs:(Some 0)
      ~mode:(Some Not_supported) Absent "Not_divisible"
  ; observes "a GPU whose division starts at its next reset is refused"
      ~vfs:(Some 0) ~mode:(Some Enable_on_reboot) Absent "Mode_pending"
  ; observes "a GPU whose division ends at its next reset is refused"
      ~vfs:(Some 16) ~mode:(Some Disable_on_reboot) Present "Mode_pending"
  ; observes "partitions on an undivided GPU are a contradiction" ~vfs:(Some 16)
      ~mode:(Some Disabled) Present "Contradiction"
  ; observes "an undivided GPU without VFs starts there" ~vfs:(Some 0)
      ~mode:(Some Disabled) Absent "Vfs_disabled Undivided"
  ; observes "an undivided GPU with VFs starts there, its partitions unread"
      ~vfs:(Some 16) ~mode:(Some Disabled) Unreadable "Vfs_enabled Undivided"
  ; observes "partitions on a GPU without VFs are refused" ~vfs:(Some 0)
      ~mode:(Some Enabled) Present "Partitions_without_vfs"
  ; observes "a divided GPU with VFs is refused if its partitions were not read"
      ~vfs:(Some 16) ~mode:(Some Enabled) Unreadable "Partitions_unreadable"
  ; observes "a divided GPU without VFs or partitions starts there"
      ~vfs:(Some 0) ~mode:(Some Enabled) Absent "Vfs_disabled Divided"
  ; observes "a divided GPU with VFs and partitions starts there" ~vfs:(Some 16)
      ~mode:(Some Enabled) Present "Vfs_enabled Divided"
  ]

(* The steps of a plan, in the order a runner runs them *)
let runs (planned : (string, string, _) plan) =
  List.map
    (function
      | Vf_enable ->
          "vf_enable"
      | Divide c ->
          "divide " ^ c
      | Destroy d ->
          "destroy " ^ d
      | Undivide d ->
          "undivide " ^ d
      )
    (steps planned :> (string, string) step list)

(* A division into [c], planned from each start *)
let division c = function
  | Ok (Vfs_disabled (Undivided empty)) ->
      runs (empty |> vf_enable |> divide c)
  | Ok (Vfs_disabled (Divided empty)) ->
      runs (empty |> vf_enable |> divide c)
  | Ok (Vfs_enabled (Undivided empty)) ->
      runs (divide c empty)
  | Ok (Vfs_enabled (Divided empty)) ->
      runs (divide c empty)
  | Error refusal ->
      [string_of_refusal refusal]

(* [teardown] planned from a divided GPU with VFs, or the start found *)
let teardown_with_vfs teardown = function
  | Ok (Vfs_enabled (Divided empty)) ->
      runs (teardown empty)
  | ( Ok (Vfs_disabled (Undivided _ | Divided _) | Vfs_enabled (Undivided _))
    | Error _ ) as other ->
      [start other]

let lists name planner ~vfs ~mode found expected =
  Alcotest.test_case name `Quick @@ fun () ->
  observe ~vfs ~mode found
  |> planner
  |> Alcotest.(check (list string)) "steps" expected

let transcripts =
  [
    lists "a division after a reboot enables the VFs, then divides"
      (division "4g") ~vfs:(Some 0) ~mode:(Some Disabled) Absent
      ["vf_enable"; "divide 4g"]
  ; (* A reset keeps the GPU divided, so no step divides it before the VFs
       are enabled *)
    lists "a division after a reset enables the VFs, then divides"
      (division "4g") ~vfs:(Some 0) ~mode:(Some Enabled) Absent
      ["vf_enable"; "divide 4g"]
  ; lists "a division with the VFs enabled only divides" (division "4g")
      ~vfs:(Some 16) ~mode:(Some Disabled) Absent ["divide 4g"]
  ; lists "a destruction keeps the GPU divided"
      (teardown_with_vfs (destroy "1"))
      ~vfs:(Some 16) ~mode:(Some Enabled) Present ["destroy 1"]
  ; lists "a release destroys, then undivides, in one step"
      (teardown_with_vfs (undivide "1"))
      ~vfs:(Some 16) ~mode:(Some Enabled) Present ["undivide 1"]
  ]

let () =
  Alcotest.run "gpu_partition_order"
    [("observe", observing); ("steps", transcripts)]
