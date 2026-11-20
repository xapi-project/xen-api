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

type vfs_disabled = [`Vfs_disabled]

type vfs_enabled = [`Vfs_enabled]

type undivided = [`Undivided]

type divided = [`Divided]

type ('c, 'd) step = Vf_enable | Divide of 'c | Destroy of 'd | Undivide of 'd

(* Newest first, so that each builder adds one step *)
type ('c, 'd, 'state) plan = ('c, 'd) step list

type ('c, 'd) ordered = ('c, 'd) step list

type ('c, 'd, 'v) by_division =
  | Undivided of ('c, 'd, 'v * undivided) plan
  | Divided of ('c, 'd, 'v * divided) plan

type ('c, 'd) observed =
  | Vfs_disabled of ('c, 'd, vfs_disabled) by_division
  | Vfs_enabled of ('c, 'd, vfs_enabled) by_division

type partitions = Present | Absent | Unreadable

type refusal =
  | No_sriov
  | Mode_unreadable
  | Not_divisible
  | Mode_pending
  | Partitions_without_vfs
  | Partitions_unreadable
  | Contradiction
[@@deriving show {with_path= false}]

let string_of_refusal = show_refusal

(* Partitions exist only on a divided GPU, so an undivided GPU has none, read
   or not; a divided GPU must have none before its VFs are enabled *)
let observe ~vfs ~(mode : Partition_mode.mode option) found =
  match vfs with
  | None ->
      Error No_sriov
  | Some count when count < 0 ->
      Error Contradiction
  | Some count -> (
    match (mode, found) with
    | None, (Present | Absent | Unreadable) ->
        Error Mode_unreadable
    | Some Not_supported, (Present | Absent | Unreadable) ->
        Error Not_divisible
    | ( Some (Enable_on_reboot | Disable_on_reboot)
      , (Present | Absent | Unreadable) ) ->
        Error Mode_pending
    | Some Disabled, Present ->
        Error Contradiction
    | Some Disabled, (Absent | Unreadable) when count = 0 ->
        Ok (Vfs_disabled (Undivided []))
    | Some Disabled, (Absent | Unreadable) ->
        Ok (Vfs_enabled (Undivided []))
    | Some Enabled, Present when count = 0 ->
        Error Partitions_without_vfs
    | Some Enabled, Unreadable ->
        Error Partitions_unreadable
    | Some Enabled, Absent when count = 0 ->
        Ok (Vfs_disabled (Divided []))
    | Some Enabled, (Present | Absent) ->
        Ok (Vfs_enabled (Divided []))
  )

let vf_enable before = Vf_enable :: before

let divide c before = Divide c :: before

let destroy d before = Destroy d :: before

let undivide d before = Undivide d :: before

let steps = List.rev
