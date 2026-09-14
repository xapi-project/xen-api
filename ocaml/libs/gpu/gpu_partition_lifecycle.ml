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

type transition =
  | Create
  | Reserve
  | Confirm
  | Attach
  | Release_halted
  | Release_suspended
  | Checkpoint
  | Abort
  | Restart_sweep
[@@deriving show {with_path= false}, enum]

type 'a ref_action = Set of 'a | Clear | Untouched [@@deriving eq]

type 'a outcome = {resident: 'a ref_action; scheduled: 'a ref_action}
[@@deriving eq]

let nothing = {resident= Untouched; scheduled= Untouched}

let outcome_of transition ~chosen =
  match transition with
  | Create ->
      {resident= Clear; scheduled= Clear}
  (* Reserve and Confirm are the only transitions that need a partition, and
     neither site writes anything without one: Reserve raises before it gets
     here, and Confirm is skipped when there is no valid reservation. *)
  | Reserve -> (
    match chosen with
    | Some v ->
        {resident= Untouched; scheduled= Set v}
    | None ->
        nothing
  )
  | Confirm -> (
    match chosen with
    | Some v ->
        {resident= Set v; scheduled= Clear}
    | None ->
        nothing
  )
  | Attach ->
      {resident= Untouched; scheduled= Untouched}
  | Release_halted ->
      {resident= Clear; scheduled= Clear}
  | Release_suspended ->
      {resident= Untouched; scheduled= Clear}
  | Checkpoint ->
      {resident= Untouched; scheduled= Untouched}
  | Abort ->
      {resident= Untouched; scheduled= Clear}
  | Restart_sweep ->
      {resident= Untouched; scheduled= Clear}

let all_transitions =
  List.init (max_transition - min_transition + 1) (fun i -> i + min_transition)
  |> List.filter_map transition_of_enum

let string_of_transition = show_transition

let string_of_ref_action show_a = function
  | Set v ->
      Printf.sprintf "Set(%s)" (show_a v)
  | Clear ->
      "Clear"
  | Untouched ->
      "Untouched"
