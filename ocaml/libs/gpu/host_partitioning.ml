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

type reason = No_partitionable_gpu | Forced_off | Pool_disabled
[@@deriving show {with_path= false}, enum]

let token r = String.lowercase_ascii (show_reason r)

let all_reasons =
  List.filter_map reason_of_enum
    (List.init (max_reason - min_reason + 1) (fun i -> min_reason + i))

type forced = Not_forced | Forced [@@deriving enum]

(* Written to disk and kept across upgrades: never change a token *)
let forced_token = function
  | Not_forced ->
      "not_forced"
  | Forced ->
      "forced_off"

let all_forced =
  List.filter_map forced_of_enum
    (List.init (max_forced - min_forced + 1) (fun i -> min_forced + i))

(* Fail safe: only the exact "not forced" value, or no file, leaves the
   pool's setting in charge. Every value of [forced] is parsed from its token,
   so a value added later cannot be read as another one *)
let forced_of_file ~unexpected read =
  let forced_off why = unexpected why ; Forced in
  let has_token contents forced = String.equal (forced_token forced) contents in
  match read with
  | Ok None ->
      Not_forced
  | Ok (Some contents) -> (
    match List.find_opt (has_token contents) all_forced with
    | Some forced ->
        forced
    | None ->
        forced_off (Printf.sprintf "it holds the unexpected value %S" contents)
  )
  | Error why ->
      forced_off why

let ( let* ) = Result.bind

let desired ~partitionable_gpu ~forced ~pool_enabled =
  let open Xapi_stdext_pervasives.Pervasiveext in
  let* () = ok_if_true (partitionable_gpu ()) ~error:No_partitionable_gpu in
  let* () =
    match forced () with Not_forced -> Ok () | Forced -> Error Forced_off
  in
  ok_if_true (pool_enabled ()) ~error:Pool_disabled
