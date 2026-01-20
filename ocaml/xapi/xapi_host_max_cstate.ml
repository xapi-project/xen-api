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

exception Invalid_cstate of (int option * int option)

exception Invalid_cstate_string of string

exception Parse_cmd_output_failure of string

exception Command_failure of string

let ( let* ) = Option.bind

let valid cstate = Option.fold ~none:true ~some:(fun v -> v >= 0) cstate

let raise_parse_fail s e =
  let msg = Printf.sprintf "output: %s; error: %s" s (Printexc.to_string e) in
  raise (Parse_cmd_output_failure msg)

let to_string (cstate, sub_cstate) =
  match (cstate, sub_cstate) with
  | None, _ ->
      ""
  | Some v, None when v >= 0 ->
      Int.to_string v
  | Some v, Some sub_v when v >= 0 && sub_v >= 0 ->
      Printf.sprintf "%d,%d" v sub_v
  | _ ->
      raise (Invalid_cstate (cstate, sub_cstate))

let of_string s =
  let cstate, sub_cstate =
    match String.split_on_char ',' s with
    | [""] ->
        (None, None)
    | [state] -> (
      try (Some (int_of_string state), None)
      with _ -> raise (Invalid_cstate_string s)
    )
    | [state; sub_state] -> (
      try (Some (int_of_string state), Some (int_of_string sub_state))
      with _ -> raise (Invalid_cstate_string s)
    )
    | _ ->
        raise (Invalid_cstate_string s)
  in
  if valid cstate && valid sub_cstate then
    (cstate, sub_cstate)
  else
    raise (Invalid_cstate_string s)

let parse_xenpm_response resp =
  let parse_max_cstate_line line =
    let line = String.trim line in
    let* cstate_word =
      try Scanf.sscanf line "max C-state set to %s" Option.some
      with e -> raise_parse_fail resp e
    in
    match cstate_word with
    | "unlimited" ->
        None
    | s -> (
      try Scanf.sscanf s "C%d" Option.some with e -> raise_parse_fail resp e
    )
  in
  let parse_sub_cstate_line line =
    let line = String.trim line in
    let* sub_cstate_word =
      try Scanf.sscanf line "max C-substate set to %s succeeded" Option.some
      with e -> raise_parse_fail resp e
    in
    match sub_cstate_word with
    | "unlimited" ->
        None
    | s -> (
      try Some (int_of_string s) with e -> raise_parse_fail resp e
    )
  in
  match String.split_on_char '\n' resp with
  | [] | [""] ->
      raise (Parse_cmd_output_failure resp)
  | cstate_line :: [] | [cstate_line; ""] ->
      (parse_max_cstate_line cstate_line, None)
  | cstate_line :: sub_cstate_line :: _ ->
      (parse_max_cstate_line cstate_line, parse_sub_cstate_line sub_cstate_line)

let parse_xencmdline_response resp =
  (* the response may be
     "" -> unlimited
     "max_cstate=N" -> max cstate N
     "max_cstate=N,M" -> max cstate N, max c-sub-state M *)
  let resp = String.trim resp in
  match Astring.String.fields ~is_sep:(fun c -> c = '=' || c = ',') resp with
  | [""] ->
      (None, None)
  | ["max_cstate"; state] -> (
    try (Some (int_of_string state), None) with e -> raise_parse_fail resp e
  )
  | ["max_cstate"; state; sub_state] -> (
    try (Some (int_of_string state), Some (int_of_string sub_state))
    with e -> raise_parse_fail resp e
  )
  | _ ->
      raise (Parse_cmd_output_failure resp)

(* xenpm examples:
   # xenpm set-max-cstate 0 0
   max C-state set to C0
   max C-substate set to 0 succeeded
   # xenpm set-max-cstate 0
   max C-state set to C0
   max C-substate set to unlimited succeeded
   # xenpm set-max-cstate unlimited
   max C-state set to unlimited
   # xenpm set-max-cstate -1
   Missing, excess, or invalid argument(s)
*)
let xenpm_set cstate sub_cstate =
  let args =
    match (cstate, sub_cstate) with
    | None, _ ->
        ["set-max-cstate"; "unlimited"]
    | Some v, None when v >= 0 ->
        ["set-max-cstate"; string_of_int v]
    | Some v, Some sub_v when v >= 0 && sub_v >= 0 ->
        ["set-max-cstate"; string_of_int v; string_of_int sub_v]
    | _ ->
        raise (Invalid_cstate (cstate, sub_cstate))
  in
  let resp =
    try Helpers.call_script !Xapi_globs.xenpm_bin args
    with e ->
      raise
        (Command_failure
           (Printf.sprintf "xenpm_set failed: %s" (Printexc.to_string e))
        )
  in
  match parse_xenpm_response resp with
  | None, _ ->
      if cstate <> None then
        raise
          (Command_failure
             (Printf.sprintf "get unmatched value in xenpm output %s" resp)
          )
  | cstate_in_resp, sub_cstate_in_resp ->
      if (cstate_in_resp, sub_cstate_in_resp) <> (cstate, sub_cstate) then
        raise
          (Command_failure
             (Printf.sprintf "get unmatched value in xenpm output %s" resp)
          )

let xen_cmdline_set cstate sub_cstate =
  let args =
    match (cstate, sub_cstate) with
    | None, _ ->
        ["--delete-xen"; "max_cstate"]
    | Some v, None when v >= 0 ->
        ["--set-xen"; Printf.sprintf "max_cstate=%d" v]
    | Some v, Some sub_v when v >= 0 && sub_v >= 0 ->
        ["--set-xen"; Printf.sprintf "max_cstate=%d,%d" v sub_v]
    | _ ->
        raise (Invalid_cstate (cstate, sub_cstate))
  in
  try Helpers.call_script !Xapi_globs.xen_cmdline_script args |> ignore
  with e ->
    raise
      (Command_failure
         (Printf.sprintf "xen_cmdline_set failed: %s" (Printexc.to_string e))
      )

let xen_cmdline_get () =
  let args = ["--get-xen"; "max_cstate"] in
  let resp =
    try Helpers.call_script !Xapi_globs.xen_cmdline_script args
    with e ->
      raise
        (Command_failure
           (Printf.sprintf "xen_cmdline_get failed: %s" (Printexc.to_string e))
        )
  in
  parse_xencmdline_response resp
