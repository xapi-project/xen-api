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

open Xenops_interface.Vbd

(* ionice invocations apply a 'class' and a 'parameter' to a process *)
let to_class_param =
  let to_param = function
    | Highest -> 0
    | High    -> 2
    | Normal  -> 4
    | Low     -> 6
    | Lowest  -> 7
    | Other x -> x in
  function
  | RealTime x   -> 1, (to_param x)
  | Idle         -> 3, (to_param Lowest)
  | BestEffort x -> 2, (to_param x)

let of_class_param_exn cls param =
  let param = match param with
    | "7" -> Lowest
    | "6" -> Low
    | "4" -> Normal
    | "2" -> High
    | "0" -> Highest
    | x -> Other (int_of_string x) in
  match cls with
  | "idle"
  | "3" -> Idle
  | "realtime"
  | "1" -> RealTime param
  | "best-effort"
  | "2" -> BestEffort param
  | _ -> raise Not_found (* caught below *)

exception Parse_failed of string

let parse_result_exn s : qos_scheduler option =
  try
    match Stdext.Xstringext.String.(split_f isspace s) with
    | [ cls_colon; "prio"; param ] ->
      let cls = String.sub cls_colon 0 (String.length cls_colon - 1) in
      Some (of_class_param_exn cls param)
    | _ ->
      raise (Parse_failed s)
  with _ ->
    raise (Parse_failed s)

let set_args qos pid =
  let cls, param = to_class_param qos in
  [
    Printf.sprintf "-c%d" cls;
    Printf.sprintf "-n%d" param;
    Printf.sprintf "-p%d" pid
  ]

let get_args pid = [ Printf.sprintf "-p%d" pid ]

