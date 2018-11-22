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


type platformdata = (string * string) list

let is_valid ~key ~platformdata =
  (not (List.mem_assoc key platformdata)) ||
  (match List.assoc key platformdata |> String.lowercase_ascii with
   | "true" | "1" | "false" | "0" -> true
   | _ -> false
  )

let is_true ~key ~platformdata ~default =
  try
    match List.assoc key platformdata |> String.lowercase_ascii with
    | "true"  | "1" -> true
    | "false" | "0" -> false
    | _ -> default (* Check for validity using is_valid if required *)
  with Not_found ->
    default


