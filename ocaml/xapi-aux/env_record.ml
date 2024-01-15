(*
 * Copyright (C) 2023 Cloud Software Group
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

type t = string

let str txt = txt

let option opt = Option.value ~default:"" opt

let list element lst = List.map element lst |> String.concat ","

let pair (key, value) = String.concat "=" [key; value]

let to_shell_string lst =
  lst
  |> List.map (fun (key, value) -> String.concat "=" [key; Filename.quote value])
  |> String.concat "\n"

let to_string_array = Array.of_list
