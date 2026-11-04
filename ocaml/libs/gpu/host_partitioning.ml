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

type reason = No_partitionable_gpu | Pool_disabled
[@@deriving show {with_path= false}]

let token r = String.lowercase_ascii (show_reason r)

let ( let* ) = Result.bind

let check ok reason =
  if ok () then
    Ok ()
  else
    Error reason

let desired ~partitionable_gpu ~pool_enabled =
  let* () = check partitionable_gpu No_partitionable_gpu in
  check pool_enabled Pool_disabled
