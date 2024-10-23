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

let uuid_of_handle h =
  let h' = String.init 16 (fun i -> char_of_int h.(i)) in
  match Uuidm.of_binary_string h' with
  | Some x ->
      x
  | None ->
      failwith (Printf.sprintf "VM handle '%s' is an invalid uuid" h')

let handle_of_uuid u =
  let s = Uuidm.to_binary_string u in
  Array.init 16 (fun i -> int_of_char s.[i])
