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

cstruct t {
  uint64_t offset;
  uint32_t len
  (* data *)
} as little_endian

let sizeof = sizeof_t

type t = {
  offset: int64;   (** offset on the physical disk *)
  data: Cstruct.t; (** data to write *)
}

let marshal (buf: Cstruct.t) t =
  set_t_offset buf t.offset;
  set_t_len buf (Int32.of_int (Cstruct.len t.data))

let is_last_chunk (buf: Cstruct.t) =
  get_t_offset buf = 0L && (get_t_len buf = 0l)

let get_offset = get_t_offset
let get_len = get_t_len
