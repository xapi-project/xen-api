(*
 * Copyright (C) 2011-2013 Citrix Inc
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

(** A disk can be streamed as a sequence of elements *)
type 'a t = [
  | `Copy of ('a * int64 * int64)
  (** [Copy (t, offset, len)] copies [len] sectors from sector [offset]
      from the file [t] *)
  | `Sectors of Cstruct.t
  (** a new sector (e.g. for metadata) *)
  | `Empty of int64
  (** empty space in sectors *)
]

val to_string: 'a t -> string

val len: 'a t -> int64
(** [len t] is the length of [t] in sectors *)
