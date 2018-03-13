(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
type t
val make : unit -> t [@@ocaml.deprecated]
val length : t -> int64 [@@ocaml.deprecated]
val get : t -> int64 -> char [@@ocaml.deprecated]
val append_substring : t -> string -> int -> int -> unit [@@ocaml.deprecated]

(** [append_string b s] appends the string [x] to the big buffer [b] *)
val append_string : t -> string -> unit [@@ocaml.deprecated]

val to_fct : t -> (string -> unit) -> unit [@@ocaml.deprecated]
val to_string : t -> string [@@ocaml.deprecated]
val to_stream : t -> out_channel -> unit [@@ocaml.deprecated]
