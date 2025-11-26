(*
 * Copyright (C) 2025 Vates.
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

val update_task_progress : Context.t -> int -> unit

val receive : (int -> unit) -> Unix.file_descr -> string -> unit

val send :
     ?relative_to:string
  -> (int -> unit)
  -> Unix.file_descr
  -> string
  -> int64
  -> unit

val parse_header : string -> int * int list
