(*
 * Copyright (C) Cloud Software Group
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

type t = {log: string; tracing: Tracing.Span.t option}

val make : log:string -> tracing:Tracing.Span.t option -> t

val of_string : string -> t

val to_string : t -> string

val to_log_string : t -> string

val with_dbg :
     ?attributes:(string * string) list
  -> ?with_thread:bool
  -> ?module_name:string
  -> name:string
  -> dbg:string
  -> (t -> 'a)
  -> 'a

val traceparent_of_dbg : string -> string option
