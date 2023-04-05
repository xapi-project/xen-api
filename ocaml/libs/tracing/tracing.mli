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

module Span : sig
  type t

  val of_string : string -> t option

  val to_string : t -> string
end

module Tracer : sig
  type t

  val start :
       tracer:t
    -> name:string
    -> parent:Span.t option
    -> (Span.t option, exn) result

  val finish : Span.t option -> (Span.t option, exn) result
end

module TracerProvider : sig
  type t
end

val get_tracer : name:string -> Tracer.t
