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

(* Basic types *)

module SpanContext : sig
  type t

  val to_traceparent : t -> string

  val of_traceparent : string -> t option
end

module Span : sig
  type t

  val get_span_context : t -> SpanContext.t
end

module Tracer : sig
  type t

  val span_of_span_context : SpanContext.t -> string -> Span.t

  val start :
       tracer:t
    -> name:string
    -> parent:Span.t option
    -> (Span.t option, exn) result

  val finish : Span.t option -> (unit, exn) result

  val assert_finished : Span.t option -> bool
end

module TracerProvider : sig
  type t

  val find_tracer : provider:t -> name:string -> Tracer.t
end

module TracerProviders : sig
  val find_or_create_tracer :
    provider:TracerProvider.t -> name:string -> Tracer.t

  val set_default :
       tags:(string * string) list
    -> endpoints:string list
    -> processors:string list
    -> filters:string list
    -> unit

  val get_default : unit -> (TracerProvider.t, exn) result

  val get_default_tracer : name:string -> Tracer.t
end

type blob = Span.t

type t = blob option

val empty : t

val is_empty : t -> bool

val t_of_string : string -> t

val t_to_string_opt : t -> string option

val enable_span_garbage_collector : ?timeout:float -> unit -> unit
