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

module W3CBaggage : sig
  module Value : sig
    type t

    val make : string -> t

    val to_string : t -> string
  end
end

type endpoint = Bugtool | Url of Uri.t

val bugtool_name : string

val endpoint_to_string : endpoint -> string

module SpanKind : sig
  type t = Server | Consumer | Client | Producer | Internal

  val to_string : t -> string
end

module Status : sig
  type status_code

  type t
end

module SpanContext : sig
  type t

  val to_traceparent : t -> string

  val of_traceparent : string -> t option

  val trace_id_of_span_context : t -> string
end

module Span : sig
  type t

  val compare : t -> t -> int

  val get_context : t -> SpanContext.t

  val add_link : t -> SpanContext.t -> (string * string) list -> t

  val add_event : t -> string -> (string * string) list -> t

  val set_span_kind : t -> SpanKind.t -> t

  val get_tag : t -> string -> string
end

module Spans : sig
  val set_max_spans : int -> unit

  val set_max_traces : int -> unit

  val dump :
    unit -> (string, Span.t list) Hashtbl.t * (string, Span.t list) Hashtbl.t
end

module Tracer : sig
  type t

  val span_of_span_context : SpanContext.t -> string -> Span.t

  val start :
       tracer:t
    -> ?attributes:(string * string) list
    -> ?span_kind:SpanKind.t
    -> name:string
    -> parent:Span.t option
    -> unit
    -> (Span.t option, exn) result

  val finish :
    ?error:exn * string -> Span.t option -> (Span.t option, exn) result

  val span_is_finished : Span.t option -> bool

  val span_hashtbl_is_empty : unit -> bool

  val finished_span_hashtbl_is_empty : unit -> bool
end

module TracerProvider : sig
  type t

  val get_name_label : t -> string

  val get_attributes : t -> (string * string) list

  val get_endpoints : t -> endpoint list

  val get_enabled : t -> bool
end

val set :
     ?enabled:bool
  -> ?attributes:(string * string) list
  -> ?endpoints:string list
  -> uuid:string
  -> unit
  -> unit

val create :
     enabled:bool
  -> attributes:(string * string) list
  -> endpoints:string list
  -> name_label:string
  -> uuid:string
  -> unit

val destroy : uuid:string -> unit

val get_tracer_providers : unit -> TracerProvider.t list

val get_tracer : name:string -> Tracer.t

val with_tracing :
     ?attributes:(string * string) list
  -> ?parent:Span.t option
  -> name:string
  -> (Span.t option -> 'a)
  -> 'a

module Export : sig
  val set_export_interval : float -> unit

  val set_host_id : string -> unit

  val set_service_name : string -> unit

  module Destination : sig
    module File : sig
      val set_max_file_size : int -> unit

      val set_trace_log_dir : string -> unit

      val set_compress_tracing_files : bool -> unit
    end

    val flush_spans : unit -> unit

    module Http : sig
      val export : url:Uri.t -> string -> (unit, exn) result
    end
  end
end

val set_observe : bool -> unit

val validate_attribute : string * string -> bool

val flush_and_exit : unit -> unit

val main : unit -> Thread.t
