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

module Attributes : sig
  include Map.S with type key := String.t

  val of_list : (string * 'a) list -> 'a t

  val to_assoc_list : 'a t -> (string * 'a) list

  val attr_of_originator : string option -> (string * string) list
end

module Status : sig
  type status_code

  type t
end

module SpanEvent : sig
  type t = {name: string; time: float; attributes: string Attributes.t}
end

module Span_id : sig
  type t

  val make : unit -> t

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string
end

module Trace_id : sig
  type t

  val make : unit -> t

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string
end

module TraceContext : sig
  type t

  val empty : t

  type traceparent = string

  type baggage = (string * string) list

  val with_traceparent : traceparent option -> t -> t

  val with_baggage : baggage option -> t -> t

  val traceparent_of : t -> traceparent option

  val baggage_of : t -> baggage option

  val to_json_string : t -> string

  val of_json_string : string -> (t, string) result
end

module SpanContext : sig
  type t

  val to_traceparent : t -> string

  val of_traceparent : string -> t option

  val of_trace_context : TraceContext.t -> t option

  val trace_id_of_span_context : t -> Trace_id.t

  val span_id_of_span_context : t -> Span_id.t

  val context_of_span_context : t -> TraceContext.t
end

module Span : sig
  type t

  val compare : t -> t -> int

  val get_context : t -> SpanContext.t

  val get_trace_context : t -> TraceContext.t

  val add_link : t -> SpanContext.t -> (string * string) list -> t

  val add_event : t -> string -> (string * string) list -> t

  val get_events : t -> SpanEvent.t list

  val set_span_kind : t -> SpanKind.t -> t

  val get_span_kind : t -> SpanKind.t

  val get_tag : t -> string -> string

  val get_name : t -> string

  val get_parent : t -> t option

  val get_begin_time : t -> float

  val get_end_time : t -> float option

  val get_attributes : t -> (string * string) list

  val with_trace_context : t -> TraceContext.t -> t
end

module TraceMap : module type of Map.Make (Trace_id)

module SpanMap : module type of Map.Make (Span_id)

module Spans : sig
  val set_max_spans : int -> unit

  val set_max_traces : int -> unit

  val span_count : unit -> int

  val since : unit -> Span.t list * int

  val dump : unit -> Span.t SpanMap.t TraceMap.t * (Span.t list * int)
end

module Tracer : sig
  type t

  val get_tracer : name:string -> t

  val span_of_span_context : SpanContext.t -> string -> Span.t

  val start :
       tracer:t
    -> ?attributes:(string * string) list
    -> ?trace_context:TraceContext.t
    -> ?span_kind:SpanKind.t
    -> name:string
    -> parent:Span.t option
    -> unit
    -> (Span.t option, exn) result

  val update_span_with_parent : Span.t -> Span.t option -> Span.t option
  (**[update_span_with_parent s p] returns [Some span] where [span] is an 
  updated verison of the span [s].
  If [p] is [Some parent], [span] is a child of [parent], otherwise it is the 
  original [s].
  
  If the span [s] is finished or is no longer considered an on-going span, 
  returns [None].
  *)

  val finish :
       ?error:exn * Printexc.raw_backtrace
    -> Span.t option
    -> (Span.t option, exn) result

  val span_hashtbl_is_empty : unit -> bool

  val finished_span_hashtbl_is_empty : unit -> bool
end

(** [TracerProvider] module provides ways to intereact with the tracer providers. 
    *)
module TracerProvider : sig
  (** Type that represents a tracer provider.*)
  type t

  val create :
       enabled:bool
    -> attributes:(string * string) list
    -> endpoints:string list
    -> name_label:string
    -> uuid:string
    -> unit
  (** [create ~enabled ~attributes ~endpoints ~name_label ~uuid] initializes a 
      tracer provider based on the following parameters: [enabled], [attributes],
      [endpoints], [name_label], and [uuid]. *)

  val set :
       ?enabled:bool
    -> ?attributes:(string * string) list
    -> ?endpoints:string list
    -> uuid:string
    -> unit
    -> unit
  (** [set ?enabled ?attributes ?endpoints ~uuid ()] updates the tracer provider
      identified by the given [uuid] with the new configuration paremeters: 
      [enabled], [attributes], and [endpoints]. 
        
      If any of the configuration parameters are
      missing, the old ones are kept.
        
      Raises [Failure] if there are no tracer provider with the given [uuid].
      *)

  val destroy : uuid:string -> unit
  (** [destroy ~uuid] destroys the tracer provider with the given [uuid]. 
      If there are no tracer provider with the given [uuid], it does nothing.
      *)

  val get_tracer_providers : unit -> t list
  (** [get_tracer_providers] returns a list of all existing tracer providers. *)

  val get_name_label : t -> string
  (** [get_name_label provider] returns the name label of the [provider]. *)

  val get_attributes : t -> (string * string) list
  (** [get_attributes provider] returns the list of attributes of the [provider]. *)

  val get_endpoints : t -> endpoint list
  (** [get_endpoints provider] returns list of endpoints of the [provider]. *)

  val get_enabled : t -> bool
  (** [get_name_label provider] returns whether or not the [provider] is enabled. *)
end

val enable_span_garbage_collector : ?timeout:float -> unit -> unit

val with_tracing :
     ?attributes:(string * string) list
  -> ?parent:Span.t option
  -> ?trace_context:TraceContext.t
  -> name:string
  -> (Span.t option -> 'a)
  -> 'a

val with_child_trace :
     ?attributes:(string * string) list
  -> ?trace_context:TraceContext.t
  -> Span.t option
  -> name:string
  -> (Span.t option -> 'a)
  -> 'a
(** [with_child_trace ?attributes ?parent ~name f] is like {!val:with_tracing}, but
  only creates a span if the [parent] span exists. *)

val get_observe : unit -> bool

val validate_attribute : string * string -> bool

(** [EnvHelpers] module is a helper module for the tracing library to easily 
    transition back and forth between a string list of environment variables to 
    a traceparent. 
    *)
module EnvHelpers : sig
  val traceparent_key : string
  (** [traceparent_key] is a constant the represents the key of the traceparent
      environment variable. 
      *)

  val of_traceparent : string option -> string list
  (** [of_traceparent traceparent_opt] returns a singleton list consisting of a
      envirentment variable with the key [traceparent_key] and value [v] if 
      [traceparent_opt] is [Some v]. Otherwise, returns an empty list. *)

  val to_traceparent : string list -> string option
  (** [to_traceparent env_var_lst] returns [Some v] where v is the value of the 
      environmental variable coresponding to the key [traceparent_key] from a 
      string list of environmental variables [env_var_lst]. If there is no such
      evironmental variable in the list, it returns [None].
      *)

  val of_span : Span.t option -> string list
  (** [of_span span] returns a singleton list consisting of a
      envirentment variable with the key [traceparent_key] and value [v], where
      [v] is traceparent representation of span [s] (if [span] is [Some s]).
        
      If [span] is [None], it returns an empty list.
      *)
end

(** [Propagator] is a utility module for creating trace propagators over arbitrary carriers. *)
module Propagator : sig
  module type S = sig
    type carrier

    val traceparent_of : carrier -> Span.t option
    (** [traceparent_of carrier] creates a span whose context is that encoded within the [carrier] input.
        If there is no traceparent encoded within the carrier, the function returns [None]. *)

    val with_tracing :
         ?attributes:(string * string) list
      -> name:string
      -> carrier
      -> (carrier -> 'a)
      -> 'a
  end

  module type PropS = sig
    type carrier

    val inject_into : TraceContext.t -> carrier -> carrier

    val extract_from : carrier -> TraceContext.t

    val name_span : carrier -> string
  end

  module Make : functor (P : PropS) -> S with type carrier = P.carrier
end
