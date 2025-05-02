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

module Type : sig
  type t = String | Set | Pairs [@@deriving sexp_of]

  exception Error of t * t
end

module Value : sig
  type t =
    | String of string
    | Set of string list
    | Pairs of (string * string) list
  [@@deriving sexp_of]

  val string : string -> t

  val set : string list -> t

  val pairs : (string * string) list -> t

  val marshal : t -> string

  val unmarshal : Type.t -> string -> t

  module Unsafe_cast : sig
    val string : t -> string

    val set : t -> string list

    val pairs : t -> (string * string) list
  end
end

type present = [`Present of Value.t]

type absent = [`Absent]

type maybe = [`Absent | `Present of Value.t]

(** Abstract type, ensuring marshaled form was created from a Value.t.

    For backwards compatibility this can also be created from a marshaled form,
    but then retrieving the value requires its {Type.t} to be known.

    A polymorphic variant is used to decide at the type level when we are always guaranteed to have
    a {type:Value.t} available, from the situations where we do not.

    When {type:Value.t} is not available at construction time then unmarshaling can incurr a performance
    overhead every time it is called, because the value here is immutable, and caching only happens at construction time.

    No guarantee is made about the encoding of the values (in the future we could also cache whether we've already checked
    for [utf8_xml] compatibility).
 *)
module CachedValue : sig
  type +!'a t

  val v : Value.t -> [> present] t
  (** [v value] creates a cached value, storing the value and its serialized form.

      [O(1)] for strings, and [O(n)] for sets and maps, where [n] is the result size in marshaled form.
   *)

  val of_string : string -> [> absent] t
  (** [of_string marshaled] created a cached value from a marshaled form.

    This is provided for backwards compatibility, e.g. for DB RPC calls which only send the marshaled form without type information.
    [O(1)] operation, but {!val:unmarshal} can be [O(n)] for sets and maps.
    *)

  val string_of : 'a t -> string
  (** [string_of t] returns [t] in marshaled form.

    This works on any cached value types.

    [O(1)] operation, marshaling happens at construction time.
    *)

  val value_of : [< present] t -> Value.t
  (** [value_of t] returns [t] in {!type:Value.t} form.

    This only works on cached values created by {!val:v}.

    [O(1)] operation, stored at construction time.
    *)

  val unmarshal : Type.t -> [< maybe] t -> Value.t
  (** [unmarshal ty t] returns [t] in Value.t form if known, or unmarshals it.

    This works on any cached value.
    When the value was created by {!val:v} this is an [O(1)] operation.
    When the value was created by {!val:of_string} this is an [O(1)] operation for strings,
    and  [O(n)] operation for sets and maps, as it requires unmarshaling.
    The unmarshaled value is not cached, so each unmarshal call has the same cost.
     *)
end

type cached_value = present CachedValue.t

type maybe_cached_value = maybe CachedValue.t

module Column : sig
  type t = {
      name: string
    ; persistent: bool
    ; empty: Value.t
    ; default: Value.t option
    ; ty: Type.t
    ; issetref: bool
  }
  [@@deriving sexp_of]

  val name_of : t -> string
end

val tabulate : 'a list -> key_fn:('a -> 'b) -> ('b, 'a) Hashtbl.t

val values_of_table : ('a, 'b) Hashtbl.t -> 'b list

module Table : sig
  type t' = {name: string; columns: Column.t list; persistent: bool}
  [@@deriving sexp_of]

  val sexp_of_t' : t' -> Sexplib0.Sexp.t

  type t = {
      name: string
    ; columns: (string, Column.t) Hashtbl.t
    ; persistent: bool
  }
  [@@deriving sexp_of]

  val t'_of_t : t -> t'

  val t_of_t' : t' -> t

  val find : string -> t -> Column.t

  val create : name:string -> columns:Column.t list -> persistent:bool -> t

  val name_of : t -> string
end

type relationship = OneToMany of string * string * string * string

val sexp_of_relationship : relationship -> Sexplib0.Sexp.t

module Database : sig
  type t' = {tables: Table.t list}

  val sexp_of_t' : t' -> Sexplib0.Sexp.t

  type t = {tables: (string, Table.t) Hashtbl.t}

  val t_of_t' : t' -> t

  val t'_of_t : t -> t'

  val sexp_of_t : t -> Sexplib0.Sexp.t

  val find : string -> t -> Table.t

  val of_tables : Table.t list -> t
end

type foreign = (string * string * string) list

val sexp_of_foreign : foreign -> Sexplib0.Sexp.t

module ForeignMap : sig
  include Map.S with type key = string

  type t' = (key * foreign) list

  val sexp_of_t' : t' -> Sexplib0.Sexp.t

  type m = foreign t [@@deriving sexp_of]
end

type t = {
    major_vsn: int
  ; minor_vsn: int
  ; database: Database.t
  ; one_to_many: ForeignMap.m
  ; many_to_many: ForeignMap.m
}
[@@deriving sexp_of]

val database : t -> Database.t

val table : string -> t -> Table.t

val empty : t

val is_table_persistent : t -> string -> bool

val is_field_persistent : t -> string -> string -> bool

val table_names : t -> string list

val one_to_many : ForeignMap.key -> t -> foreign

val many_to_many : ForeignMap.key -> t -> foreign
