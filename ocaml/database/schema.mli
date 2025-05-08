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

  val marshal : t -> string

  val unmarshal : Type.t -> string -> t

  module Unsafe_cast : sig
    val string : t -> string

    val set : t -> string list

    val pairs : t -> (string * string) list
  end
end

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
