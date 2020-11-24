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

open Datamodel_types

(* An abstract type representing an entire datamodel-style API *)
type api

val print_api_stats : api -> unit

(* Extract a list of relations from an API *)
val relations_of_api : api -> relation list

(* Extract a list of the objects from an API *)
val objects_of_api : api -> obj list

val get_field_by_name : api -> objname:string -> fieldname:string -> field
(** Find and return a field by name *)

val get_obj_by_name : api -> objname:string -> obj
(** Find and return an object by name *)

val field_exists : api -> objname:string -> fieldname:string -> bool
(** True if the named field exists *)

val filter : (obj -> bool) -> (field -> bool) -> (message -> bool) -> api -> api
(** Apply a predicate to every object, field and message, to generate a sub-API *)

val map : (field -> field) -> (message -> message) -> api -> api
(** Transform an API *)

val make : obj list * relation list -> api
(** Create an API from raw components (implicitly calls check) *)

val check :
  api -> (string * string) list (* list of "emergency calls" *) -> unit
(** Perform basic sanity-checking on the API to ensure various constraints
    are satisfied *)
