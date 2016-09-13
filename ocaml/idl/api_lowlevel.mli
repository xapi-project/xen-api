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

(** Operations generated for each field can be any of these *)
type field_op = Get | Set | Add | Remove

(** Operations generated for each object can be any of these *)
type obj_op = Make | Delete | GetAll

(** Represents an individual operation (on the wire), generating from
    either a field, an object or a message *)
type operation =
    Field of field_op * Datamodel_types.obj * Datamodel_types.field
  | Object of obj_op * Datamodel_types.obj
  | Msg of Datamodel_types.obj * Datamodel_types.message

(** Returns the object corresponding to an operation *)
val obj_of_operation : operation -> Datamodel_types.obj

(** Returns the XMLRPC wire name of the operation (eg Async.VM.do_clean_shutdown) *)
val wire_name_of_operation : sync:bool -> operation -> string

(** A 'lowlevel api' consists of an association list of objects and their
    generated operations *)
type t = (Datamodel_types.obj * operation list) list

(** Filter function over operations *)
val filter : (operation -> bool) -> t -> t

(** Converts a raw high-level datamodel API into a low-level one. Performs basic
    filtering (eg doesn't ever generate an Add operation for a field consisting of
    a String (only Sets and Maps) *)
val of_api : Dm_api.api -> t
