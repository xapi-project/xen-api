(* Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

val ( // ) : string -> string -> string

val snake_to_camel : string -> string

type fields = string * Datamodel_types.field list

val render_template :
  string -> Mustache.Json.t -> ?newline:bool -> unit -> string

val generate_file :
  rendered:string -> destdir:string -> output_file:string -> unit

val objs_and_convert_functions :
  Datamodel_types.obj list -> (string * Mustache.Json.t) list * Mustache.Json.t

module Json : sig
  val xenapi :
    Datamodel_types.obj list -> (string * Mustache.Json.t) list * fields list

  val all_enums : Datamodel_types.obj list -> Mustache.Json.t

  val api_messages : Mustache.Json.value list

  val api_errors : Mustache.Json.value list
end
