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

val render_template :
  string -> Mustache.Json.t -> ?newline:bool -> unit -> string

val generate_file :
  rendered:string -> destdir:string -> output_file:string -> unit

module Json : sig
  type enum = (string * string) list

  module StringMap : Map.S with type key = string

  type enums = enum StringMap.t

  val suffix_of_type : Datamodel_types.ty -> string

  val string_of_ty_with_enums : Datamodel_types.ty -> string * enums

  val xenapi : Datamodel_types.obj list -> (string * Mustache.Json.t) list

  val all_enums : Datamodel_types.obj list -> Mustache.Json.t

  val api_messages : Mustache.Json.value list

  val api_errors : Mustache.Json.value list
end

module Convert : sig
  type params = {func_suffix: string; value_ty: string}

  type params_of_option = {func_suffix: string}

  type params_of_set = {
      func_suffix: string
    ; value_ty: string
    ; item_fp_type: string
  }

  type params_of_record_field = {
      name: string
    ; name_internal: string
    ; name_exported: string
    ; func_suffix: string
    ; type_option: bool
  }

  type params_of_record = {
      func_suffix: string
    ; value_ty: string
    ; fields: params_of_record_field list
  }

  type params_of_enum_item = {value: string; name: string}

  type params_of_enum = {
      func_suffix: string
    ; value_ty: string
    ; items: params_of_enum_item list
  }

  type params_of_map = {
      func_suffix: string
    ; value_ty: string
    ; key_ty: string
    ; val_ty: string
  }

  type convert_params =
    | Simple of params
    | Int of params
    | Float of params
    | Time of params
    | Ref of params
    | Option of params_of_option
    | Set of params_of_set
    | Enum of params_of_enum
    | Record of params_of_record
    | Map of params_of_map

  val template_of_convert : convert_params -> string

  val to_json : convert_params -> Mustache.Json.value

  val fields : string -> (string * string * bool) list

  val of_ty : Datamodel_types.ty -> convert_params

  val of_serialize : convert_params -> Mustache.Json.t

  val of_deserialize : convert_params -> Mustache.Json.t

  val event_batch : Mustache.Json.t

  val interface : Mustache.Json.t
end
