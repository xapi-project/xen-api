(*
 * Copyright (C) Cloud Software Group, Inc.
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

val dump : 'a Rpc.Types.typ -> 'a Fmt.t
(** [dump typ_of formatter v] pretty prints [v] by converting it to an {!Rpc.t}
  described by [typ_of] and then formatting that representation.

  This doesn't use {!Rpc.to_string} which would clutter the output with type
  information.
*)

val using :
     aname:string
  -> ('a -> 'b)
  -> ('b -> 'a)
  -> ?test_data:'a list
  -> 'b Rpc.Types.typ
  -> 'a Rpc.Types.typ
(** [using ~aname conv_to conv_from ?test_data typ_of] is an {!Rpc} serializer
    implemented by first converting the OCaml type using [conv_to]
    and then using the [typ_of] serializer of that type.
    Deserialization is implemented using [typ_of] and [conv_from].
    Tests are generated from [test_data] if supplied, otherwise random ['b]
    values are generated and [conv_from] is used to generate test inputs,
    however this will only work if [conv_from] accepts any value for ['b] as
    valid.

    This can be a convenient way for writing a serializer for types like
    Maps or Sets where we cannot modify the original type.
*)

val serialize : 'a Rpc.Types.typ -> 'a -> string
(** [serialize typ_of v] serializes [v] using [typ_of] into a string
    representation.

    The exact serialization format is implementation defined (currently JSON),
    but future versions of XAPI should be able to deserialize this
*)

val deserialize :
  'a Rpc.Types.typ -> string -> ('a, [> Rresult.R.msg]) Rresult.result
(** [deserialize typ_of str] deserializes [str] using [typ_of].
    [str] must've been created by {!val:serialize}.
 *)

val typ_of_stringmap : 'a Rpc.Types.typ -> 'a Map.Make(String).t Rpc.Types.typ
(** [typ_of_stringmap elt_typ_of] is a serializer for string maps using
    [elt_typ_of] for values. *)

val typ_of_path : Fpath.t Rpc.Types.typ
(** [typ_of_path] is a serializer for filenames *)

val string_to_file_exn : Fpath.t -> string -> unit
(** [string_to_file_exn path str] persists [str] in the file at [path].

  @raises Unix.Unix_error if saving fails
*)

val string_of_file_exn : Fpath.t -> string
(** [string_of_file_exn path] loads the entire file [path] as a string.

  @raises Unix.Unix_error if reading fails
*)
