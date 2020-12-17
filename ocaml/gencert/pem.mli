(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

(** the content of a PEM file *)
type t = {private_key: string; host_cert: string; other_certs: string list}

val key : string Angstrom.t
(** parsers for various cert components *)

val cert : string Angstrom.t

val host_pem : t Angstrom.t

val parse_string_using : 'a Angstrom.t -> string -> ('a, string) result
(** parse a string using a parser *)

val parse_file_using : 'a Angstrom.t -> string -> ('a, string) result
(** parse a file using a parser *)

val parse_string : string -> (t, string) result
(** [parse_string str] parses a PEM encoded sequence of certificates and
privates keys into components. *)

val parse_file : string -> (t, string) result
(** [parse_file path] parses a PEM encoded sequence of certificates from
file [path] *)
