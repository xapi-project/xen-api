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
(** Thrown when parsing malformed XML *)
exception Unmarshall_error of string

(** Type of hashtables supported by this module *)
type h = (string, string) Hashtbl.t

(** Serialise the given hashtbl to the given Xmlm output *)
val to_xml: h -> Xmlm.output -> unit

(** Deserialise a hashtbl from the given Xmlm input *)
val of_xml: Xmlm.input -> h
