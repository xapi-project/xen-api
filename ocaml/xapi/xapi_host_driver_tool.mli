(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

type variant = {
    version: string  (** just a string, not interpreted right now *)
  ; hw_present: bool  (** hardware present *)
  ; priority: float  (** higher = higher priority *)
  ; dev_status: string  (** development status, like alpha, beta *)
}

type driver = {
    ty: string  (** category, like "network" *)
  ; name: string  (** unique *)
  ; descr: string
  ; info: string
  ; selected: string option  (** refers to named variant below *)
  ; active: string option  (** refers to named variant below *)
  ; variants: (string * variant) list  (** named variants, name is unique *)
}

type t = {protocol: string; operation: string; drivers: (string * driver) list}

val parse : string -> (string * driver) list
(** parse from a string *)

val read : string -> (string * driver) list
(** read from a file whose path is provided *)

val call : string list -> string
(** invoke drivertool with argumtns and return stdout *)

(** install a mock drivertool.sh *)
module Mock : sig
  val install : unit -> unit
end
