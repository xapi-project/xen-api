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

(** The representation of Epoch concept in RPM versioning *)
module Epoch : sig
  type t = int option

  val epoch_none : string

  val of_string : string -> t

  val to_string : t -> string
end

(** The representation of a RPM Name-Version-Release (NVR) *)
module Pkg : sig
  type t = {
      name: string
    ; epoch: Epoch.t
    ; version: string
    ; release: string
    ; arch: string
  }

  type order

  val string_of_order : order -> string

  val to_name_arch_string : t -> string

  val to_fullname : t -> string

  val of_fullname : string -> t option

  val compare_version_strings : string -> string -> order

  val to_epoch_ver_rel_json : t -> Yojson.Basic.t

  val lt : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val gt : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val eq : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val lte : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val gte : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool
end
