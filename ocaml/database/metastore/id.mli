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

(** Unique identifiers implemented with {!Uuidm}

  See {{!page-metastore_design.section-"unique-identifiers"} the design}.
*)

include Map.OrderedType with type t = Uuidm.t

(** A {!module:Map} with {!t} identifiers as keys *)
module Map : sig
  include Map.S with type key = Uuidm.t

  val dump : 'a Fmt.t -> 'a t Fmt.t
  (** [dump pp_val ppf t] dumps a representation of [t] on [ppf] for debugging,
      using [pp_val] to format values. *)
end

(** A {!module:Set} with {!t} identifiers as keys *)
module Set : sig
  include Set.S with type elt = Uuidm.t

  val dump : t Fmt.t
  (** [dump ppf t] dumps a representation of [t] on [ppf] for debugging
      purposes. *)
end
