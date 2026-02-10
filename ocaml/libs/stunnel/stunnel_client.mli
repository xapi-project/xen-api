(*
 * Copyright (C) Systems Inc.
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

val get_verify_by_default : unit -> bool

val set_verify_by_default : bool -> unit

val pool : unit -> Stunnel.verification_config option
(** [pool ()] returns the configuration that's meant to be used to connect to
    other xapi hosts in the pool *)

val appliance : unit -> Stunnel.verification_config option
(** [appliance ()] returns the configuration that's meant to be used to connect
    to appliances providing services, like WLB or a licensing server. *)

val external_host : string -> Stunnel.verification_config option
(** [external_host path] returns the configuration that's meant to be used to connect to
    a xapi hosts outside the pool. This is useful, for example, to provide an
    update repository to download updates from. *)
