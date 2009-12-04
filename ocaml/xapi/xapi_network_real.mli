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
(** 'Real' network backend
 * @group Networking
 *)

(** Set-up a guest installer network [bridge]. *)
val setup_guest_installer_network :
  __context:Context.t -> string -> (string * string) list -> unit

(** Try to shut down the guest installer network [bridge]. *)
val maybe_shutdown_guest_installer_network : string -> unit
