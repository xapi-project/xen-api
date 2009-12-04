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
(** Client module to handle v6 licenses with the v6 licensing daemon.
 *  V6 licenses enable the features of Citrix Essentials for XenServer.
 * @group Licensing
 *)

(** {2 State variables} *)

val licensed : string option ref
(** Equal to the edition string, if a license has been checked out,
 *  or None otherwise *)
val expires : float ref
(** Equal to the expiry date, if a license has been checked out *)
val grace : bool ref
(** Reflects whether the current license is a grace license, 
 *  if a license has been checked out *)

(** {2 Obtaining and Releasing a License} *)

val get_v6_license : __context:Context.t -> host:[`host] Ref.t -> edition:string -> unit
(** Obtain a v6 license via the licensing daemon. The edition parameter is
 *  either "enterprise" or "platinum". Uses the contact details in host.license_server. *)
val release_v6_license : unit -> unit
(** Release the current v6 license (if any) via the licensing daemon *)

