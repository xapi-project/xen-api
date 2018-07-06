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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type t
(** A role containing a set of privileges *)

val superuser: t
(** The [superuser] role has all privileges *)

val of_domain : int -> t
(** The role associated with a specific domain id *)

val restrict: t -> int -> t
(** [restrict role domid] returns a new role which contains the subset
    of [role] which applies to domain [domid] *)

val set_target: t -> int -> t
(** [set_target role domid] needs rationalising *)

type permission =
	| READ         (** ability to read the value associated with a node *)
	| WRITE        (** ability to modify the value associated with a node *)
	| CHANGE_ACL   (** ability to change the ACL associated with a node *)
	| DEBUG        (** ability to invoke debug operations *)
	| INTRODUCE    (** ability to grant access to other domains *)
	| ISINTRODUCED (** ability to query whether a domain has been introduced *)
	| RESUME       (** ability to restore access to previously shutdown domains *)
	| RELEASE      (** ability to revoke access from other domains *)
	| SET_TARGET   (** ability to allow one domain to impersonate a specific other *)
	| RESTRICT     (** ability to imperonate a specific other domain *)
	| CONFIGURE    (** ability to view/edit the daemon configuration *)

exception Permission_denied
(** Thrown by the [check] function if role does not have a specific permission *)

val check: t -> permission -> Xs_protocol.ACL.t -> unit
(** [check role permission acl] throws [Permission_denied] if [role] does not
    have [permission] according to the access control list [acl] *)

val has: t -> permission -> unit
(** [has role permission] throws [Permission_denied] if [role] does not
    have [permission] *)
