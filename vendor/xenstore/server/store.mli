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

exception Already_exists of string
(** thrown when a (watch) name already exists *)

module Node : sig
	(** A Node in the main xenstore tree *)

	type t

	val create : string -> int -> Xs_protocol.ACL.t -> string -> t
	(** [create name creator perms value] returns fresh Node.t *)

	val get_perms: t -> Xs_protocol.ACL.t
	(** [get_perms t] returns the permissions attached to [t] *)

end

module Name : sig

	type t
	(** The name of an entity which may be watched. Note that not all these
		entities are stored in the xenstore tree (e.g. @introduceDomain) *)

	val introduceDomain: t
	(** The special watch when a domain is created *)

	val releaseDomain: t
	(** The special watch when a domain shuts down *)

	val of_string: string -> t

	val to_string: t -> string

	val is_relative: t -> bool
	(** [is_relative t] is true if [t] contains a relative path *)

	val make_absolute: t -> string -> t
	(** [make_absolute t basename] returns [t] as an absolute path,
		treating paths relative to [basename] *)

	val to_key: t -> string list
	(** [to_key t] returns a key which can be used as a path in a Trie *)

end

module Path : sig

	type t
	(** Represents an absolute path within the xenstore tree *)

	exception Doesnt_exist of string

	val doesnt_exist: t -> 'a
	(** [doesnt_exist path] raises the Doesnt_exist exception *)

	val getdomainpath: int -> t
	(** [getdomainpath domid] returns the default directory for [domid] *)

	val create: string -> t -> t
	(** [create path default] is the absolute path of [path], where if [path]
		is relative then it is resolved relative to [default] *)

	val to_name: t -> Name.t
	(** [to_name t] returns the associated Name.t, suitable for watching *)

	val to_string: t -> string
	(** [to_string t] returns [t] as a '/'-separated path string *)

	val to_string_list: t -> string list
	(** [to_string_list t] returns [t] as a list of path element strings *)

	val of_string_list: string list -> t
	(** [of_string_list l] returns [l] as an instance of type [t] *)

	val get_hierarchy: t -> t list
	(** [get_hierarchy t] returns all t's on the path from the root node to [t] *)

	val get_common_prefix: t -> t -> t
	(** [get_common_prefix a b] returns the largest common prefix of [a] and [b] *)

	val get_parent: t -> t
	(** [get_parent t] returns the parent node of [t]. The parent of the root node
		is itself. *)

	val make_relative: t -> Name.t -> Name.t
	(** [make_relative base name] returns a Name.t which is [name] transformed into
		a relative path from [base] *)

end

val lookup: Node.t -> Path.t -> Node.t option
(** [lookup node path] follows [path] from [node] and returns the node it
	finds, or None *)

type t =
{
	mutable stat_transaction_coalesce: int;
	mutable stat_transaction_abort: int;
	mutable root: Node.t;
	mutable quota: Quota.t;
}

val set_root: t -> Node.t -> unit
val set_quota: t -> Quota.t -> unit

val create: unit -> t

val copy: t -> t

val exists: t -> Path.t -> bool

val write: t -> int -> Perms.t -> Path.t -> string -> unit

val mkdir: t -> int -> Perms.t -> Path.t -> unit

val setperms: t -> Perms.t -> Path.t -> Xs_protocol.ACL.t -> unit

val rm: t -> Perms.t -> Path.t -> unit

val ls: t -> Perms.t -> Path.t -> string list

val read: t -> Perms.t -> Path.t -> string

val getperms: t -> Perms.t -> Path.t -> Xs_protocol.ACL.t


val set_node: t -> Path.t -> Node.t -> Quota.t -> Quota.t -> unit

val mark_symbols: t -> unit
