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
(** Store Management *)


(** {2 Store Nodes} *)

module Node :
sig
	type t
	(** The type of nodes. *)

	val get_name : t -> string
	(** Get the node name *)

	val get_perms : t -> Perms.Node.t
	(** Get the node permissions. *)

	val get_value : t -> string
	(** Get the node value *)

	val unpack : t -> string * Perms.Node.t * string
	(** [unpack n] returns the node name, the node permissions and the node value of node [n]. *)
end

(** {2 Store Paths} *)

module Path :
sig
	type t
	(** The type of paths. *)

	val create : string -> string -> t
	(** [create path connection_path] creates a new path such that:
-         if [path] starts by '/', then the new created path corresponds to [path]
-         else the created path corresponds to [connection_path]/[path] *)

	val of_string : string -> t
	(** Convert a string into a path. *)

	val to_string : t -> string
	(** Convert a path into a sting. *)

	val to_string_list : t -> string list
	(** Returns the list of names appearing in the path, ie. [to_string_list "/a/b/c"] will return the list \[a;b;c\] *)
 
	val get_parent : t -> t
	(** Get the parent path. *)

	val get_hierarchy : t -> t list
	(** Get the hierarchy of paths of a given path. For instance, [get_hierarchy "/local/domain/0"] returns [ "/"; "/local"; "/local/domain"; "/local/domain/0"] *)

	val get_common_prefix : t -> t -> t
	(** Get the common prefix between two paths. The granularity is the node name, not the characters, ie. [get_common_prefix "/foo/bar" "/fo/bar"] returns ["/"] *) 

	val get_node : Node.t -> t -> Node.t option
	(** [get_node node path] returns the children of [node] corresponding to the path [path]. *)
end

(** {2 Store Management} *)

type t
(** The type of stores. *)

(** {8 Basic Accessors} *)

val create : unit-> t
(** Create an empty store having {!Quota.maxent} and {!Quota.maxsize} as quota. *)

val copy : t -> t

val get_root : t -> Node.t
val set_root : t -> Node.t -> unit

val get_quota : t -> Quota.t
val set_quota : t -> Quota.t -> unit

val get_node : t -> Path.t -> Node.t option
val set_node : t -> Path.t -> Node.t -> unit

(** {8 Reading Functions} *)

val path_exists : t -> Path.t -> bool
val read : t -> Perms.Connection.t -> Path.t -> string
val ls : t -> Perms.Connection.t -> Path.t -> string list
val getperms : t -> Perms.Connection.t -> Path.t -> Perms.Node.t

(** {8 Writing Functions} *)

val write : t -> Perms.Connection.t -> Path.t -> string -> unit
val mkdir : t -> Perms.Connection.t -> Path.t -> unit
val rm : t -> Perms.Connection.t -> Path.t -> unit
val setperms : t -> Perms.Connection.t -> Path.t -> Perms.Node.t -> unit

(** {8 Dumping functions} *)

val dump_fct : t -> (Path.t -> Node.t -> unit) -> unit
val dump : t -> out_channel -> unit
val dump_stdout : t -> unit
val dump_buffer : t -> Buffer.t

(** {8 Helpers} *)

type ops = {
	store: t;
	write: Path.t -> string -> unit;
	mkdir: Path.t -> unit;
	rm: Path.t -> unit;
	setperms: Path.t -> Perms.Node.t -> unit;
	ls: Path.t -> string list;
	read: Path.t -> string;
	getperms: Path.t -> Perms.Node.t;
	path_exists: Path.t -> bool;
}

val get_ops : t -> Perms.Connection.t -> ops

(** {8 Statistics} *)

val incr_transaction_coalesce : t -> unit
val incr_transaction_abort : t -> unit
val stats : t -> int * int * int

(** {8 Garbage Collection of Symbols} *)

val mark_symbols : t -> unit
