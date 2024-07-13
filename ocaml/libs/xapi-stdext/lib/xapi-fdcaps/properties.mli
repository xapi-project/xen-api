(*
 * Copyright (C) 2023 Cloud Software Group
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

(** Static file property checking

 When file descriptors are open they have:
 * a file kind: ({!type:reg}, {!type:blk), {!type:chr}, {!type:lnk}, {!type:fifo}, {!type:sock})
 * an open mode: {!type:readonly}, {!type:writeonly}, {!type:readwrite} depending on the {!type:Unix.open_flag} used

 Depending on these properties there are {!val:Unix} operations on file descriptors that always fail, e.g.:
 * writing to a read-only file
 * socket operation on non-socket
 * seeking on a pipe
 * ...

 The read-write property can also change at runtime:
 * {!val:Unix.shutdown} can be used to shutdown the socket in either direction

 We track the property of the file at open time, and we reject operations that we can statically determine to always fail.
 This doesn't guarantee the absence of runtime errors, but catches programming errors like accidentally swapping the read and write ends of a pipe,
 or attempting to set a socket timeout on a pipe.

 We use polymorphic variants as type parameters to track these properties: they are simple to use and work well with type inference.
 They also allow dispatching at runtime on the actual capabilities available, although they could be purely compile-time types (phantom types).

 Alternative approaches (typically with phantom types):
 * abstract types as phantom type parameters: don't work well with type inference, and cannot express removing a property
 * behavioural types (recursive polymorphic variants) can express removing a property, but error messages and type signatures become too long
 * object phantom types strike a good balance between clarity of error messages and complexity of type signatures

 It'd be also possible to use purely boolean properties (capabilities), but that causes a long type signature, and allows expressing meaningless combinations,
 such as a file that is both a socket and seekable, which is impossible.
 Instead we directly map the concepts from the Unix module to a polymorphic variant (e.g. instead of separate read and write properties we have the 3 properties from the Unix module).

{b References.}
{ul
  {- Yaron Minsky.
   {e {{:https://blog.janestreet.com/howto-static-access-control-using-phantom-types/}HOWTO: Static access control using phantom types}. 2008.}}
  {- KC Sivaramakrishnan.
   {e {{:https://kcsrk.info/ocaml/types/2016/06/30/behavioural-types/#file-descriptors}Behavioural types}. 2016.}}
  {- Florian Angeletti.
   {e {{:https://stackoverflow.com/a/55081337}Object phantom types}. 2019.}}
}
*)

(** {1 File properties}

 Polymorphic type parameters for the set of properties a file descriptor may have on a given codepath.
 E.g. {[> rdonly | wronly]} means that this codepath may be reached by a file descriptor with either of these properties,
 although of course a file descriptor can only have one of these properties at a time.

 Usual rules for using polymorphic variants apply:
 * when receiving a type declare an upper bound on what the code can handle, e.g. : {[< readable]}
 * when returning a type declare a lower bound to make type inference/unification work, e.g. : {[> readable]}

 Naming conventions:
 * [type property ]
 * [val as_property : [< property] t -> [> property] t]
 * [val as_property_opt: [< all] t -> [> property] t option]
*)

(** file properties: {!type:rw}, {!type:kind}

  Upper bounds are avoided here to make the type usable in functors
 *)
type (+!'a, +!'b) props

(** {2 Read/write property}

  A file can be read-only, write-only, or read-write.
*)

(** file opened with {!val:Unix.O_RDONLY} or the read end of a pipe *)
type rdonly = [`rdonly]

(** file opened with {!val:Unix.O_WRONLY} or the write end of a pipe *)
type wronly = [`wronly]

(** file opened with {!val:Unix.O_RDWR} or a socketpair *)
type rdwr = [`rdwr]

(** file opened with either {!val:Unix.O_RDONLY} or {!val:Unix.O_RDWR} *)
type readable = [rdonly | rdwr]

(** file opened with either {!val:Unix.O_WRONLY} or {!val:Unix.O_RDWR} *)
type writable = [wronly | rdwr]

(** the read-write property *)
type rw = [rdonly | wronly | rdwr]

val pp_rw : Format.formatter -> [< rw] -> unit
(** [pp_rw formatter rw] pretty prints the [rw] state on [formatter]. *)

(** {2 File kind} *)

(** A regular file, {!val:Unix.S_REG} *)
type reg = [`reg]

(** A block device, {!val:Unix.S_BLK} *)
type blk = [`blk]

(** A character device, {!val:Unix.S_CHR} *)
type chr = [`chr]

(** A directory, {!val:Unix.S_DIR} *)
type dir = [`dir]

(** A symbolic link, {!val:Unix.S_LNK} *)
type lnk = [`lnk]

(** A pipe or FIFO, {!val:Unix.S_FIFO} *)
type fifo = [`fifo]

(** A socket, {!val:Unix.S_SOCK} *)
type sock = [`sock]

(** a {!type:Unix.file_kind} *)
type kind = [reg | blk | chr | dir | lnk | fifo | sock]

val pp_kind : Format.formatter -> [< kind] -> unit
(** [pp_kind formatter kind] pretty prints [kind] on [formatter]. *)

(** {2 Property type} *)

(** upper bounds on properties *)
type (+!'a, +!'b) t = (([< rw] as 'a), ([< kind] as 'b)) props

(** {2 Operations on read-write properties} *)

val as_readable : ([< readable], 'a) t -> ([> readable], 'a) t
(** [as_readable t] requires [t] to be readable and ignores the writeonly property. *)

val as_writable : ([< writable], 'a) t -> ([> writable], 'a) t
(** [as_writable t] requires [t] to be writable and ignores the readonly property. *)

val as_readable_opt : ([< rw], 'a) t -> ([> readable], 'a) t option
(** [as_readable_opt t] tests for the presence of the readable property at runtime.

  @returns [Some t] when [t] is readable, and [None] otherwise
*)

val as_writable_opt : ([< rw], 'a) t -> ([> writable], 'a) t option
(** [as_writable_opt t] tests for the presence of the writable property at runtime.

  @returns [Some t] when [t] is writable, and [None] otherwise
*)

(** {2 Operations on file kind properties} *)

val to_unix_kind : kind -> Unix.file_kind
(** [to_unix_kind kind] converts the polymorphic variant [kind] to {!type:Unix.file_kind} *)

val of_unix_kind : Unix.file_kind -> kind
(** [of_unix_kind kind] converts the {!type:Unix.file_kind} to {!type:kind}. *)

(** pipe, FIFO or socket that may raise {!val:Unix.ESPIPE} *)
type espipe = [fifo | sock]

val as_kind_opt : ([< kind] as 'a) -> ('b, [< kind]) t -> ('b, 'a) t option
(** [as_kind_opt kind t] checks whether [t] is of type [kind].

  @returns [Some t] if [t] is of type [kind], and [None] otherwise
 *)

(** {2 Properties derived from file kind} *)

(** seek may be implementation defined on devices other than regular files or block devices.

  E.g. {!type:chr} devices would always return 0 when seeking, which doesn't follow the usual semantics of seek.
*)
type seekable = [reg | blk]

(** truncate only works on regular files *)
type truncatable = reg

(** {2 Create properties} *)

val make : ([< rw] as 'a) -> ([< kind] as 'b) -> ('a, 'b) t
(** [make rw kind] builds a file property *)

(** {2 Pretty printing} *)

val pp : Format.formatter -> (_, _) t -> unit
(** [pp formatter t] pretty prints the properties on [formatter]. *)
