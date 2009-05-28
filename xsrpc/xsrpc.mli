(*
 * Copyright (C) 2008 Citrix Ltd.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 *)

type t

exception Stop_listen
exception Timeout
exception Protocol_error of string

type status = Error | Success

val bind : int -> string -> t
val query_with_id : t -> string -> string -> string -> (status * string)
val query : t -> string -> string -> (status * string)
val listen : string -> (int -> string -> string -> string -> (status * string) option) -> unit
