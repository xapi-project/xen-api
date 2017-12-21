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
(** Buffered IO with timeouts *)

(** {2 Abstract type of inputs} *)
type t

val of_fd : Unix.file_descr -> t

val fd_of : t -> Unix.file_descr

val infinite_timeout : float

(** {2 Input functions} *)

(** Input one line terminated by \n *)
val input_line : ?timeout:float -> t -> string

(** Input 'len' characters from ic and put them into the string 'str' starting from 'from' *)
val really_input : ?timeout:float -> t -> string -> int -> int -> unit

val really_input_buf : ?timeout:float -> t -> int -> string

(** {2 Exceptions} *)

(** Waited too long for data to appear *)
exception Timeout

exception Eof

(** Raised by input_line only *)
type err =
  | Too_long   (** Line input is > 1024 chars *)
  | No_newline (** EOF found, with no newline *)

exception Line of err

(** {2 Internal functions} *)
val is_buffer_empty : t -> bool
val assert_buffer_empty : t -> unit

(* val assert_buffer_empty : t -> unit
   val shift : t -> unit
   val got_line : t -> int
   val is_full : t -> bool
   val fill_buf : buffered:bool -> t -> float -> unit
*)
