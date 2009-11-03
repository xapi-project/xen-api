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

(** An example implementation of a licensing service which always returns "real" 
    licenses that never expire. *)

val initialise : string -> int32 -> string -> string * int32
(** Obtain a license *)
val shutdown : unit -> bool
(** Release the license *)
val reopen_logs : unit -> bool
(** Close and re-open the log file *)

