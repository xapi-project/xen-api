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

(** Connects via stunnel (optionally via an external 'close fds' wrapper) to
    a host and port. If there is a suitable stunnel in the cache then this 
    will be used, otherwise we make a fresh one. *)
val connect :
  ?use_external_fd_wrapper:bool ->
  ?write_to_log:(string -> unit) -> string -> int -> Stunnel.t

(** Adds a reusable stunnel to the cache *)
val add : Stunnel.t -> unit

(** Given a host and port return a cached stunnel, or throw Not_found *)
val remove : string -> int -> Stunnel.t

(** Empty the cache of all stunnels *)
val flush : unit -> unit

(** GCs old stunnels *)
val gc : unit -> unit
