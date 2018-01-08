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

(** Operate a small cache of stunnels so we can re-use them for repeated calls.

    Caveats:
   * stunnel donators should only donate stunnels which they knows are connected
     to the main HTTP request loop in the server -- HTTP 1.1 should be used and 
     the connection should be kept-alive.
*)


(** Connects via stunnel (optionally via an external 'fork/exec helper') to
    a host and port. If there is a suitable stunnel in the cache then this 
    will be used, otherwise we make a fresh one. *)
val connect :
  ?use_fork_exec_helper:bool ->
  ?write_to_log:(string -> unit) -> string -> int -> bool -> Stunnel.t

(** Adds a reusable stunnel to the cache *)
val add : Stunnel.t -> unit

(** Given a host and port return a cached stunnel, or throw Not_found *)
val remove : string -> int -> bool -> Stunnel.t

(** Empty the cache of all stunnels *)
val flush : unit -> unit

(** GCs old stunnels *)
val gc : unit -> unit
