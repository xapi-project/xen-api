(*
 * Copyright (c) Cloud Software Group, Inc.
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

(** Abort a helper process that has stopped doing any work. *)

val activity : int -> int64
(** [activity pid] is the number of bytes read and written so far by [pid] and
    its children, from [/proc/<pid>/io]. A process that has gone counts as
    zero. *)

val with_watchdog :
     name:string
  -> pid:int
  -> timeout:float
  -> abort:(unit -> unit)
  -> (unit -> 'a)
  -> 'a
(** [with_watchdog ~name ~pid ~timeout ~abort f] runs [f], calling [abort] once
    if [activity pid] stays the same for [timeout] seconds before [f] returns.

    If [abort] was called and [f] then raises, the exception is replaced by an
    [Internal_error] naming [name]. [abort] is never called once [f] has returned,
    and [with_watchdog] does not wait for the watchdog thread to notice. *)
