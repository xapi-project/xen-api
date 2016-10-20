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
exception Inconsistent_state of string

(** [create n] create a semaphore with initial value [n] (a positive integer).
    Raise {Invalid_argument} if [n] <= 0 *)
val create : int -> t

(** [acquire k s] block until the semaphore value is >= [k] (a positive integer),
    then atomically decrement the semaphore value by [k].
    Raise {Invalid_argument} if [k] <= 0 *)
val acquire : t -> int -> unit

(** [release k s] atomically increment the semaphore value by [k] (a positive
    integer).
    Raise {Invalid_argument} if [k] <= 0 *)
val release : t -> int -> unit

(** [execute_with_weight s k f] {acquire} the semaphore with [k],
    then run [f ()], and finally {release} the semaphore with the same value [k]
    (even in case of failure in the execution of [f]).
    Return the value of [f ()] or re-raise the exception if any. *)
val execute_with_weight : t -> int -> (unit -> 'a) -> 'a

(** [execute s f] same as [{execute_with_weight} s 1 f] *)
val execute : t -> (unit -> 'a) -> 'a
