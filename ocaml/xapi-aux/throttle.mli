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

module type SIZE = sig
  val n : unit -> int
  (** evaluated on first [execute] *)
end

module Make (_ : SIZE) : sig
  (** [execute f] up to [Size.n ()] in parallel. *)

  val execute : (unit -> 'a) -> 'a
end

module Batching : sig
  (** batching delay configuration *)
  type t

  (** internal argument for recursive calls *)
  type arg = private float

  val make : delay_before:float -> delay_between:float -> t
  (** [make ~delay_before ~delay_between] creates a configuration,
    where we delay the API call by [delay_before] once,
    and then with [delay_between] between each recursive call.
   *)

  val with_recursive : t -> ((arg -> 'b) -> arg -> 'b) -> unit -> 'b
  (** [with_recursive config f arg] calls [f self arg], where [self] can be used
    for recursive calls.

    [arg] needs to be passed through unchanged.

    A [delay_before] amount of seconds is inserted once, and [delay_between] is inserted between recursive calls,
    except the first one:
    {v
      delay_before
      f ...
        self ...
         f ...
          self ...
          delay_between
          f ...
     v}

    The delays are determined by [config], and [delay_between] uses an exponential backoff, up to [config.delay_between] delay.
   *)
end
