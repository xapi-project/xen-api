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

  val make : delay_before:Mtime.Span.t -> delay_between:Mtime.Span.t -> t
  (** [make ~delay_before ~delay_between] creates a configuration,
    where we delay the API call by [delay_before] once,
    and then with [delay_between] between each recursive call.
   *)

  val with_recursive_loop : t -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  (** [with_recursive config f arg] calls [f self arg], where [self] can be used
    for recursive calls.

    [arg] is an argument that the implementation of [f] can change between recursive calls for its own purposes,
    otherwise [()] can be used.

    A [delay_before] amount of seconds is inserted once, and [delay_between/8] is inserted between recursive calls,
    except the first one, and delays increase exponentially until [delay_between] is reached
    {v
      delay_before
      f ...
        (self[@tailcall]) ...
         f ...
          (self[@tailcall]) ...
          delay_between/8
          f ...
            (self[@tailcall]) ...
            delay_between/4
            f ...
     v}

    The delays are determined by [config], and [delay_between] uses an exponential backoff, up to [config.delay_between] delay.
   *)
end
