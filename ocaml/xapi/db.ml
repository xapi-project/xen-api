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
(**
 * @group Database Operations
*)

include Db_actions.DB_Action
let is_valid_ref __context r =
  let t = Context.database_of __context in
  let module DB = (val (Db_cache.get t) : Db_interface.DB_ACCESS) in
  DB.is_valid_ref t (Ref.string_of r)

module LazyMemo : sig
  type 'a t

  (** [memo f v] constructs an ['a LazyMemo.t] lazy value for [f v] that is only evaluated the first time it is used.
  *)
  val memo : ('a -> 'b) -> 'a -> 'b t

  (** [memoU f] constructs an ['a LazyMemo.t] equivalent to [memo f ()] *)
  val memoU : (unit -> 'a) -> 'a t

  (** [!! v] forces the evaluation of the lazy value [v].
      If the evaluation succeeds the result is memoized and [f v] is not called again for the liftime of [v].
  *)
  val (!!) : 'a t -> 'a

  (** [f $ v] constructs a new lazy value that will call [f !!v] when evaluated,
      i.e. this is a shorter syntax for [memo f !!v] *)
  val ($!) : ('a -> 'b) -> 'a t -> 'b t
end = struct
  type 'a t = 'a Records.lzy ref

(* Lazy.t is not thread-safe (it can raise Lazy.undefined), so use a ref instead.
   We only want to initiate the (potentially RPC) call when we know we are
   going to need the result, and then memoize it for the lifetime of this value.
   Useful for example in update_allowed_operations where sometimes you need the
   same value multiple times, but in assert_valid you only need some values for
   the current operation.
*)
  let memo f v = ref (Records.ToGet (fun () -> f v))

  let memoU f = ref (Records.ToGet f)

  let (!!) = Records.lzy_get

  let ($!) f v = memo f !!v
end
