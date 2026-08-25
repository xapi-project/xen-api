(*
 * Copyright (C) 2026 Cloud Software Group
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

type caller_statistics = {call_count: int; token_count: float}

type t = {caller_uuid: string; statistics: caller_statistics Atomic.t}

let create ~caller_uuid =
  {caller_uuid; statistics= Atomic.make {call_count= 0; token_count= 0.0}}

(* Recursion should only trigger rarely when under contention *)
let rec register_call ~token_amount ({statistics; _} as t) =
  let ({call_count; token_count} as vl) = Atomic.get statistics in
  if
    not
      (Atomic.compare_and_set statistics vl
         {call_count= call_count + 1; token_count= token_count +. token_amount}
      )
  then
    register_call ~token_amount t

let get_uuid {caller_uuid; _} = caller_uuid

let get_call_count {statistics; _} = (Atomic.get statistics).call_count

let get_token_count {statistics; _} = (Atomic.get statistics).token_count
