(*
   We are maintaining a pool of stunnel connections. An existing
   connection can be either retrieved from the pool or created afresh,
   used and put into the pool. When a connection from the pool it used,
   it is removed from the pool (to avoid it being used concurrently). So
   all connections in the pool should be idle. Somewhat confusingly, the
   pool is called a cache.
*)

module LRU = Lru

module D = Debug.Make (struct let name = __MODULE__ end)

open D

(** key into cache *)
type key = {
    host: string
  ; port: int
  ; verified: Stunnel.verification_config option
}

type value = {stunnel: Stunnel.t; created: float  (** Unix timestamp *)}

(** Criteria we use to expire connections from the cache *)
module Limit = struct
  (** cache entries *)
  let capacity = 70

  (** seconds - time since stunnel was first created *)
  let age = 180. *. 60.

  (** seconds - time since added to cache *)
  let idle = 5. *. 60.
end

(** We [evict] any entry exceeding cache capacity as well any entry
    exceeding age or idle time. *)
let evict (key, value) over_capacity =
  let now = Unix.gettimeofday () in
  let idle = now -. value.created in
  let age = now -. value.stunnel.Stunnel.connected_time in
  match over_capacity || idle > Limit.idle || age > Limit.age with
  | true ->
      debug "%s: evicting %s:%d from cache" __FUNCTION__ key.host key.port ;
      Stunnel.disconnect value.stunnel ;
      true (* signals to remove entry *)
  | false ->
      false (* signals to keep entry *)

(** global stunnel cache *)
let cache : (key, value) LRU.t = LRU.create Limit.capacity

(** drop entries until we are no longer exceeding capacity *)
let trim cache = LRU.drop_while cache ~evict

(** drop entries until we are no longer exceeding capacity *)
let gc () = trim cache

(** remove all entries from the cache; closing connections *)
let flush () =
  let evict (k, v) _ =
    debug "%s: evicting %s:%d from cache" __FUNCTION__ k.host k.port ;
    Stunnel.disconnect v.stunnel ;
    true
  in
  LRU.drop_while cache ~evict

(** add an stunnel to the cache *)
let add (stunnel : Stunnel.t) =
  debug "%s: caching %s:%d" __FUNCTION__ stunnel.host stunnel.port ;
  let value = {stunnel; created= Unix.gettimeofday ()} in
  let key =
    {host= stunnel.host; port= stunnel.port; verified= stunnel.verified}
  in
  ( match LRU.lookup cache key with
  | Some v ->
      warn "%s: %s:%d already cached - removing old entry first" __FUNCTION__
        key.host key.port ;
      Stunnel.disconnect v.stunnel ;
      LRU.remove cache key
  | None ->
      ()
  ) ;
  match LRU.add cache key value with true -> trim cache | false -> ()

let with_remove ~host ~port verified f =
  let key = {host; port; verified} in
  trim cache ;
  (* retrieve removes the entry from the cache if found *)
  LRU.retrieve cache key |> Option.map (fun v -> f v.stunnel)

let with_connect ?use_fork_exec_helper ?write_to_log ~verify_cert ~host ~port f
    =
  match with_remove ~host ~port verify_cert f with
  | Some result ->
      result
  | None ->
      info "%s: did not find %s:%d in cache" __FUNCTION__ host port ;
      Stunnel.with_connect ?use_fork_exec_helper ?write_to_log ~verify_cert host
        port f
(*
 * Copyright (C) 2023 Cloud Software Group
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
