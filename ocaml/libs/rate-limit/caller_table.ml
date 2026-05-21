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

module D = Debug.Make (struct let name = "caller_table" end)

module Key = struct
  (* Prefix "" is a wildcard *)
  type match_pattern = Full of string | Prefix of string

  type t = {user_agent: string; client_ip: string}

  type pattern_key = {
      user_agent_pattern: match_pattern
    ; client_ip_pattern: match_pattern
  }

  let matches_pattern ~pattern ~target =
    match pattern with
    | Full s ->
        target = s
    | Prefix prefix ->
        String.starts_with ~prefix target

  let matches_key ~pattern ~target =
    matches_pattern ~pattern:pattern.user_agent_pattern
      ~target:target.user_agent
    && matches_pattern ~pattern:pattern.client_ip_pattern
         ~target:target.client_ip

  let equal_pattern a b =
    a.user_agent_pattern = b.user_agent_pattern
    && a.client_ip_pattern = b.client_ip_pattern

  let wildcard_score = function Full _ -> 0 | Prefix "" -> 2 | Prefix _ -> 1

  let compare_wildcard k =
    let user_agent_score = wildcard_score k.user_agent_pattern in
    let client_ip_score = wildcard_score k.client_ip_pattern in
    (user_agent_score + client_ip_score, user_agent_score, client_ip_score)

  let is_all_wildcard k =
    k.user_agent_pattern = Prefix "" && k.client_ip_pattern = Prefix ""

  (** Total order: fewer wildcards first, then lexicographic by patterns *)
  let compare a b =
    match compare (compare_wildcard a) (compare_wildcard b) with
    | 0 -> (
      match compare a.user_agent_pattern b.user_agent_pattern with
      | 0 ->
          compare a.client_ip_pattern b.client_ip_pattern
      | n ->
          n
    )
    | n ->
        n
end

type 'a cached_table = {
    table: (Key.pattern_key * 'a) list
  ; cache: (Key.t, 'a list) Lru.t
}

type 'a t = 'a cached_table Atomic.t

let cache_capacity = 100

let create () = Atomic.make {table= []; cache= Lru.create cache_capacity}

(** Build a fresh cache from [old_cache] but drop entries whose cached
    target is matched by [pattern]. Those are exactly the entries whose
    result list would change if [pattern] is inserted into or removed
    from the table. *)
let cache_without_matches ~pattern old_cache =
  Lru.filter old_cache ~f:(fun target _ ->
      not (Key.matches_key ~pattern ~target)
  )

(** Insert [entry] into [table] (sorted by Key.compare ascending, i.e. most
    specific first) at the position that preserves the ordering. *)
let rec insert_sorted entry table =
  match table with
  | [] ->
      [entry]
  | (k, _) :: _ when Key.compare (fst entry) k <= 0 ->
      entry :: table
  | hd :: tl ->
      hd :: insert_sorted entry tl

(** Find all matching entries for a caller_id, ordered by Key.compare
    (most specific first). Priority: exact > prefix > full wildcard. *)
let find_matches {table; cache} ~caller_id =
  let entry_opt = Lru.lookup cache caller_id in
  match entry_opt with
  | Some result ->
      result
  | None ->
      let result =
        List.filter_map
          (fun (key, v) ->
            if Key.matches_key ~pattern:key ~target:caller_id then
              Some v
            else
              None
          )
          table
      in
      Lru.add_trim cache caller_id result ;
      result

let mem t ~caller_id =
  let entries = Atomic.get t in
  find_matches entries ~caller_id <> []

let insert t ~pattern data =
  if Key.is_all_wildcard pattern then
    false
  (* Reject keys with both fields full wildcards. *)
  else
    let {table; cache} = Atomic.get t in
    if List.exists (fun (key, _) -> Key.equal_pattern key pattern) table then
      false
    else (
      Atomic.set t
        {
          table= insert_sorted (pattern, data) table
        ; cache= cache_without_matches ~pattern cache
        } ;
      true
    )

let delete t ~pattern =
  let {table; cache} = Atomic.get t in
  match List.find_opt (fun (key, _) -> Key.equal_pattern key pattern) table with
  | None ->
      ()
  | Some _ ->
      Atomic.set t
        {
          table=
            List.filter
              (fun (key, _) -> not (Key.equal_pattern key pattern))
              table
        ; cache= cache_without_matches ~pattern cache
        }

let get t ~caller_id =
  let entries = Atomic.get t in
  find_matches entries ~caller_id

let get_exact t ~pattern =
  let {table; _} = Atomic.get t in
  Option.map snd
    (List.find_opt (fun (key, _) -> Key.equal_pattern key pattern) table)
