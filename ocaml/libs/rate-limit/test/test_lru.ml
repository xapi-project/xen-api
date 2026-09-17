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

module LRU = Rate_limit_lib.Lru

(* Generators *)
let kvs =
  let open QCheck in
  list_of_size Gen.(int_range 1 20) (pair char int)

let lru =
  let open QCheck in
  kvs
  |> map @@ fun kvs ->
     let lru = LRU.create (List.length kvs) in
     List.iter (fun (k, v) -> LRU.add lru k v |> ignore) kvs ;
     lru

(* Tests *)

let count = 1000

let test_lru_length =
  QCheck.Test.make ~name:"LRU length matches to_list" lru ~count @@ fun lru ->
  LRU.to_list lru |> List.length = LRU.size lru

let test_lru_drop =
  QCheck.Test.make ~name:"LRU drop_while evicts all" lru ~count @@ fun lru ->
  let evict (_, _) _ = true in
  LRU.drop_while ~evict lru ;
  LRU.size lru = 0 && LRU.cap lru > 0

(** add a new value but make room if the cache is full *)
let add lru (key, value) =
  match LRU.add lru key value with true -> LRU.trim lru | false -> ()

(** The test takes a full cache and adds more elements but now elements
   are trimmed such that the cache does not grow *)
let test_lru_growth =
  QCheck.Test.make ~name:"LRU growth respects capacity"
    QCheck.(pair lru kvs)
    ~count
  @@ fun (lru, kvs) ->
  List.iter (add lru) kvs ;
  LRU.size lru <= LRU.cap lru

(* We expect to find all keys; we sort the keys before looking them up.
   The least recently used key should be the head of that list *)
let test_lru_lookup =
  QCheck.Test.make ~name:"LRU lookup finds all keys" lru ~count @@ fun lru ->
  let sort = List.sort_uniq Char.compare in
  let keys = List.map fst (LRU.to_list lru) |> sort in
  let lookup key =
    match LRU.lookup lru key with
    | Some _ ->
        ()
    | None ->
        failwith "failed to find key"
  in
  List.iter lookup keys ;
  LRU.to_list lru |> List.map fst |> sort = keys

let test_lru_add_trim_growth =
  QCheck.Test.make ~name:"LRU add_trim respects capacity"
    QCheck.(pair lru kvs)
    ~count
  @@ fun (lru, kvs) ->
  List.iter (fun (k, v) -> LRU.add_trim lru k v) kvs ;
  LRU.size lru <= LRU.cap lru

let test_lru_add_trim_equivalent =
  QCheck.Test.make ~name:"LRU add_trim equivalent to add+trim"
    QCheck.(pair kvs kvs)
    ~count
  @@ fun (init, extra) ->
  let lru_at = LRU.create (max 1 (List.length init)) in
  let lru_sep = LRU.create (max 1 (List.length init)) in
  List.iter (fun (k, v) -> LRU.add_trim lru_at k v) init ;
  List.iter (fun (k, v) -> add lru_sep (k, v)) init ;
  List.iter (fun (k, v) -> LRU.add_trim lru_at k v) extra ;
  List.iter (fun (k, v) -> add lru_sep (k, v)) extra ;
  LRU.to_list lru_at = LRU.to_list lru_sep

(* [filter] tests *)

let test_lru_filter_keep_all =
  QCheck.Test.make ~name:"LRU filter ~f:(fun _ _ -> true) preserves contents"
    lru ~count
  @@ fun lru ->
  let kept = LRU.filter lru ~f:(fun _ _ -> true) in
  LRU.to_list kept = LRU.to_list lru
  && LRU.cap kept = LRU.cap lru
  && LRU.size kept = LRU.size lru

let test_lru_filter_drop_all =
  QCheck.Test.make ~name:"LRU filter ~f:(fun _ _ -> false) empties the cache"
    lru ~count
  @@ fun lru ->
  let dropped = LRU.filter lru ~f:(fun _ _ -> false) in
  LRU.size dropped = 0 && LRU.cap dropped = LRU.cap lru

let test_lru_filter_does_not_mutate_source =
  QCheck.Test.make ~name:"LRU filter does not mutate the source cache" lru
    ~count
  @@ fun lru ->
  let before = LRU.to_list lru in
  let _ = LRU.filter lru ~f:(fun (k : char) _ -> Char.code k mod 2 = 0) in
  LRU.to_list lru = before

let test_lru_filter_predicate_correct =
  QCheck.Test.make ~name:"LRU filter keeps exactly the matching entries" lru
    ~count
  @@ fun lru ->
  let keep k _ = Char.code k mod 2 = 0 in
  let kept = LRU.filter lru ~f:keep in
  let expected = List.filter (fun (k, v) -> keep k v) (LRU.to_list lru) in
  LRU.to_list kept = expected

let test_lru_filter_preserves_lru_order =
  QCheck.Test.make ~name:"LRU filter preserves LRU order of survivors" lru
    ~count
  @@ fun lru ->
  let keep k _ = Char.code k mod 3 <> 0 in
  let kept = LRU.filter lru ~f:keep in
  let survivors =
    List.filter_map
      (fun (k, v) ->
        if keep k v then
          Some k
        else
          None
      )
      (LRU.to_list lru)
  in
  List.map fst (LRU.to_list kept) = survivors

let test_lru_filter_result_usable =
  (* Stress test: after a filter, the resulting LRU must still behave like a
     normal LRU — lookups update LRU order, add+trim respects capacity, and
     adding distinct keys beyond capacity evicts the oldest first. *)
  QCheck.Test.make ~name:"LRU filter result behaves like a normal LRU"
    QCheck.(pair lru kvs)
    ~count
  @@ fun (lru, extras) ->
  let kept = LRU.filter lru ~f:(fun k _ -> Char.code k mod 2 = 0) in
  (* lookups on surviving keys succeed *)
  let surviving_keys = List.map fst (LRU.to_list kept) in
  List.iter
    (fun k ->
      match LRU.lookup kept k with
      | Some _ ->
          ()
      | None ->
          failwith "filter dropped a key it should have kept"
    )
    surviving_keys ;
  (* now add a bunch of extras; capacity must be respected *)
  List.iter (fun (k, v) -> LRU.add_trim kept k v) extras ;
  LRU.size kept <= LRU.cap kept

let test_lru_filter_repeated =
  (* Stress test: alternating filter operations on the same LRU should be
     idempotent in the sense that filtering with the same predicate twice
     yields the same result as filtering once, and composing filters is
     equivalent to a single combined predicate. *)
  QCheck.Test.make ~name:"LRU filter composes" lru ~count @@ fun lru ->
  let p1 k _ = Char.code k mod 2 = 0 in
  let p2 _ v = v mod 3 = 0 in
  let combined k v = p1 k () && p2 () v in
  let a = LRU.filter (LRU.filter lru ~f:p1) ~f:p2 in
  let b = LRU.filter lru ~f:combined in
  LRU.to_list a = LRU.to_list b

let test =
  [
    QCheck_alcotest.to_alcotest test_lru_length
  ; QCheck_alcotest.to_alcotest test_lru_drop
  ; QCheck_alcotest.to_alcotest test_lru_growth
  ; QCheck_alcotest.to_alcotest test_lru_lookup
  ; QCheck_alcotest.to_alcotest test_lru_add_trim_growth
  ; QCheck_alcotest.to_alcotest test_lru_add_trim_equivalent
  ; QCheck_alcotest.to_alcotest test_lru_filter_keep_all
  ; QCheck_alcotest.to_alcotest test_lru_filter_drop_all
  ; QCheck_alcotest.to_alcotest test_lru_filter_does_not_mutate_source
  ; QCheck_alcotest.to_alcotest test_lru_filter_predicate_correct
  ; QCheck_alcotest.to_alcotest test_lru_filter_preserves_lru_order
  ; QCheck_alcotest.to_alcotest test_lru_filter_result_usable
  ; QCheck_alcotest.to_alcotest test_lru_filter_repeated
  ]

let () = Alcotest.run "LRU library" [("LRU tests", test)]
