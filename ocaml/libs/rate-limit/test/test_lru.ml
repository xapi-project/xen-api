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

open Rate_limit
module LRU = Lru
module LL = Lru.LL

(* Generators *)
let chars =
  let open QCheck in
  list_of_size Gen.(int_range 0 50) char

let ints =
  let open QCheck in
  list_of_size Gen.(int_range 0 50) int

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

let test_ll_from_to_list =
  QCheck.Test.make ~name:"LL from_list/to_list roundtrip" chars ~count
  @@ fun chars ->
  let t = LL.from_list chars in
  LL.to_list t = chars

let test_ll_append_drop =
  QCheck.Test.make ~name:"LL append and drop" chars ~count @@ fun chars ->
  let open LL in
  let t = from_list chars in
  let x = node 'x' in
  let y = node 'y' in
  let z = node 'z' in
  List.iter (append t) [x; y; z] ;
  assert (match last t with Some z -> value z = 'z' | None -> false) ;
  List.iter (drop t) [y; z; x] ;
  to_list t = chars

let test_ll_fold =
  QCheck.Test.make ~name:"LL foldl/foldr consistency" ints ~count @@ fun ints ->
  let total = List.fold_left ( + ) 0 ints in
  let open LL in
  let t = from_list ints in
  List.for_all (( = ) total) [foldl ( + ) 0 t; foldr ( + ) t 0]

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

let test =
  [
    QCheck_alcotest.to_alcotest test_ll_from_to_list
  ; QCheck_alcotest.to_alcotest test_ll_append_drop
  ; QCheck_alcotest.to_alcotest test_ll_fold
  ; QCheck_alcotest.to_alcotest test_lru_length
  ; QCheck_alcotest.to_alcotest test_lru_drop
  ; QCheck_alcotest.to_alcotest test_lru_growth
  ; QCheck_alcotest.to_alcotest test_lru_lookup
  ]

let () = Alcotest.run "LRU library" [("LRU tests", test)]
