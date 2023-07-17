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

module LRU = Lru
module LL = Lru.LL
module Q = QCheck
module QG = QCheck.Gen
module QT = QCheck.Test

(* Generators *)
let chars =
  let open Q in
  list_of_size Gen.(int_range 0 50) char

let ints =
  let open Q in
  list_of_size Gen.(int_range 0 50) int

let kvs =
  let open Q in
  list_of_size Gen.(int_range 1 20) (pair char int)

let lru =
  let open Q in
  kvs
  |> map @@ fun kvs ->
     let lru = LRU.create (List.length kvs) in
     List.iter (fun (k, v) -> LRU.add lru k v |> ignore) kvs ;
     lru

(* Tests *)

let count = 1000

let ll_1 : QT.t =
  QT.make ~name:__FUNCTION__ chars ~count @@ fun chars ->
  let t = LL.from_list chars in
  LL.to_list t = chars

let ll_2 : QT.t =
  QT.make ~name:__FUNCTION__ chars ~count @@ fun chars ->
  let open LL in
  let t = from_list chars in
  let x = node 'x' in
  let y = node 'y' in
  let z = node 'z' in
  List.iter (append t) [x; y; z] ;
  assert (match last t with Some z -> value z = 'z' | None -> false) ;
  List.iter (drop t) [y; z; x] ;
  to_list t = chars

let ll_3 : QT.t =
  QT.make ~name:__FUNCTION__ ints ~count @@ fun ints ->
  let total = List.fold_left ( + ) 0 ints in
  let open LL in
  let t = from_list ints in
  List.for_all (( = ) total) [foldl ( + ) 0 t; foldr ( + ) t 0]

let length : QT.t =
  QT.make ~name:__FUNCTION__ lru ~count @@ fun lru ->
  LRU.to_list lru |> List.length = LRU.size lru

let drop : QT.t =
  QT.make ~name:__FUNCTION__ lru ~count @@ fun lru ->
  let evict (_, _) _ = true in
  LRU.drop_while ~evict lru ;
  LRU.size lru = 0 && LRU.cap lru > 0

(** add a new value but make room if the cache is full *)
let add lru (key, value) =
  match LRU.add lru key value with true -> LRU.trim lru | false -> ()

(** The test takes a full cache and adds more elements but now elements
   are trimmed such that the cache does not grow *)
let growth : QT.t =
  QT.make ~name:__FUNCTION__ Q.(pair lru kvs) ~count @@ fun (lru, kvs) ->
  List.iter (add lru) kvs ;
  LRU.size lru <= LRU.cap lru

(* We expect to find all keys; we sort the keys before looking them up.
   The least recently used key should be the head of that list *)
let lookup : QT.t =
  QT.make ~name:__FUNCTION__ lru ~count @@ fun lru ->
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

(* Execute tests - "make test" *)

let suite = [length; drop; growth; lookup; ll_1; ll_2; ll_3]

let () =
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  exit errcode
