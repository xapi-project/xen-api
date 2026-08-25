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

let invalid_arg fmt = Printf.ksprintf invalid_arg fmt

module LL = Linked_list

(** A store for key/value pairs of type ['k] and ['v]. The main store
      is [table] that maps a key to a key/value node. Every item in the
      store is also a member in the [queue]. The [queue] keeps track of
      which elements are looked up most often. The first element in the
      [queue] is the least used one. *)
type ('k, 'v) t = {
    table: ('k, ('k * 'v) LL.node) Hashtbl.t
  ; queue: ('k * 'v) LL.t
  ; cap: int  (** max capacity of table and queue *)
  ; mutable entries: int  (** actual capacity of table and queue *)
  ; lock: Mutex.t  (** lock while operating on this value *)
}

let locked m f =
  let finally () = Mutex.unlock m in
  Mutex.lock m ; Fun.protect ~finally f

(* All primed functions below are not thread safe because they are
   manipulating state; we will use a lock to protect against concurrent
   update. However, we have to do that on an outer layer such that we
   can use these functions internally after we obtained the lock. *)

module Unsafe = struct
  let create' capacity =
    if capacity <= 0 then
      invalid_arg "%s: capacity needs to be postive" __FUNCTION__ ;
    {
      table= Hashtbl.create capacity
    ; queue= LL.create ()
    ; cap= capacity
    ; entries= 0
    ; lock= Mutex.create ()
    }

  let size' t = t.entries

  let cap' t = t.cap

  let to_list' t = LL.to_list t.queue

  (** [lookup] an entry based on its [key]; this may fail or succeeed.
      In the success case, the entry is moved to the tail of the
      [queue]. Hnece, the least-used entry is at the front. *)
  let lookup' t key =
    match Hashtbl.find_opt t.table key with
    | Some v ->
        LL.drop t.queue v ;
        LL.append t.queue v ;
        Some (LL.value v |> snd)
    | None ->
        None

  (** [remove] an entry based on this [key] *)
  let remove' t key =
    match Hashtbl.find_opt t.table key with
    | Some v ->
        LL.drop t.queue v ;
        Hashtbl.remove t.table key ;
        t.entries <- t.entries - 1
    | None ->
        ()

  (** [add] a new entry; do nothing if the entry exists. If the new
      entry exceeds the capacity of the table, [true]
      is returned and [false] otherwise. It signals the caller to [trim]
      the table.*)
  let add' t key value =
    match lookup' t key with
    | None ->
        let node = LL.node (key, value) in
        Hashtbl.add t.table key node ;
        t.entries <- t.entries + 1 ;
        LL.append t.queue node ;
        t.entries > t.cap
    | Some _ ->
        t.entries > t.cap

  (** [lru] returns the least-recently-used key/value pair *)
  let lru' t = LL.first t.queue |> Option.map LL.value

  (** [drop_while] drops elements starting in least-recently-used order
    while predicate [evict] is true. The predicate receives the key/value
    and a boolean that indicates if the cache is over capacity. If
    [evict] returns true it can perform any finalisation on the value
    before it will be removed by [drop_while]. *)

  let rec drop_while' t ~evict =
    match lru' t with
    | Some ((key, _) as kv) when evict kv (t.entries > t.cap) ->
        remove' t key ; drop_while' t ~evict
    | Some _ ->
        ()
    | None ->
        ()

  (** [trim] the table such that it does not exceed its capacity by
    removing the least-used element repeatedly until this is achieved.
    If finalisation of values is required, use [drop_while] because
    [trim] does not provide it.  *)

  let trim' t =
    let evict _ x = x in
    drop_while' t ~evict

  let add_trim' t key value =
    ignore (add' t key value) ;
    trim' t
end

(* Functions below are intended to be used by clients of this modules.
   They have to take care of locking. *)

let create = Unsafe.create'

let size t = locked t.lock @@ fun () -> Unsafe.size' t

let cap t = locked t.lock @@ fun () -> Unsafe.cap' t

let to_list t = locked t.lock @@ fun () -> Unsafe.to_list' t

let lookup t = locked t.lock @@ fun () -> Unsafe.lookup' t

let remove t = locked t.lock @@ fun () -> Unsafe.remove' t

let add t = locked t.lock @@ fun () -> Unsafe.add' t

let drop_while t = locked t.lock @@ fun () -> Unsafe.drop_while' t

let trim t = locked t.lock @@ fun () -> Unsafe.trim' t

let add_trim t key value =
  locked t.lock @@ fun () -> Unsafe.add_trim' t key value

let filter t ~f =
  locked t.lock @@ fun () ->
  let fresh = Unsafe.create' t.cap in
  List.iter
    (fun (k, v) -> if f k v then Unsafe.add_trim' fresh k v)
    (Unsafe.to_list' t) ;
  fresh
