(*
 * This module implements a cache with support for a least-recently-used
 * (LRU) replacement policy. Main features:
 *
 * Implemented with standard OCaml data types, no dependency on outside
 * libararies for the main functionality.
 *
 * Implemented with mutable state. This keeps the implementation compact
 * and efficient but requires thinking about state more.
 *
 * The architecture is: elements are kept in a hash table to look them
 * up based on a key. Additionally they are kept in a doubly linked
 * list. The head of the list is the least recently used element that
 * can be dropped to make room in the cache. When an element is found in
 * the cache it is moved to the tail of the linked list.
 *)

let invalid_arg fmt = Printf.ksprintf invalid_arg fmt

let _fail fmt = Printf.ksprintf failwith fmt

module LL : sig
  (** Doubly linked list ['a t] holding elements of type ['a]. *)

  (** doubly linked list; this is a cyclic data structure; don't use [=]
      on it as it may not terminate. *)
  type 'a t

  (** a node in the list. A node can be removed from its list. Don't use
      [=] on [node] values as it may not terminate. *)
  type 'a node

  val create : unit -> 'a t
  (** create an empty list *)

  val node : 'a -> 'a node
  (** create a node to carry a value *)

  val value : 'a node -> 'a
  (** obtain the value from a node *)

  val append : 'a t -> 'a node -> unit
  (** append a node at the end *)

  val drop : 'a t -> 'a node -> unit
  (** [drop t n] a node [n] from list [t]. It is an unchecked error to
      pass a node [n] to [drop] that is not an element of [t] to begin
      with.*)

  val first : 'a t -> 'a node option
  (** first/head node of the list *)

  val last : 'a t -> 'a node option
  (** last/tail node of the list *)

  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** fold from head *)

  val foldr : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** fold from tail *)

  val to_list : 'a t -> 'a list
  (** retrieve all elements from the list *)

  val from_list : 'a list -> 'a t
  (** construct a [t] value from list *)
end = struct
  type 'a node = {
      value: 'a
    ; mutable prev: 'a node option
    ; mutable next: 'a node option
  }

  type 'a t = {mutable first: 'a node option; mutable last: 'a node option}

  let create () = {first= None; last= None}

  let node x = {value= x; prev= None; next= None}

  let append t n =
    match t.last with
    | None ->
        let node = Some n in
        t.first <- node ;
        t.last <- node
    | Some lst ->
        let node = Some n in
        lst.next <- node ;
        n.prev <- t.last ;
        t.last <- node

  (** [drop] a node [n] from (its) list [t]. The interesting property is
      that we can drop any element from its list that we know. However,
      we don't check that [n] is indeed a member of [t] and it's an
      unchecked error to pass an [n] that is not a member of [t].

      This is similar to a
      pointer-based implementation in C. We infer that we need to update
      the fist, last entry of the list of [n]'s prev or next is [None],
      hence it is the first or last element in the list. *)
  let drop t n =
    let np = n.prev in
    let nn = n.next in
    ( match np with
    | None ->
        t.first <- nn
    | Some x ->
        x.next <- nn ;
        n.prev <- None
    ) ;
    match nn with
    | None ->
        t.last <- np
    | Some x ->
        x.prev <- np ;
        n.next <- None

  let first t = t.first

  let last t = t.last

  let value node = node.value

  let foldl f zero t =
    let rec loop acc = function
      | None ->
          acc
      | Some n ->
          loop (f acc n.value) n.next
    in
    loop zero t.first

  let foldr f t zero =
    let rec loop acc = function
      | None ->
          acc
      | Some n ->
          loop (f n.value acc) n.prev
    in
    loop zero t.last

  let to_list t = foldr (fun x xs -> x :: xs) t []

  let from_list xs =
    let t = create () in
    List.iter (fun x -> append t (node x)) xs ;
    t
end

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
