(* A safe implementation of the KV storage interface entirely in-memory.
   This is useful for checking that the property test code passes with a correct implementation.
*)

open Xapi_blobstore_core

module IO = struct type 'a t = 'a end

module Key = Bounded_string.Make (struct let max_length = 1024 end)

module Value = Bounded_string.Make (struct let max_length = 128 * 1024 end)

type config = unit

let max_key_count = 128

let max_data_size = 256 * 1024

type t = {m: Mutex.t; tbl: (Key.t, Value.t) Hashtbl.t; mutable size: int}

let name = __MODULE__

let pp_config _ () = ()

let connect () = {m= Mutex.create (); tbl= Hashtbl.create 7; size= 0}

let disconnect _ = ()

let with_mutex t f =
  Mutex.lock t.m ;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.m) (fun () -> f t.tbl)

let get t k = with_mutex t @@ fun tbl -> Hashtbl.find_opt tbl k

let kv_length k v =
  String.length (Key.to_string k) + String.length (Value.to_string v)

let put t k v =
  with_mutex t @@ fun tbl ->
  let old = Hashtbl.find_opt tbl k in
  if Option.is_none old && Hashtbl.length tbl >= max_key_count then
    Fmt.invalid_arg "Too many keys" ;
  let delta = kv_length k v in
  let old_size = Option.fold ~none:0 ~some:(kv_length k) old in
  let next = t.size + delta - old_size in
  if next > max_data_size then
    Fmt.invalid_arg "max_data_size exceeded: %d + %d > %d" t.size delta
      max_data_size ;
  Hashtbl.replace tbl k v ;
  t.size <- next

let delete t k =
  with_mutex t @@ fun tbl ->
  let old = Hashtbl.find_opt tbl k in
  Hashtbl.remove tbl k ;
  let old_size = old |> Option.fold ~none:0 ~some:(kv_length k) in
  t.size <- t.size - old_size

let list t =
  with_mutex t @@ fun tbl -> tbl |> Hashtbl.to_seq_keys |> List.of_seq
