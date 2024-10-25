(*
 * Copyright (C) Cloud Software Group
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

module User = String

module Secret = struct
  type key = string

  type digest = string

  type salt = string

  type secret = string

  type t = digest * salt * secret

  let create digest salt secret = (digest, salt, secret)

  let read t = t

  let create_salt () = Int32.(to_string (Random.int32 max_int))

  let hash key salt =
    match Pam.(crypt ~algo:SHA512 ~key ~salt) with
    | Ok hash ->
        hash
    | _ ->
        failwith ("Failed to compute hash in " ^ __MODULE__)

  let equal_digest = ( = )
end

module Cache = Helpers.AuthenticationCache.Make (User) (Secret)

let insert cache (user, password, session) =
  Cache.cache cache user password session

let is_cached cache (user, password, session) =
  match Cache.cached cache user password with
  | Some session' when session = session' ->
      true
  | _ ->
      false

module CS = Set.Make (struct
  type t = string * string * string

  let compare (u, _, _) (u', _, _) = compare u u'
end)

let credentials =
  [
    ("foo", "password", "session123")
  ; ("bar", "pass123", "token321")
  ; ("baz", "s3cr3t", "_sessid_")
  ; ("user", "_password_", "sessi0n")
  ]
  |> CS.of_list

(* Simple test to demonstrate that the combination of hash function and salt
   above is sufficient to avoid similar passwords from granting
   access. It tests that every prefix of a password - whose length is
   less than the original password - does not permit access to the
   cached result. In practice, the validity of this test is up to the
   choice of hash function: weak hash functions with many collisions
   may permit this.
*)
let test_cache_similar_passwords () =
  let user = "user" in
  let password = "passwordpasswordpassword" in
  let cache = Cache.create ~size:1 ~ttl:Mtime.Span.(10 * s) in
  insert cache (user, password, "session") ;
  for len = String.length password - 1 downto 0 do
    let password' = String.sub password 0 len in
    if Option.is_some (Cache.cached cache user password') then
      failwith "Similar password granted access to cached secret"
  done

(* This test caches some credentials with a short time-to-live (expiry)
   and checks that the values are no longer cached after their
   expiration time. *)
let test_cache_expiration () =
  let expiry_seconds = 2 in
  let ttl = Mtime.Span.(expiry_seconds * s) in
  let cache = Cache.create ~size:100 ~ttl in
  (* Cache all the credentials. *)
  CS.iter (insert cache) credentials ;
  (* Immediately check that all the values are cached. *)
  if not (CS.for_all (is_cached cache) credentials) then
    failwith "Cache expired too early or corrupted the stored value" ;
  (* Delay for at least as long as the time-to-live, then check again. *)
  Thread.delay (float_of_int expiry_seconds) ;
  if CS.exists (is_cached cache) credentials then
    failwith "Cache kept an entry after its expiry time"

(* This test exercises the property that the cache permits you to
   insert a fresh credential for a user even if an entry is already
   present for that user. It is currently the responsibility of the
   user of the cache to guard against this. The side effect of
   permitting this is that it allows you to observably extend the TTL
   of cached entries. *)
let test_cache_updates_duplicates () =
  let expiry_seconds = 1 in
  let ttl = Mtime.Span.(expiry_seconds * s) in
  let count = CS.cardinal credentials in
  let cache = Cache.create ~size:count ~ttl in
  let credentials = CS.to_seq credentials in
  Seq.iter (insert cache) credentials ;
  let is_even i = i mod 2 = 0 in
  (* Elements occurring at even indices will have their TTLs extended. *)
  Seq.iteri (fun i c -> if is_even i then insert cache c) credentials ;
  (* Delay for at least as long as the original TTL. *)
  Thread.delay (float_of_int expiry_seconds) ;
  (* Check that any credential still cached occurs at an even index. *)
  let check i c =
    if is_cached cache c && not (is_even i) then
      failwith "Entry should have expired."
  in
  Seq.iteri check credentials

(* Test to ensure eviction works when cache reaches capacity.
   A single test case credential is a 3-tuple (user, password, session).
   Let c_1, c_2, ..., c_N denote a sequence of N distinct credentials.
   This test creates a cache of fixed capacity N and then inserts each
   c_i in c_1, c_2, ..., c_N.
   Then, for each c_i in the same sequence, a fresh credential, c'_i,
   distinct from all previous credentials, is derived from c_i.
   The test checks that insertion of c'_i displaces c_i in the cache.
   By the end, the cache must have iteratively evicted each previous
   entry and should only contain elements of c'_1, c'_2, ..., c'_N. *)
let test_cache_eviction () =
  let ttl = Mtime.Span.(30 * s) in
  let count = CS.cardinal credentials in
  let cache = Cache.create ~size:count ~ttl in
  CS.iter (insert cache) credentials ;
  (* Augment each of the credentials *)
  let change = ( ^ ) "_different_" in
  let insert_and_check ((user, password, session) as entry) =
    if not (is_cached cache entry) then
      failwith "Entry should still be cached prior to eviction" ;
    let entry' = (change user, change password, change session) in
    insert cache entry' ;
    (* Check that old entry has been evicted. *)
    if is_cached cache entry then
      failwith "Old entry should have been evicted" ;
    if not (is_cached cache entry') then
      failwith "Failed to cache an entry when at capacity"
  in
  CS.iter insert_and_check credentials

let () =
  let open Alcotest in
  run ~verbose:true "Authentication Cache"
    [
      ( "External Auth Cache"
      , [
          test_case "Password Similarity" `Quick test_cache_similar_passwords
        ; test_case "Entry Expiration" `Quick test_cache_expiration
        ; test_case "Iterative Eviction" `Quick test_cache_eviction
        ; test_case "Permits Updates" `Quick test_cache_updates_duplicates
        ]
      )
    ]
