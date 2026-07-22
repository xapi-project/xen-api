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

module Caller_table = Rate_limit_lib.Caller_table
open Caller_table.Key

(* Build a target (Key.t) from a single string for the user_agent field. *)
let target s = {user_agent= s; client_ip= ""}

(* Build a pattern_key with an exact-match user_agent and empty client_ip. *)
let pat s = {user_agent_pattern= Full s; client_ip_pattern= Full ""}

let test_create () =
  let table = Caller_table.create () in
  Alcotest.(check (list int))
    "Empty table returns [] for get" []
    (Caller_table.get table ~caller_id:{user_agent= "test"; client_ip= ""})

let test_insert () =
  let table = Caller_table.create () in
  let success =
    Caller_table.insert table
      ~pattern:{user_agent_pattern= Full "test"; client_ip_pattern= Full ""}
      42
  in
  Alcotest.(check bool) "Inserting should succeed" true success ;
  Alcotest.(check (list int))
    "Get should return inserted value" [42]
    (Caller_table.get table ~caller_id:{user_agent= "test"; client_ip= ""})

let test_insert_duplicate () =
  let table = Caller_table.create () in
  let success = Caller_table.insert table ~pattern:(pat "agent1") 1 in
  Alcotest.(check bool) "First insert should succeed" true success ;
  let success_dup = Caller_table.insert table ~pattern:(pat "agent1") 2 in
  Alcotest.(check bool) "Inserting duplicate key should fail" false success_dup

let test_delete () =
  let table = Caller_table.create () in
  let _ = Caller_table.insert table ~pattern:(pat "agent1") 42 in
  Alcotest.(check (list int))
    "Entry exists before delete" [42]
    (Caller_table.get table ~caller_id:(target "agent1")) ;
  Caller_table.delete table ~pattern:(pat "agent1") ;
  Alcotest.(check (list int))
    "Entry removed after delete" []
    (Caller_table.get table ~caller_id:(target "agent1"))

let test_delete_nonexistent () =
  let table = Caller_table.create () in
  Caller_table.delete table ~pattern:(pat "nonexistent") ;
  Alcotest.(check pass) "Deleting nonexistent entry should not raise" () ()

let test_get_nonexistent () =
  let table = Caller_table.create () in
  Alcotest.(check (list int))
    "Get nonexistent entry returns []" []
    (Caller_table.get table ~caller_id:(target "nonexistent"))

let test_multiple_entries () =
  let table = Caller_table.create () in
  let _ = Caller_table.insert table ~pattern:(pat "agent1") 10 in
  let _ = Caller_table.insert table ~pattern:(pat "agent2") 20 in
  Alcotest.(check (list int))
    "Agent1 has correct value" [10]
    (Caller_table.get table ~caller_id:(target "agent1")) ;
  Alcotest.(check (list int))
    "Agent2 has correct value" [20]
    (Caller_table.get table ~caller_id:(target "agent2"))

let test_concurrent_insert_delete_stress () =
  (* Stress test: rapidly insert and delete entries. *)
  let table = Caller_table.create () in
  let iterations = 1000 in
  let num_keys = 10 in
  let errors = ref 0 in
  let errors_mutex = Mutex.create () in
  let add_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let k =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              let _ = Caller_table.insert table ~pattern:(pat k) i in
              ()
            done
          )
          ()
    )
  in
  let delete_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let k =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              Caller_table.delete table ~pattern:(pat k)
            done
          )
          ()
    )
  in
  let read_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let k =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              (* This should never crash, even if key doesn't exist *)
              try
                let _ = Caller_table.get table ~caller_id:(target k) in
                ()
              with _ ->
                Mutex.lock errors_mutex ;
                incr errors ;
                Mutex.unlock errors_mutex
            done
          )
          ()
    )
  in
  Array.iter Thread.join add_threads ;
  Array.iter Thread.join delete_threads ;
  Array.iter Thread.join read_threads ;
  Alcotest.(check int) "No errors during concurrent operations" 0 !errors

let test_get_during_delete_race () =
  (* Test that get doesn't crash when entry is being deleted. *)
  let iterations = 500 in
  let errors = ref 0 in
  let errors_mutex = Mutex.create () in
  for _ = 1 to iterations do
    let table = Caller_table.create () in
    let _ = Caller_table.insert table ~pattern:(pat "target") 42 in
    let barrier = ref 0 in
    let barrier_mutex = Mutex.create () in
    let reader =
      Thread.create
        (fun () ->
          Mutex.lock barrier_mutex ;
          incr barrier ;
          Mutex.unlock barrier_mutex ;
          while
            Mutex.lock barrier_mutex ;
            let b = !barrier in
            Mutex.unlock barrier_mutex ; b < 2
          do
            Thread.yield ()
          done ;
          try
            let _ = Caller_table.get table ~caller_id:(target "target") in
            ()
          with _ ->
            Mutex.lock errors_mutex ; incr errors ; Mutex.unlock errors_mutex
        )
        ()
    in
    let deleter =
      Thread.create
        (fun () ->
          Mutex.lock barrier_mutex ;
          incr barrier ;
          Mutex.unlock barrier_mutex ;
          while
            Mutex.lock barrier_mutex ;
            let b = !barrier in
            Mutex.unlock barrier_mutex ; b < 2
          do
            Thread.yield ()
          done ;
          Caller_table.delete table ~pattern:(pat "target")
        )
        ()
    in
    Thread.join reader ; Thread.join deleter
  done ;
  Alcotest.(check int) "No crashes during get/delete race" 0 !errors

(* Wildcard matching tests *)

let test_wildcard_user_agent_matches_any () =
  (* An entry with a full-wildcard user_agent should match any user_agent *)
  let table = Caller_table.create () in
  let pattern =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Full "192.168.1.1"}
  in
  let _ = Caller_table.insert table ~pattern 1 in
  (* Should match any user_agent with same client_ip *)
  let client1 = {user_agent= "curl"; client_ip= "192.168.1.1"} in
  let client2 = {user_agent= "wget"; client_ip= "192.168.1.1"} in
  let client3 = {user_agent= ""; client_ip= "192.168.1.1"} in
  Alcotest.(check bool)
    "wildcard user_agent matches curl" true
    (Caller_table.mem table ~caller_id:client1) ;
  Alcotest.(check bool)
    "wildcard user_agent matches wget" true
    (Caller_table.mem table ~caller_id:client2) ;
  Alcotest.(check bool)
    "wildcard user_agent matches empty" true
    (Caller_table.mem table ~caller_id:client3) ;
  (* Should not match different client_ip *)
  let client_other = {user_agent= "curl"; client_ip= "10.0.0.1"} in
  Alcotest.(check bool)
    "{user_agent=curl, client_ip=10.0.0.1} does not match {user_agent=*, \
     client_ip=192.168.1.1}"
    false
    (Caller_table.mem table ~caller_id:client_other)

let test_wildcard_client_ip_matches_any () =
  (* An entry with a full-wildcard client_ip should match any client_ip *)
  let table = Caller_table.create () in
  let pattern =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Prefix ""}
  in
  let _ = Caller_table.insert table ~pattern 1 in
  (* Should match any client_ip with same user_agent *)
  let client1 = {user_agent= "curl"; client_ip= "192.168.1.1"} in
  let client2 = {user_agent= "curl"; client_ip= "10.0.0.1"} in
  let client3 = {user_agent= "curl"; client_ip= ""} in
  Alcotest.(check bool)
    "wildcard client_ip matches 192.168.1.1" true
    (Caller_table.mem table ~caller_id:client1) ;
  Alcotest.(check bool)
    "wildcard client_ip matches 10.0.0.1" true
    (Caller_table.mem table ~caller_id:client2) ;
  Alcotest.(check bool)
    "wildcard client_ip matches empty" true
    (Caller_table.mem table ~caller_id:client3) ;
  (* Should not match different user_agent *)
  let client_other = {user_agent= "wget"; client_ip= "192.168.1.1"} in
  Alcotest.(check bool)
    "wildcard does not match different user_agent" false
    (Caller_table.mem table ~caller_id:client_other)

let test_wildcard_match_priority_exact_first () =
  (* Exact match should take priority over wildcards *)
  let table = Caller_table.create () in
  let exact =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Full "192.168.1.1"}
  in
  let wildcard_ua =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Full "192.168.1.1"}
  in
  let wildcard_ip =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Prefix ""}
  in
  (* Add in reverse priority order to test sorting *)
  let _ = Caller_table.insert table ~pattern:wildcard_ua 5 in
  let _ = Caller_table.insert table ~pattern:wildcard_ip 15 in
  let _ = Caller_table.insert table ~pattern:exact 10 in
  (* Lookup with exact key should return exact entry (10), not wildcards *)
  let client = {user_agent= "curl"; client_ip= "192.168.1.1"} in
  Alcotest.(check (list int))
    "matches ordered most-specific first: exact, prefix-ip, wildcard-ua"
    [10; 15; 5]
    (Caller_table.get table ~caller_id:client)

let test_wildcard_match_priority_client_ip_over_user_agent () =
  (* client_ip wildcard (user_agent specified) should match before
     user_agent wildcard (client_ip specified) *)
  let table = Caller_table.create () in
  let wildcard_ua =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Full "192.168.1.1"}
  in
  let wildcard_ip =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Prefix ""}
  in
  (* Add user_agent wildcard first *)
  let _ = Caller_table.insert table ~pattern:wildcard_ua 5 in
  (* Add client_ip wildcard second *)
  let _ = Caller_table.insert table ~pattern:wildcard_ip 15 in
  (* Lookup should prefer client_ip wildcard (15) over user_agent wildcard (5) *)
  let client = {user_agent= "curl"; client_ip= "192.168.1.1"} in
  Alcotest.(check (list int))
    "client_ip wildcard ordered before user_agent wildcard" [15; 5]
    (Caller_table.get table ~caller_id:client)

let test_no_spurious_wildcard_matches () =
  (* Ensure wildcards don't match when they shouldn't *)
  let table = Caller_table.create () in
  let pattern1 =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Full "192.168.1.1"}
  in
  let pattern2 =
    {user_agent_pattern= Full "wget"; client_ip_pattern= Prefix ""}
  in
  let _ = Caller_table.insert table ~pattern:pattern1 10 in
  let _ = Caller_table.insert table ~pattern:pattern2 20 in
  (* Client with different user_agent and client_ip should not match pattern1 *)
  let client1 = {user_agent= "curl"; client_ip= "10.0.0.1"} in
  Alcotest.(check bool)
    "{user_agent=curl, client_ip=10.0.0.1} does not match {user_agent=curl, \
     client_ip=192.168.1.1}"
    false
    (Caller_table.mem table ~caller_id:client1) ;
  (* Client with matching user_agent but different client_ip should match pattern2 *)
  let client2 = {user_agent= "wget"; client_ip= "10.0.0.1"} in
  Alcotest.(check (list int))
    "{user_agent=wget, client_ip=10.0.0.1} matches {user_agent=wget, \
     client_ip=*} wildcard"
    [20]
    (Caller_table.get table ~caller_id:client2) ;
  (* Client with no matching pattern *)
  let client3 = {user_agent= "firefox"; client_ip= "172.16.0.1"} in
  Alcotest.(check bool)
    "{user_agent=firefox, client_ip=172.16.0.1} has no match" false
    (Caller_table.mem table ~caller_id:client3)

let test_lru_cache_overflow () =
  (* The internal LRU cache has capacity 100. Insert a wildcard entry and
     perform >100 distinct lookups so the cache overflows, then verify
     lookups still return correct results after evictions. *)
  let table = Caller_table.create () in
  let pattern =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Prefix ""}
  in
  let _ = Caller_table.insert table ~pattern 42 in
  (* Perform 150 distinct lookups to overflow the cache *)
  for i = 1 to 150 do
    let client =
      {user_agent= "curl"; client_ip= Printf.sprintf "10.0.0.%d" i}
    in
    Alcotest.(check (list int))
      (Printf.sprintf "lookup %d returns correct value" i)
      [42]
      (Caller_table.get table ~caller_id:client)
  done ;
  (* Re-check the first few lookups which should have been evicted from
     the cache; they must still resolve correctly via the table scan. *)
  for i = 1 to 10 do
    let client =
      {user_agent= "curl"; client_ip= Printf.sprintf "10.0.0.%d" i}
    in
    Alcotest.(check (list int))
      (Printf.sprintf "re-lookup %d after eviction" i)
      [42]
      (Caller_table.get table ~caller_id:client)
  done ;
  (* Also check that a non-matching lookup still returns None *)
  let miss = {user_agent= "wget"; client_ip= "10.0.0.1"} in
  Alcotest.(check (list int))
    "non-matching lookup after overflow" []
    (Caller_table.get table ~caller_id:miss)

let test_reject_all_wildcard_key () =
  (* Patterns with both fields as full wildcards should be rejected *)
  let table = Caller_table.create () in
  let all_wildcard =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Prefix ""}
  in
  let success = Caller_table.insert table ~pattern:all_wildcard 1 in
  Alcotest.(check bool) "all-wildcard key rejected" false success

let test_prefix_wildcard_matches_prefix () =
  let table = Caller_table.create () in
  let pattern =
    {
      user_agent_pattern= Prefix "xen_api_libs/"
    ; client_ip_pattern= Full "192.168.1.1"
    }
  in
  let _ = Caller_table.insert table ~pattern 7 in
  let matching = {user_agent= "xen_api_libs/1.2"; client_ip= "192.168.1.1"} in
  let non_matching = {user_agent= "xen_api/1.2"; client_ip= "192.168.1.1"} in
  Alcotest.(check (list int))
    "prefix wildcard matches xen_api_libs/1.2" [7]
    (Caller_table.get table ~caller_id:matching) ;
  Alcotest.(check (list int))
    "prefix wildcard does not match different prefix" []
    (Caller_table.get table ~caller_id:non_matching)

let test_prefix_priority_over_full_wildcard () =
  let table = Caller_table.create () in
  let full =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Full "192.168.1.1"}
  in
  let prefix =
    {
      user_agent_pattern= Prefix "xen_api_libs/"
    ; client_ip_pattern= Full "192.168.1.1"
    }
  in
  let _ = Caller_table.insert table ~pattern:full 5 in
  let _ = Caller_table.insert table ~pattern:prefix 9 in
  let client = {user_agent= "xen_api_libs/1.2"; client_ip= "192.168.1.1"} in
  Alcotest.(check (list int))
    "prefix match ordered before full wildcard" [9; 5]
    (Caller_table.get table ~caller_id:client)

let test_get_ordered_specificity_ladder () =
  (* All eight insertable wildcard combinations match the same client.
     Inserted in a deliberately scrambled order; get must return them
     ordered by Key.compare (most specific first). *)
  let table = Caller_table.create () in
  let exact =
    {user_agent_pattern= Full "curl/1.2"; client_ip_pattern= Full "192.168.1.1"}
  in
  let ua_exact_ip_prefix =
    {user_agent_pattern= Full "curl/1.2"; client_ip_pattern= Prefix "192.168."}
  in
  let ua_prefix_ip_exact =
    {user_agent_pattern= Prefix "curl/"; client_ip_pattern= Full "192.168.1.1"}
  in
  let ua_exact_ip_full =
    {user_agent_pattern= Full "curl/1.2"; client_ip_pattern= Prefix ""}
  in
  let ua_prefix_ip_prefix =
    {user_agent_pattern= Prefix "curl/"; client_ip_pattern= Prefix "192.168."}
  in
  let ua_full_ip_exact =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Full "192.168.1.1"}
  in
  let ua_prefix_ip_full =
    {user_agent_pattern= Prefix "curl/"; client_ip_pattern= Prefix ""}
  in
  let ua_full_ip_prefix =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Prefix "192.168."}
  in
  (* Insert in a scrambled order to exercise the sorted-insert logic *)
  let _ = Caller_table.insert table ~pattern:ua_prefix_ip_full 7 in
  let _ = Caller_table.insert table ~pattern:exact 1 in
  let _ = Caller_table.insert table ~pattern:ua_full_ip_prefix 8 in
  let _ = Caller_table.insert table ~pattern:ua_prefix_ip_exact 3 in
  let _ = Caller_table.insert table ~pattern:ua_exact_ip_full 4 in
  let _ = Caller_table.insert table ~pattern:ua_full_ip_exact 6 in
  let _ = Caller_table.insert table ~pattern:ua_exact_ip_prefix 2 in
  let _ = Caller_table.insert table ~pattern:ua_prefix_ip_prefix 5 in
  let client = {user_agent= "curl/1.2"; client_ip= "192.168.1.1"} in
  Alcotest.(check (list int))
    "all eight overlapping patterns ordered most-specific first"
    [1; 2; 3; 4; 5; 6; 7; 8]
    (Caller_table.get table ~caller_id:client)

let test_get_ordered_tiebreak_lex () =
  (* When wildcard scores are identical, ties break lexicographically by
     user_agent then client_ip. All three patterns are ua-prefix + ip-exact
     (score (1,1,0)) and all match the client "abc/foo". *)
  let table = Caller_table.create () in
  let p_a =
    {user_agent_pattern= Prefix "a"; client_ip_pattern= Full "10.0.0.1"}
  in
  let p_ab =
    {user_agent_pattern= Prefix "ab"; client_ip_pattern= Full "10.0.0.1"}
  in
  let p_abc =
    {user_agent_pattern= Prefix "abc"; client_ip_pattern= Full "10.0.0.1"}
  in
  (* Insert in reverse lex order *)
  let _ = Caller_table.insert table ~pattern:p_abc 30 in
  let _ = Caller_table.insert table ~pattern:p_ab 20 in
  let _ = Caller_table.insert table ~pattern:p_a 10 in
  let client = {user_agent= "abc/foo"; client_ip= "10.0.0.1"} in
  (* Lex order: "a" < "ab" < "abc" *)
  Alcotest.(check (list int))
    "equal wildcard scores tie-break by user_agent lex order" [10; 20; 30]
    (Caller_table.get table ~caller_id:client)

let test_get_ordered_tiebreak_client_ip () =
  (* Ties on user_agent fall through to client_ip lex order. Patterns
     have identical user_agent and the same wildcard scores, so client_ip
     decides. *)
  let table = Caller_table.create () in
  let p1 =
    {user_agent_pattern= Full "curl/1.2"; client_ip_pattern= Prefix "10.0."}
  in
  let p2 =
    {user_agent_pattern= Full "curl/1.2"; client_ip_pattern= Prefix "10.0.0."}
  in
  let _ = Caller_table.insert table ~pattern:p2 200 in
  let _ = Caller_table.insert table ~pattern:p1 100 in
  let client = {user_agent= "curl/1.2"; client_ip= "10.0.0.5"} in
  Alcotest.(check (list int))
    "equal user_agent + equal scores tie-break by client_ip lex order"
    [100; 200]
    (Caller_table.get table ~caller_id:client)

let test_get_ordered_after_delete_reinsert () =
  (* Sorting is preserved across mutation: delete a middle entry and
     re-insert; the result list must still be ordered. *)
  let table = Caller_table.create () in
  let exact =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Full "1.2.3.4"}
  in
  let ua_full =
    {user_agent_pattern= Prefix ""; client_ip_pattern= Full "1.2.3.4"}
  in
  let ip_full =
    {user_agent_pattern= Full "curl"; client_ip_pattern= Prefix ""}
  in
  let _ = Caller_table.insert table ~pattern:ua_full 2 in
  let _ = Caller_table.insert table ~pattern:exact 1 in
  let _ = Caller_table.insert table ~pattern:ip_full 3 in
  let client = {user_agent= "curl"; client_ip= "1.2.3.4"} in
  Alcotest.(check (list int))
    "ordered before delete" [1; 3; 2]
    (Caller_table.get table ~caller_id:client) ;
  Caller_table.delete table ~pattern:ip_full ;
  Alcotest.(check (list int))
    "ordered after delete" [1; 2]
    (Caller_table.get table ~caller_id:client) ;
  let _ = Caller_table.insert table ~pattern:ip_full 30 in
  Alcotest.(check (list int))
    "ordered after re-insert in new position" [1; 30; 2]
    (Caller_table.get table ~caller_id:client)

let test =
  [
    ("Create empty table", `Quick, test_create)
  ; ("Insert entry", `Quick, test_insert)
  ; ("Insert duplicate", `Quick, test_insert_duplicate)
  ; ("Delete entry", `Quick, test_delete)
  ; ("Delete nonexistent entry", `Quick, test_delete_nonexistent)
  ; ("Get nonexistent", `Quick, test_get_nonexistent)
  ; ("Multiple entries", `Quick, test_multiple_entries)
  ; ( "Concurrent insert/delete stress"
    , `Quick
    , test_concurrent_insert_delete_stress
    )
  ; ("Get during delete race", `Quick, test_get_during_delete_race)
  ; ( "Wildcard user_agent matches any"
    , `Quick
    , test_wildcard_user_agent_matches_any
    )
  ; ( "Wildcard client_ip matches any"
    , `Quick
    , test_wildcard_client_ip_matches_any
    )
  ; ( "Wildcard priority: exact first"
    , `Quick
    , test_wildcard_match_priority_exact_first
    )
  ; ( "Wildcard priority: client_ip over user_agent"
    , `Quick
    , test_wildcard_match_priority_client_ip_over_user_agent
    )
  ; ("No spurious wildcard matches", `Quick, test_no_spurious_wildcard_matches)
  ; ("Reject all-wildcard key", `Quick, test_reject_all_wildcard_key)
  ; ("Prefix wildcard matches", `Quick, test_prefix_wildcard_matches_prefix)
  ; ( "Prefix wildcard preferred over full wildcard"
    , `Quick
    , test_prefix_priority_over_full_wildcard
    )
  ; ("LRU cache overflow", `Quick, test_lru_cache_overflow)
  ; ( "Ordered retrieval: full specificity ladder"
    , `Quick
    , test_get_ordered_specificity_ladder
    )
  ; ( "Ordered retrieval: lex tiebreak on user_agent"
    , `Quick
    , test_get_ordered_tiebreak_lex
    )
  ; ( "Ordered retrieval: lex tiebreak on client_ip"
    , `Quick
    , test_get_ordered_tiebreak_client_ip
    )
  ; ( "Ordered retrieval: preserved across delete and re-insert"
    , `Quick
    , test_get_ordered_after_delete_reinsert
    )
  ]

let () = Alcotest.run "Client table library" [("Client table tests", test)]
