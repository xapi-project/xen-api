(*
 * Copyright (C) Cloud Software Group, Inc.
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

(* Exercises the cap on auto-registered callers (Xapi_globs.max_auto_registered_callers)
   and the least-recently-called eviction policy in Xapi_caller. The in-memory
   caller_table is process-global, so all cases share one database context and
   clean up through the public [destroy] between phases. *)

let context = lazy (Test_common.make_test_database ())

(* Run the auto-create/rate-limit machinery synchronously by giving [submit_sync]
   a [task_create] that runs its closure inline against the test context. *)
let call ~__context ~user_agent ~client_ip =
  Xapi_caller.submit_sync ~user_agent ~client_ip
    ~callback:(fun () -> ())
    ~task_create:(fun f -> f __context)
    1.0

(* Drop every caller so the global caller_table and the database start empty. *)
let reset ~__context =
  List.iter
    (fun self -> Xapi_caller.destroy ~__context ~self)
    (Db.Caller.get_all ~__context)

let user_agents ~__context =
  Db.Caller.get_all ~__context
  |> List.map (fun self -> Db.Caller.get_user_agent ~__context ~self)
  |> List.sort String.compare

let check_user_agents ~__context msg expected =
  Alcotest.(check (list string)) msg expected (user_agents ~__context)

let with_setup ~limit f =
  let __context = Lazy.force context in
  reset ~__context ;
  Xapi_globs.rate_limit_enabled := true ;
  Xapi_globs.max_auto_registered_callers := limit ;
  f ~__context

(* At the cap, a further distinct caller evicts the auto-registered caller with
   the least recent call - not merely the oldest by creation. *)
let test_evicts_least_recently_called () =
  with_setup ~limit:2 (fun ~__context ->
      call ~__context ~user_agent:"agent-1" ~client_ip:"10.0.0.1" ;
      call ~__context ~user_agent:"agent-2" ~client_ip:"10.0.0.2" ;
      check_user_agents ~__context "two callers auto-registered"
        ["agent-1"; "agent-2"] ;
      (* Touch agent-1 again so agent-2 becomes the least recently called. *)
      call ~__context ~user_agent:"agent-1" ~client_ip:"10.0.0.1" ;
      (* A third distinct caller trips the cap and evicts agent-2. *)
      call ~__context ~user_agent:"agent-3" ~client_ip:"10.0.0.3" ;
      check_user_agents ~__context "agent-2 (least recently called) evicted"
        ["agent-1"; "agent-3"]
  )

(* Manually created callers are not auto-registered: they neither count towards
   the cap nor get evicted by it. *)
let test_manual_callers_not_evicted () =
  with_setup ~limit:2 (fun ~__context ->
      let (_ : API.ref_Caller) =
        Xapi_caller.create ~__context ~name_label:"manual"
          ~name_description:"manually created" ~user_agent:"manual-agent"
          ~client_ip:"192.168.0.1"
      in
      call ~__context ~user_agent:"agent-1" ~client_ip:"10.0.0.1" ;
      call ~__context ~user_agent:"agent-2" ~client_ip:"10.0.0.2" ;
      (* Trips the cap: an auto caller is evicted, the manual one is not. *)
      call ~__context ~user_agent:"agent-3" ~client_ip:"10.0.0.3" ;
      let agents = user_agents ~__context in
      Alcotest.(check bool)
        "manual caller survives eviction" true
        (List.mem "manual-agent" agents) ;
      Alcotest.(check int)
        "cap counts only auto-registered callers" 3 (List.length agents)
  )

(* A limit of 0 disables auto-registration: no callers are auto-created. *)
let test_zero_limit_disables_autoregistration () =
  with_setup ~limit:0 (fun ~__context ->
      call ~__context ~user_agent:"agent-1" ~client_ip:"10.0.0.1" ;
      call ~__context ~user_agent:"agent-2" ~client_ip:"10.0.0.2" ;
      check_user_agents ~__context "no callers auto-registered when limit is 0"
        []
  )

(* A negative limit means unbounded - every distinct caller is registered and
   none are evicted. *)
let test_negative_limit_is_unbounded () =
  with_setup ~limit:(-1) (fun ~__context ->
      call ~__context ~user_agent:"agent-1" ~client_ip:"10.0.0.1" ;
      call ~__context ~user_agent:"agent-2" ~client_ip:"10.0.0.2" ;
      call ~__context ~user_agent:"agent-3" ~client_ip:"10.0.0.3" ;
      check_user_agents ~__context "no eviction when limit is negative"
        ["agent-1"; "agent-2"; "agent-3"]
  )

(* The RRD reporter aggregates per-caller usage into per-group totals. A caller
   in several groups contributes to each; a caller in no group contributes to
   none. *)
let test_group_totals_aggregate_callers () =
  with_setup ~limit:100 (fun ~__context ->
      let mk user_agent client_ip =
        Xapi_caller.create ~__context ~name_label:user_agent
          ~name_description:"" ~user_agent ~client_ip
      in
      let c1 = mk "a1" "10.0.0.1" in
      let c2 = mk "a2" "10.0.0.2" in
      let _ungrouped = mk "a3" "10.0.0.3" in
      (* Accumulate stats: one call for a1, two for a2, one for the ungrouped
         caller a3 (which should not show up in any group total). *)
      call ~__context ~user_agent:"a1" ~client_ip:"10.0.0.1" ;
      call ~__context ~user_agent:"a2" ~client_ip:"10.0.0.2" ;
      call ~__context ~user_agent:"a2" ~client_ip:"10.0.0.2" ;
      call ~__context ~user_agent:"a3" ~client_ip:"10.0.0.3" ;
      Xapi_caller.add_group ~__context ~self:c1 ~group:"g1" ;
      Xapi_caller.add_group ~__context ~self:c2 ~group:"g1" ;
      Xapi_caller.add_group ~__context ~self:c2 ~group:"g2" ;
      let summary =
        Xapi_caller.group_totals ()
        |> List.map (fun (g, tokens, calls) ->
            Printf.sprintf "%s:%.0f:%d" g tokens calls
        )
        |> List.sort String.compare
      in
      (* g1 = a1 (1) + a2 (2); g2 = a2 (2); a3 is in no group. *)
      Alcotest.(check (list string))
        "per-group token/call totals" ["g1:3:3"; "g2:2:2"] summary
  )

let test =
  [
    ( "test_evicts_least_recently_called"
    , `Quick
    , test_evicts_least_recently_called
    )
  ; ( "test_group_totals_aggregate_callers"
    , `Quick
    , test_group_totals_aggregate_callers
    )
  ; ("test_manual_callers_not_evicted", `Quick, test_manual_callers_not_evicted)
  ; ( "test_zero_limit_disables_autoregistration"
    , `Quick
    , test_zero_limit_disables_autoregistration
    )
  ; ( "test_negative_limit_is_unbounded"
    , `Quick
    , test_negative_limit_is_unbounded
    )
  ]
