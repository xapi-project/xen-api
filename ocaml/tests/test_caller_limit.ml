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

(* Rate limit parameters must be positive finite numbers (an infinite burst
   size is allowed). NaN fails every comparison, and an infinite fill rate
   can turn the token count into NaN. *)
let test_rate_limit_rejects_non_finite () =
  let __context = Lazy.force context in
  let invalid f =
    match f () with
    | (_ : API.ref_Rate_limit) ->
        false
    | exception Api_errors.Server_error (code, _) ->
        code = Api_errors.invalid_value
  in
  let create ~burst_size ~fill_rate () =
    Xapi_rate_limit.create ~__context ~name_label:"invalid" ~name_description:""
      ~burst_size ~fill_rate
  in
  List.iter
    (fun (burst_size, fill_rate) ->
      Alcotest.(check bool)
        (Printf.sprintf "burst %g, rate %g is INVALID_VALUE" burst_size
           fill_rate
        )
        true
        (invalid (create ~burst_size ~fill_rate))
    )
    [(1., Float.nan); (1., Float.infinity); (Float.nan, 1.); (0., 1.); (1., 0.)] ;
  (* A refused create must not leave a row behind. *)
  Alcotest.(check bool)
    "no Rate_limit row holds an invalid parameter" true
    (List.for_all
       (fun self ->
         let fill_rate = Db.Rate_limit.get_fill_rate ~__context ~self in
         let burst_size = Db.Rate_limit.get_burst_size ~__context ~self in
         Float.is_finite fill_rate && fill_rate > 0. && burst_size > 0.
       )
       (Db.Rate_limit.get_all ~__context)
    ) ;
  let r = create ~burst_size:1. ~fill_rate:1. () in
  List.iter
    (fun (name, set) ->
      Alcotest.(check bool)
        (name ^ " nan is INVALID_VALUE")
        true
        (invalid (fun () ->
             set ~__context ~self:r ~value:Float.nan ;
             r
         )
        )
    )
    [
      ("set_fill_rate", Xapi_rate_limit.set_fill_rate)
    ; ("set_burst_size", Xapi_rate_limit.set_burst_size)
    ] ;
  Alcotest.(check (pair (float 0.) (float 0.)))
    "fill_rate and burst_size unchanged" (1., 1.)
    ( Db.Rate_limit.get_fill_rate ~__context ~self:r
    , Db.Rate_limit.get_burst_size ~__context ~self:r
    ) ;
  Xapi_rate_limit.destroy ~__context ~self:r

(* A dispatch that found the limiter of a rate limit, and submits to it after
   the rate limit was changed or destroyed, must not fail: its call runs
   under the new limiter, or without limit. *)
let test_submit_after_limiter_deleted () =
  with_setup ~limit:100 (fun ~__context ->
      (* The call returns the same result under a limiter and without limit,
         so record what each submission to a limiter did: a call run under
         the new limiter is refused once and then served. *)
      let submissions = ref [] in
      let submit_fn rl ?observer ~callback ~caller_details amount =
        match
          Rate_limit_lib.Rate_limit.submit_sync rl ?observer ~callback
            ~caller_details amount
        with
        | result ->
            submissions := "served" :: !submissions ;
            result
        | exception Rate_limit_lib.Rate_limit.Deleted ->
            submissions := "refused" :: !submissions ;
            raise Rate_limit_lib.Rate_limit.Deleted
      in
      let submit ~self rl =
        submissions := [] ;
        let result =
          Xapi_caller.submit_to_limiter ~submit_fn ~rate_limit_ref:self
            ~callback:(fun () -> "ran")
            ~caller_details:"test" 1. rl
        in
        (result, List.rev !submissions)
      in
      let found ~self =
        match Xapi_rate_limit.find_bucket self with
        | Some rl ->
            rl
        | None ->
            Alcotest.fail "no limiter is installed"
      in
      let changed =
        Xapi_rate_limit.create ~__context ~name_label:"changed"
          ~name_description:"" ~burst_size:10. ~fill_rate:1.
      in
      let old_limiter = found ~self:changed in
      Xapi_rate_limit.set_fill_rate ~__context ~self:changed ~value:2. ;
      Alcotest.(check (pair string (list string)))
        "the call runs under the new limiter"
        ("ran", ["refused"; "served"])
        (submit ~self:changed old_limiter) ;
      let destroyed =
        Xapi_rate_limit.create ~__context ~name_label:"destroyed"
          ~name_description:"" ~burst_size:10. ~fill_rate:1.
      in
      let old_limiter = found ~self:destroyed in
      Xapi_rate_limit.destroy ~__context ~self:destroyed ;
      Alcotest.(check (pair string (list string)))
        "the call runs without limit" ("ran", ["refused"])
        (submit ~self:destroyed old_limiter) ;
      Xapi_rate_limit.destroy ~__context ~self:changed
  )

(* A negative call cost in the costs file is ignored: it would add tokens to
   the caller's bucket on every call, and the caller would not be limited. *)
let test_negative_call_cost_ignored () =
  let path = Filename.temp_file "call-costs" ".conf" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove path ;
      Xapi_caller.load_token_costs ~path:"/nonexistent/call-costs.conf" ()
    )
    (fun () ->
      Out_channel.with_open_text path (fun oc ->
          output_string oc "VM.clone = -5\nVM.copy = 300\nVM.start = 0\n"
      ) ;
      Xapi_caller.load_token_costs ~path () ;
      Alcotest.(check (float 0.))
        "cost of VM.copy" 300.
        (Xapi_caller.get_token_cost "VM.copy") ;
      Alcotest.(check (float 0.))
        "cost of VM.start" 0.
        (Xapi_caller.get_token_cost "VM.start") ;
      Alcotest.(check (float 0.))
        "cost of VM.clone, negative in the file" Xapi_caller.default_token_cost
        (Xapi_caller.get_token_cost "VM.clone")
    )

let test =
  [
    ( "test_submit_after_limiter_deleted"
    , `Quick
    , test_submit_after_limiter_deleted
    )
  ; ( "test_evicts_least_recently_called"
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
  ; ( "test_rate_limit_rejects_non_finite"
    , `Quick
    , test_rate_limit_rejects_non_finite
    )
  ; ("test_negative_call_cost_ignored", `Quick, test_negative_call_cost_ignored)
  ]
