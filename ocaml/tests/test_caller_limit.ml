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

(* Calls are served before [Xapi_caller.register] runs at startup, so a call
   can auto-register a caller for the exact pattern of an administrator's
   caller that is not in the table yet, and such a row persists across a
   restart. [register] gives the pattern to the administrator's caller and
   destroys the auto-registered duplicate, whichever of the two it meets
   first. It meets the rows in the order of [Db.Caller.get_all], which is that
   of their references, so the persisted pairs get chosen references. *)
let test_register_prefers_administrator_callers () =
  with_setup ~limit:(-1) (fun ~__context ->
      let client_ip = "10.0.5.1" in
      let row ?(ref = Ref.make ()) ~user_agent ~auto_registered () =
        let uuid = Uuidx.(to_string (make () : [`Caller] t)) in
        Db.Caller.create ~__context ~ref ~uuid ~name_label:user_agent
          ~name_description:"" ~user_agent ~client_ip
          ~last_access:Clock.Date.epoch ~groups:[] ~rate_limit:Ref.null
          ~auto_registered ;
        ref
      in
      let ref_number n =
        Ref.of_string
          (Printf.sprintf "%s00000000-0000-0000-0000-%012d" Ref.ref_prefix n)
      in
      (* An administrator's row that the table does not have yet, as before
         [register]: a call from its client auto-registers a duplicate. *)
      let early = row ~user_agent:"early" ~auto_registered:false () in
      call ~__context ~user_agent:"early" ~client_ip ;
      (* Duplicates left in the database by such calls: in one pair the
         auto-registered row has the lower reference, in the other the
         administrator's. *)
      let auto_a =
        row ~ref:(ref_number 1) ~user_agent:"dup-a" ~auto_registered:true ()
      in
      let dup_a =
        row ~ref:(ref_number 2) ~user_agent:"dup-a" ~auto_registered:false ()
      in
      let dup_b =
        row ~ref:(ref_number 3) ~user_agent:"dup-b" ~auto_registered:false ()
      in
      let auto_b =
        row ~ref:(ref_number 4) ~user_agent:"dup-b" ~auto_registered:true ()
      in
      check_user_agents ~__context "a duplicate row for each administrator"
        ["dup-a"; "dup-a"; "dup-b"; "dup-b"; "early"; "early"] ;
      let order = Db.Caller.get_all ~__context in
      let rec position i self = function
        | [] ->
            -1
        | r :: rs ->
            if r = self then
              i
            else
              position (i + 1) self rs
      in
      let admin_first (admin, auto) =
        position 0 admin order < position 0 auto order
      in
      Alcotest.(check bool)
        "register meets one pair's administrator first, the other's last" true
        (admin_first (dup_a, auto_a) <> admin_first (dup_b, auto_b)) ;
      Xapi_caller.register ~__context ;
      check_user_agents ~__context "the auto-registered duplicates destroyed"
        ["dup-a"; "dup-b"; "early"] ;
      List.iter
        (fun (user_agent, self) ->
          call ~__context ~user_agent ~client_ip ;
          Alcotest.(check bool)
            (user_agent ^ ": the administrator's row is registered")
            false
            (Db.Caller.get_auto_registered ~__context ~self) ;
          Alcotest.(check int64)
            (user_agent ^ ": the call is counted on the administrator's row")
            1L
            (Xapi_caller.query_call_count ~__context ~self)
        )
        [("early", early); ("dup-a", dup_a); ("dup-b", dup_b)]
  )

(* A [Caller.create] served before [Xapi_caller.register] finds no entry for
   the pattern of an administrator's caller that is not in the table yet, so
   it creates a second administrator's row, and [register] cannot insert the
   first one. Destroying that first row must leave the entry of the second. *)
let test_destroy_keeps_entry_of_other_row () =
  with_setup ~limit:(-1) (fun ~__context ->
      let user_agent = "admin" and client_ip = "10.0.6.1" in
      let first = Ref.make () in
      Db.Caller.create ~__context ~ref:first
        ~uuid:Uuidx.(to_string (make () : [`Caller] t))
        ~name_label:"first" ~name_description:"" ~user_agent ~client_ip
        ~last_access:Clock.Date.epoch ~groups:[] ~rate_limit:Ref.null
        ~auto_registered:false ;
      let second =
        Xapi_caller.create ~__context ~name_label:"second" ~name_description:""
          ~user_agent ~client_ip
      in
      Xapi_caller.register ~__context ;
      check_user_agents ~__context "two administrators' rows, one pattern"
        ["admin"; "admin"] ;
      Xapi_caller.destroy ~__context ~self:first ;
      call ~__context ~user_agent ~client_ip ;
      check_user_agents ~__context "no caller auto-registered" ["admin"] ;
      Alcotest.(check int64)
        "the call is counted on the remaining row" 1L
        (Xapi_caller.query_call_count ~__context ~self:second)
  ) ;
  (* Before [register], a call from the client of an administrator's row that
     is only in the database auto-registers a caller for its pattern.
     Destroying the administrator's row must leave that caller counted against
     the cap: with a cap of one, the next client's call evicts it. *)
  with_setup ~limit:1 (fun ~__context ->
      let user_agent = "admin" and client_ip = "10.0.6.2" in
      let first = Ref.make () in
      Db.Caller.create ~__context ~ref:first
        ~uuid:Uuidx.(to_string (make () : [`Caller] t))
        ~name_label:"first" ~name_description:"" ~user_agent ~client_ip
        ~last_access:Clock.Date.epoch ~groups:[] ~rate_limit:Ref.null
        ~auto_registered:false ;
      call ~__context ~user_agent ~client_ip ;
      check_user_agents ~__context "a caller auto-registered for the pattern"
        ["admin"; "admin"] ;
      Xapi_caller.destroy ~__context ~self:first ;
      call ~__context ~user_agent:"other" ~client_ip:"10.0.6.3" ;
      check_user_agents ~__context
        "the auto-registered caller is evicted at the cap" ["other"]
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

(* Run [body] with [hook] set to run once, as soon as the database has
   written the field [field] of a Rate_limit row: a change that writes the
   database and then installs a limiter is interrupted between the two. *)
let with_write_hook ~__context ~field ~hook body =
  let open Xapi_database in
  let name = "test-write-hook" and armed = ref true in
  let callback event _ =
    match event with
    | Db_cache_types.WriteField ("Rate_limit", _, f, _, _)
      when !armed && f = field ->
        armed := false ;
        hook ()
    | _ ->
        ()
  in
  let db = Context.database_of __context in
  Db_ref.update_database db
    (Db_cache_types.Database.register_callback name callback) ;
  Fun.protect body ~finally:(fun () ->
      Db_ref.update_database db
        (Db_cache_types.Database.unregister_callback name)
  )

(* Two changes of one rate limit at once must leave the limiter that xapi
   uses in step with the database. A database callback runs the change of
   the fill rate right after the change of the burst size has written the
   database, and before it installs its limiter, as if the first change
   were descheduled there. *)
let test_concurrent_changes_match_database () =
  with_setup ~limit:100 (fun ~__context ->
      let rl =
        Xapi_rate_limit.create ~__context ~name_label:"changes"
          ~name_description:"" ~burst_size:1. ~fill_rate:1e-6
      in
      with_write_hook ~__context ~field:"burst_size"
        ~hook:(fun () ->
          Xapi_rate_limit.set_fill_rate ~__context ~self:rl ~value:1e6
        )
        (fun () -> Xapi_rate_limit.set_burst_size ~__context ~self:rl ~value:2.) ;
      Alcotest.(check (float 0.))
        "fill rate in the database" 1e6
        (Db.Rate_limit.get_fill_rate ~__context ~self:rl) ;
      (* With a burst size of 2 and a fill rate of 1e6 per second, the
         third request is served at once; with the old fill rate of 1e-6,
         it would wait for about 11 days. *)
      let user_agent = "changes-agent" and client_ip = "10.0.3.1" in
      let caller =
        Xapi_caller.create ~__context ~name_label:"changes" ~name_description:""
          ~user_agent ~client_ip
      in
      Xapi_rate_limit.add_caller ~__context ~self:rl ~caller ;
      let served = Atomic.make 0 in
      for _ = 1 to 3 do
        Xapi_caller.submit_async ~user_agent ~client_ip
          ~callback:(fun () -> Atomic.incr served)
          ~task_create:(fun f -> f __context)
          1.
      done ;
      let deadline = Unix.gettimeofday () +. 5. in
      while Atomic.get served < 3 && Unix.gettimeofday () < deadline do
        Thread.delay 0.01
      done ;
      Alcotest.(check int)
        "requests served by the installed limiter" 3 (Atomic.get served) ;
      Xapi_rate_limit.destroy ~__context ~self:rl
  )

(* A change of a rate limit that races its destruction must not leave a
   limiter for the destroyed rate limit. A database callback destroys the
   rate limit right after the change of the burst size has written the
   database, and before it installs its limiter. *)
let test_change_racing_destroy_installs_nothing () =
  with_setup ~limit:100 (fun ~__context ->
      let rl =
        Xapi_rate_limit.create ~__context ~name_label:"destroyed"
          ~name_description:"" ~burst_size:1. ~fill_rate:1.
      in
      with_write_hook ~__context ~field:"burst_size"
        ~hook:(fun () -> Xapi_rate_limit.destroy ~__context ~self:rl)
        (fun () ->
          try Xapi_rate_limit.set_burst_size ~__context ~self:rl ~value:2.
          with _ -> ()
        ) ;
      Alcotest.(check bool)
        "a limiter is installed for the destroyed rate limit" false
        (Option.is_some (Xapi_rate_limit.find_bucket rl))
  )

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

(* A rate-limited HTTP handler must have run, on the connection's own thread,
   by the time the rate-limit wrapper returns: Http_svr reads the next request
   from the connection, or closes it, as soon as the wrapper returns. *)
let test_http_handler_runs_before_return () =
  with_setup ~limit:100 (fun ~__context ->
      let user_agent = "http-agent" and client_ip = "10.0.1.1" in
      let caller =
        Xapi_caller.create ~__context ~name_label:"http" ~name_description:""
          ~user_agent ~client_ip
      in
      let rate_limit =
        Xapi_rate_limit.create ~__context ~name_label:"http"
          ~name_description:"" ~burst_size:1. ~fill_rate:20.
      in
      Xapi_rate_limit.add_caller ~__context ~self:rate_limit ~caller ;
      let run_handler () =
        let ran_on = ref None in
        Xapi_http.rate_limit_handler ~name:"get_blob"
          (Some (user_agent, client_ip))
          (fun () -> ran_on := Some (Thread.id (Thread.self ())))
          () ;
        !ran_on
      in
      let this_thread = Some (Thread.id (Thread.self ())) in
      (* The first request takes the only token, so the second one is
         delayed until the bucket refills. *)
      Alcotest.(check (option int))
        "immediate handler ran on the calling thread" this_thread
        (run_handler ()) ;
      Alcotest.(check (option int))
        "delayed handler ran on the calling thread before returning" this_thread
        (run_handler ()) ;
      Xapi_rate_limit.destroy ~__context ~self:rate_limit
  )

(* Changing a rate limit's parameters waits for the old limiter to serve its
   queue. Meanwhile, the dispatch path must still be able to look up other
   rate limits. *)
let test_rate_limit_update_does_not_stall_lookups () =
  with_setup ~limit:100 (fun ~__context ->
      let create name =
        Xapi_rate_limit.create ~__context ~name_label:name ~name_description:""
          ~burst_size:1. ~fill_rate:10.
      in
      let changed = create "changed" and other = create "other" in
      let user_agent = "update-agent" and client_ip = "10.0.2.1" in
      let caller =
        Xapi_caller.create ~__context ~name_label:"update" ~name_description:""
          ~user_agent ~client_ip
      in
      Xapi_rate_limit.add_caller ~__context ~self:changed ~caller ;
      (* Queue a request on the old limiter that is served only when [gate]
         opens, so that deleting the old limiter waits for the gate. *)
      let gate = Semaphore.Binary.make false in
      let wait_for_gate () =
        Semaphore.Binary.acquire gate ;
        Semaphore.Binary.release gate
      in
      let open_gate () = Semaphore.Binary.release gate in
      (* The first request takes the only token, so the second is queued. *)
      Xapi_caller.submit_sync ~user_agent ~client_ip ~callback:ignore
        ~task_create:(fun f -> f __context)
        1. ;
      Xapi_caller.submit_async ~user_agent ~client_ip ~callback:wait_for_gate
        ~task_create:(fun f -> f __context)
        1. ;
      let setter =
        Thread.create
          (fun () ->
            Xapi_rate_limit.set_fill_rate ~__context ~self:changed ~value:2.
          )
          ()
      in
      (* Open the gate after 2 s at the latest, so that the test ends even when
         the lookup below stalls. It is not joined, so that the test does not
         wait for it when the gate is opened below. *)
      let _ : Thread.t =
        Thread.create (fun () -> Thread.delay 2. ; open_gate ()) ()
      in
      Thread.delay 0.2 ;
      let start = Unix.gettimeofday () in
      let found = Xapi_rate_limit.find_bucket other in
      let elapsed = Unix.gettimeofday () -. start in
      open_gate () ;
      Thread.join setter ;
      Alcotest.(check bool) "other rate limit found" true (Option.is_some found) ;
      Alcotest.(check bool)
        (Printf.sprintf "lookup took %.3f s, less than 1 s" elapsed)
        true (elapsed < 1.) ;
      Xapi_rate_limit.destroy ~__context ~self:changed ;
      Xapi_rate_limit.destroy ~__context ~self:other
  )

(* The handlers of these actions dispatch API calls, and the dispatch
   (Server_helpers) counts and charges every external call. The HTTP wrapper
   must therefore leave them alone, or each call would be charged twice. *)
let test_http_api_actions_charged_once () =
  with_setup ~limit:100 (fun ~__context ->
      let user_agent = "api-http-agent" and client_ip = "10.0.3.1" in
      let caller =
        Xapi_caller.create ~__context ~name_label:"api-http"
          ~name_description:"" ~user_agent ~client_ip
      in
      let calls_counted_by_wrapper name =
        let before = Xapi_caller.query_call_count ~__context ~self:caller in
        let ran = ref false in
        Xapi_http.rate_limit_handler ~name
          (Some (user_agent, client_ip))
          (fun () -> ran := true)
          () ;
        Alcotest.(check bool) (name ^ " handler ran") true !ran ;
        Int64.sub (Xapi_caller.query_call_count ~__context ~self:caller) before
      in
      List.iter
        (fun name ->
          Alcotest.(check int64)
            (name ^ " not counted by the HTTP wrapper")
            0L
            (calls_counted_by_wrapper name)
        )
        ["post_root"; "post_RPC2"; "post_json"; "post_jsonrpc"; "post_cli"] ;
      Alcotest.(check int64)
        "get_blob counted by the HTTP wrapper" 1L
        (calls_counted_by_wrapper "get_blob")
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

(* Rate_limit.add_caller with a reference that names no Rate_limit row, such
   as one destroyed before the call, must fail and leave the caller as it
   was. Stored, the reference would name no row, and the caller would run
   with no rate limit. *)
let test_add_caller_refuses_missing_rate_limit () =
  with_setup ~limit:100 (fun ~__context ->
      let rate_limit =
        Xapi_rate_limit.create ~__context ~name_label:"gone"
          ~name_description:"" ~burst_size:1. ~fill_rate:1.
      in
      Xapi_rate_limit.destroy ~__context ~self:rate_limit ;
      let caller =
        Xapi_caller.create ~__context ~name_label:"c" ~name_description:""
          ~user_agent:"missing-rl-agent" ~client_ip:""
      in
      let refused =
        try
          Xapi_rate_limit.add_caller ~__context ~self:rate_limit ~caller ;
          false
        with Db_exn.DBCache_NotFound _ -> true
      in
      Alcotest.(check bool) "add_caller refused" true refused ;
      Alcotest.(check string)
        "Caller.rate_limit unchanged" (Ref.string_of Ref.null)
        (Ref.string_of (Db.Caller.get_rate_limit ~__context ~self:caller)) ;
      Xapi_caller.destroy ~__context ~self:caller
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
  ; ( "test_register_prefers_administrator_callers"
    , `Quick
    , test_register_prefers_administrator_callers
    )
  ; ( "test_destroy_keeps_entry_of_other_row"
    , `Quick
    , test_destroy_keeps_entry_of_other_row
    )
  ; ( "test_rate_limit_rejects_non_finite"
    , `Quick
    , test_rate_limit_rejects_non_finite
    )
  ; ( "test_concurrent_changes_match_database"
    , `Quick
    , test_concurrent_changes_match_database
    )
  ; ( "test_change_racing_destroy_installs_nothing"
    , `Quick
    , test_change_racing_destroy_installs_nothing
    )
  ; ( "test_http_handler_runs_before_return"
    , `Quick
    , test_http_handler_runs_before_return
    )
  ; ( "test_rate_limit_update_does_not_stall_lookups"
    , `Quick
    , test_rate_limit_update_does_not_stall_lookups
    )
  ; ( "test_http_api_actions_charged_once"
    , `Quick
    , test_http_api_actions_charged_once
    )
  ; ("test_negative_call_cost_ignored", `Quick, test_negative_call_cost_ignored)
  ; ( "test_add_caller_refuses_missing_rate_limit"
    , `Quick
    , test_add_caller_refuses_missing_rate_limit
    )
  ]
