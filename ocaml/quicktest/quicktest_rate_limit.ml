module Caller = Client.Client.Caller
module Rate_limit = Client.Client.Rate_limit
module Pool = Client.Client.Pool

(* Create an RPC function that uses a specific User-Agent header. Mirrors the
   transport chosen for the framework's default RPC so that the throttled and
   unthrottled measurements go over the same channel. *)
let make_rpc_with_user_agent user_agent =
  let http =
    Http.Request.make ~user_agent ~version:"1.1" ~keep_alive:false Http.Post "/"
  in
  let open Xmlrpc_client in
  let transport =
    if !Quicktest_args.using_unix_domain_socket then
      Unix Xapi_globs.unix_domain_socket
    else
      SSL
        ( SSL.make ~use_fork_exec_helper:false
            ~verify_cert:(Stunnel_client.pool ()) ()
        , !Quicktest_args.host
        , 443
        )
  in
  fun xml ->
    XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi" ~transport ~http xml

(* Build a caller + rate-limit pair and link them. Returns a cleanup
   thunk that tears them down in dependency order. *)
let with_throttled_caller rpc session_id ~name ~user_agent ~burst_size
    ~fill_rate k =
  let caller_ref =
    Caller.create ~rpc ~session_id ~name_label:name ~name_description:""
      ~user_agent ~client_ip:""
  in
  let rate_limit_ref =
    Rate_limit.create ~rpc ~session_id ~name_label:name ~name_description:""
      ~burst_size ~fill_rate
  in
  Rate_limit.add_caller ~rpc ~session_id ~self:rate_limit_ref ~caller:caller_ref ;
  let cleanup () =
    (* destroy clears each caller's rate_limit pointer first. *)
    (try Rate_limit.destroy ~rpc ~session_id ~self:rate_limit_ref with _ -> ()) ;
    try Caller.destroy ~rpc ~session_id ~self:caller_ref with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () -> k caller_ref rate_limit_ref)

(* Detect whether the server has rate limiting enabled. The Caller datamodel
   API works regardless of the feature flag (validation and DB CRUD always
   run), but dispatch-time bookkeeping (which feeds Caller.query_usage) only
   happens when rate_limit=true in xapi.conf. So if a probe of throttled calls
   leaves the caller's "calls" counter at 0, the server is running with the
   feature disabled and the throttling assertions cannot be meaningfully run. *)
let rate_limiting_active rpc session_id caller_ref ~throttled_rpc =
  for _ = 1 to 5 do
    ignore (Client.Client.Pool.get_all ~rpc:throttled_rpc ~session_id)
  done ;
  let usage =
    Client.Client.Caller.query_call_count ~rpc ~session_id ~self:caller_ref
  in
  usage > 0L

let rate_limit_throttling_test rpc session_id () =
  let test_user_agent =
    "quicktest-rate-limit-throttle-" ^ Uuidx.(to_string (make ()))
  in
  (* pool.get_all is not listed in call-costs.conf, so its cost falls back to
     xapi_caller.ml's default_token_cost of 1.0. Pick burst/fill so 100 calls
     take a few seconds while still throttling well below the unthrottled
     rate. *)
  let burst_size = 10.0 in
  let fill_rate = 40.0 in
  let call_cost = 1.0 in
  with_throttled_caller rpc session_id ~name:"quicktest-throttle"
    ~user_agent:test_user_agent ~burst_size ~fill_rate (fun caller_ref _ ->
      let throttled_rpc = make_rpc_with_user_agent test_user_agent in
      if not (rate_limiting_active rpc session_id caller_ref ~throttled_rpc)
      then
        Printf.printf
          "Rate limiting disabled on server (rate_limit=false in xapi.conf); \
           skipping throttling assertions\n\
           %!"
      else
        let make_call () =
          ignore (Client.Client.Pool.get_all ~rpc:throttled_rpc ~session_id)
        in
        (* Measure baseline: unthrottled calls *)
        let num_calls = 100 in
        let start_unthrottled = Mtime_clock.counter () in
        for _ = 1 to num_calls do
          ignore (Client.Client.Pool.get_all ~rpc ~session_id)
        done ;
        let elapsed_unthrottled =
          Mtime.Span.to_float_ns (Mtime_clock.count start_unthrottled) *. 1e-9
        in
        Printf.printf "%d unthrottled calls took %.3f seconds\n%!" num_calls
          elapsed_unthrottled ;
        let start_throttled = Mtime_clock.counter () in
        for _ = 1 to num_calls do
          make_call ()
        done ;
        let elapsed_throttled =
          Mtime.Span.to_float_ns (Mtime_clock.count start_throttled) *. 1e-9
        in
        Printf.printf "%d throttled calls took %.3f seconds\n%!" num_calls
          elapsed_throttled ;
        (* Token-bucket adds a fixed delay once the initial burst is drained:
           (N*cost - burst)/fill_rate. The unthrottled baseline captures
           per-call RPC overhead, so subtracting it isolates the throttler's
           contribution. Bound above and below to catch both under- and
           over-limiting. Requires both paths to share a transport. *)
        let num_calls_f = Float.of_int num_calls in
        let floor =
          Float.max 0.0 (((num_calls_f *. call_cost) -. burst_size) /. fill_rate)
        in
        let tolerance_low = 0.10 *. floor in
        let tolerance_high = 0.5 +. (0.20 *. floor) in
        Alcotest.(check bool)
          "Throttled call time above predicted rate limit overhead" true
          (elapsed_throttled >= floor -. tolerance_low) ;
        Alcotest.(check bool)
          "Throttled time below unthrottled + rate limit overhead" true
          (elapsed_throttled <= floor +. elapsed_unthrottled +. tolerance_high)
  )

(* Test that invalid rate limits are rejected *)
let rate_limit_invalid_test rpc session_id () =
  let is_invalid_value f =
    try f () ; false
    with Api_errors.Server_error (code, _) -> code = Api_errors.invalid_value
  in
  (* Creating with non-positive fill_rate or burst_size must fail before any
     state is left behind. *)
  Alcotest.(check bool)
    "Zero fill rate rejected at create" true
    (is_invalid_value (fun () ->
         let r =
           Rate_limit.create ~rpc ~session_id ~name_label:"invalid"
             ~name_description:"" ~burst_size:10.0 ~fill_rate:0.0
         in
         Rate_limit.destroy ~rpc ~session_id ~self:r
     )
    ) ;
  Alcotest.(check bool)
    "Negative fill rate rejected at create" true
    (is_invalid_value (fun () ->
         let r =
           Rate_limit.create ~rpc ~session_id ~name_label:"invalid"
             ~name_description:"" ~burst_size:10.0 ~fill_rate:(-1.0)
         in
         Rate_limit.destroy ~rpc ~session_id ~self:r
     )
    ) ;
  Alcotest.(check bool)
    "Zero burst rejected at create" true
    (is_invalid_value (fun () ->
         let r =
           Rate_limit.create ~rpc ~session_id ~name_label:"invalid"
             ~name_description:"" ~burst_size:0.0 ~fill_rate:1.0
         in
         Rate_limit.destroy ~rpc ~session_id ~self:r
     )
    ) ;
  (* Once a valid rate limit exists, setters must reject invalid updates. *)
  let r =
    Rate_limit.create ~rpc ~session_id ~name_label:"invalid-setters"
      ~name_description:"" ~burst_size:10.0 ~fill_rate:1.0
  in
  Fun.protect
    ~finally:(fun () -> Rate_limit.destroy ~rpc ~session_id ~self:r)
    (fun () ->
      Alcotest.(check bool)
        "set_fill_rate rejects zero" true
        (is_invalid_value (fun () ->
             Rate_limit.set_fill_rate ~rpc ~session_id ~self:r ~value:0.0
         )
        ) ;
      Alcotest.(check bool)
        "set_burst_size rejects negative" true
        (is_invalid_value (fun () ->
             Rate_limit.set_burst_size ~rpc ~session_id ~self:r ~value:(-1.0)
         )
        )
    )

let tests () =
  let open Qt_filter in
  [
    [
      ("rate_limit_throttling", `Slow, rate_limit_throttling_test)
    ; ("rate_limit_invalid_rejected", `Quick, rate_limit_invalid_test)
    ]
    |> conn
  ]
  |> List.concat
