module Rate_limit = Client.Client.Rate_limit
module Pool = Client.Client.Pool

(* Create an RPC function that uses a specific User-Agent header *)
let make_rpc_with_user_agent user_agent =
  let http =
    Http.Request.make ~user_agent ~version:"1.1" ~keep_alive:false Http.Post "/"
  in
  fun xml ->
    Xmlrpc_client.XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi"
      ~transport:(Unix Xapi_globs.unix_domain_socket) ~http xml

let rate_limit_throttling_test rpc session_id () =
  let test_user_agent =
    "quicktest-rate-limit-throttle-" ^ Uuidx.(to_string (make ()))
  in
  (* pool.get_all costs ~0.000059 tokens per call.
     Use a tiny burst (0.001) and slow fill rate (0.001 tokens/sec) so that:
     - First ~17 calls fit in burst
     - After that, we can only do ~17 calls/sec
     This makes throttling observable with reasonable call counts. *)
  let burst_size = 0.001 in
  let fill_rate = 0.001 in
  let rate_limit_ref =
    Rate_limit.create ~rpc ~session_id ~user_agent:test_user_agent ~host_ip:""
      ~burst_size ~fill_rate
  in
  let cleanup () = Rate_limit.destroy ~rpc ~session_id ~self:rate_limit_ref in
  Fun.protect ~finally:cleanup (fun () ->
      let throttled_rpc = make_rpc_with_user_agent test_user_agent in
      let make_call () =
        ignore (Client.Client.Pool.get_all ~rpc:throttled_rpc ~session_id)
      in
      (* Measure baseline: unthrottled calls *)
      let num_calls = 500 in
      let start_unthrottled = Mtime_clock.counter () in
      for _ = 1 to num_calls do
        ignore (Client.Client.Pool.get_all ~rpc ~session_id)
      done ;
      let elapsed_unthrottled =
        Mtime.Span.to_float_ns (Mtime_clock.count start_unthrottled) *. 1e-9
      in
      Printf.printf "%d unthrottled calls took %.3f seconds\n%!" num_calls
        elapsed_unthrottled ;
      (* Now measure throttled calls - these should be much slower.
         With 0.001 burst and 0.001 fill rate, after ~17 calls exhaust the burst,
         each subsequent call must wait for tokens to refill. *)
      let start_throttled = Mtime_clock.counter () in
      for _ = 1 to num_calls do
        make_call ()
      done ;
      let elapsed_throttled =
        Mtime.Span.to_float_ns (Mtime_clock.count start_throttled) *. 1e-9
      in
      Printf.printf "%d throttled calls took %.3f seconds\n%!" num_calls
        elapsed_throttled ;
      let throttle_ratio = elapsed_throttled /. max elapsed_unthrottled 0.001 in
      Printf.printf "Throttle ratio: %.2fx slower\n%!" throttle_ratio ;
      (* Throttled calls should take noticeably longer - at least 2x slower
         given the restrictive rate limit *)
      Alcotest.(check bool)
        "Rate limiting causes observable slowdown" true (throttle_ratio > 2.0)
  )

(* Test that invalid rate limits are rejected *)
let rate_limit_invalid_test rpc session_id () =
  (* Empty user_agent and host_ip should fail *)
  let empty_key_rejected =
    try
      let ref =
        Rate_limit.create ~rpc ~session_id ~user_agent:"" ~host_ip:""
          ~burst_size:10.0 ~fill_rate:1.0
      in
      Rate_limit.destroy ~rpc ~session_id ~self:ref ;
      false
    with Api_errors.Server_error (code, _) -> code = Api_errors.invalid_value
  in
  Alcotest.(check bool)
    "Empty user_agent and host_ip rejected" true empty_key_rejected ;
  (* Zero fill rate should fail *)
  let zero_fill_rejected =
    try
      let ref =
        Rate_limit.create ~rpc ~session_id ~user_agent:"test-agent" ~host_ip:""
          ~burst_size:10.0 ~fill_rate:0.0
      in
      Rate_limit.destroy ~rpc ~session_id ~self:ref ;
      false
    with Api_errors.Server_error (code, _) -> code = Api_errors.invalid_value
  in
  Alcotest.(check bool) "Zero fill rate rejected" true zero_fill_rejected ;
  (* Negative fill rate should fail *)
  let negative_fill_rejected =
    try
      let ref =
        Rate_limit.create ~rpc ~session_id ~user_agent:"test-agent2" ~host_ip:""
          ~burst_size:10.0 ~fill_rate:(-1.0)
      in
      Rate_limit.destroy ~rpc ~session_id ~self:ref ;
      false
    with Api_errors.Server_error (code, _) -> code = Api_errors.invalid_value
  in
  Alcotest.(check bool)
    "Negative fill rate rejected" true negative_fill_rejected

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
