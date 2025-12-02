open Thread
open Rate_limit

let test_bad_fill_rate () =
  let tb_zero = Token_bucket.create ~burst_size:1.0 ~fill_rate:0.0 in
  Alcotest.(check bool)
    "Creating a token bucket with 0 fill rate should fail" true (tb_zero = None) ;
  let tb_negative = Token_bucket.create ~burst_size:1.0 ~fill_rate:~-.1.0 in
  Alcotest.(check bool)
    "Creating a token bucket with negative fill rate should fail" true
    (tb_negative = None)

let test_consume_removes_correct_amount () =
  let initial_time = Mtime.Span.of_uint64_ns 0L in
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp initial_time ~burst_size:10.0
         ~fill_rate:2.0
      )
  in

  Alcotest.(check (float 0.0))
    "Initial tokens should be burst_size" 10.0
    (Token_bucket.peek_with_timestamp initial_time tb) ;

  let consume_time = Mtime.Span.of_uint64_ns 1_000_000_000L in
  let success =
    Token_bucket.consume_with_timestamp (fun () -> consume_time) tb 3.0
  in
  Alcotest.(check bool) "Consume 3 tokens should succeed" true success ;
  Alcotest.(check (float 0.0))
    "After consume, tokens should be 7" 7.0
    (Token_bucket.peek_with_timestamp consume_time tb)

let test_consume_more_than_available () =
  let initial_time = Mtime.Span.of_uint64_ns 0L in
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp initial_time ~burst_size:5.0
         ~fill_rate:1.0
      )
  in

  let _ = Token_bucket.consume_with_timestamp (fun () -> initial_time) tb 4.0 in

  let consume_time = Mtime.Span.of_uint64_ns 1_000_000_000L in
  let success =
    Token_bucket.consume_with_timestamp (fun () -> consume_time) tb 10.0
  in
  Alcotest.(check bool) "Consume more than available should fail" false success ;
  Alcotest.(check (float 0.0))
    "After failed consume, tokens should be 2" 2.0
    (Token_bucket.peek_with_timestamp consume_time tb)

let test_consume_refills_before_removing () =
  let initial_time = Mtime.Span.of_uint64_ns 0L in
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp initial_time ~burst_size:10.0
         ~fill_rate:2.0
      )
  in

  let first_consume =
    Token_bucket.consume_with_timestamp (fun () -> initial_time) tb 5.0
  in
  Alcotest.(check bool) "First consume should succeed" true first_consume ;

  let later_time = Mtime.Span.of_uint64_ns 3_000_000_000L in
  let second_consume =
    Token_bucket.consume_with_timestamp (fun () -> later_time) tb 8.0
  in

  Alcotest.(check bool)
    "Second consume after refill should succeed" true second_consume ;

  Alcotest.(check (float 0.0))
    "After refill and consume, tokens should be 2" 2.0
    (Token_bucket.peek_with_timestamp later_time tb)

let test_peek_respects_burst_size () =
  let initial_time = Mtime.Span.of_uint64_ns 0L in
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp initial_time ~burst_size:10.0
         ~fill_rate:5.0
      )
  in

  let _ = Token_bucket.consume_with_timestamp (fun () -> initial_time) tb 8.0 in

  let later_time = Mtime.Span.of_uint64_ns 10_000_000_000L in
  let available = Token_bucket.peek_with_timestamp later_time tb in
  Alcotest.(check (float 0.0))
    "Peek should respect burst_size limit" 10.0 available

let test_concurrent_access () =
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp Mtime.Span.zero ~burst_size:15.0
         ~fill_rate:0.01
      )
  in
  let threads =
    Array.init 10 (fun _ ->
        create
          (fun () ->
            Token_bucket.consume_with_timestamp
              (fun () -> Mtime.Span.zero)
              tb 1.0
          )
          ()
    )
  in
  Array.iter Thread.join threads ;
  Alcotest.(check (float 0.0))
    "Threads consuming concurrently should all remove from token amount"
    (Token_bucket.peek_with_timestamp Mtime.Span.zero tb)
    5.0

let test_sleep () =
  let tb = Option.get (Token_bucket.create ~burst_size:20.0 ~fill_rate:5.0) in
  let _ = Token_bucket.consume tb 10.0 in
  Thread.delay 1.0 ;
  Alcotest.(check (float 0.2))
    "Sleep 1 should refill token bucket by fill_rate" 15.0 (Token_bucket.peek tb)

let test_system_time_versions () =
  let tb = Option.get (Token_bucket.create ~burst_size:10.0 ~fill_rate:2.0) in

  let initial_peek = Token_bucket.peek tb in
  Alcotest.(check (float 0.01))
    "System time peek should return burst_size initially" 10.0 initial_peek ;

  let consume_result = Token_bucket.consume tb 3.0 in
  Alcotest.(check bool) "System time consume should succeed" true consume_result ;

  let after_consume_peek = Token_bucket.peek tb in
  Alcotest.(check (float 0.01))
    "After consume, should have 7 tokens" 7.0 after_consume_peek

let test_concurrent_system_time () =
  let tb = Option.get (Token_bucket.create ~burst_size:100.0 ~fill_rate:10.0) in
  let num_threads = 20 in
  let consume_per_thread = 3 in

  let threads =
    Array.init num_threads (fun _ ->
        create
          (fun () ->
            for _ = 1 to consume_per_thread do
              ignore (Token_bucket.consume tb 1.0)
            done
          )
          ()
    )
  in
  Array.iter Thread.join threads ;

  let remaining = Token_bucket.peek tb in
  let expected_remaining =
    100.0 -. float_of_int (num_threads * consume_per_thread)
  in
  Alcotest.(check (float 0.1))
    "Concurrent system time consumption should work correctly"
    expected_remaining remaining

let test_consume_more_than_available_concurrent () =
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp Mtime.Span.zero ~burst_size:5.0
         ~fill_rate:0.1
      )
  in
  let num_threads = 10 in
  let consume_per_thread = 1 in
  let successful_consumes = ref 0 in
  let counter_mutex = Mutex.create () in

  let threads =
    Array.init num_threads (fun _ ->
        create
          (fun () ->
            let success =
              Token_bucket.consume_with_timestamp
                (fun () -> Mtime.Span.zero)
                tb
                (float_of_int consume_per_thread)
            in
            if success then (
              Mutex.lock counter_mutex ;
              incr successful_consumes ;
              Mutex.unlock counter_mutex
            )
          )
          ()
    )
  in
  Array.iter Thread.join threads ;

  Alcotest.(check int)
    "Only 5 consumptions should succeed" 5 !successful_consumes ;
  Alcotest.(check (float 0.1))
    "Bucket should be empty after consumptions" 0.0
    (Token_bucket.peek_with_timestamp Mtime.Span.zero tb)

let test_delay_until_available () =
  let initial_time = Mtime.Span.of_uint64_ns 0L in
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp initial_time ~burst_size:10.0
         ~fill_rate:2.0
      )
  in

  let _ =
    Token_bucket.consume_with_timestamp (fun () -> initial_time) tb 10.0
  in

  let delay =
    Token_bucket.delay_until_available_timestamp initial_time tb 4.0
  in
  Alcotest.(check (float 0.01))
    "Delay for 4 tokens at 2 tokens/sec should be 2 seconds" 2.0 delay ;

  let tb_fresh =
    Option.get (Token_bucket.create ~burst_size:10.0 ~fill_rate:2.0)
  in
  let _ = Token_bucket.consume tb_fresh 10.0 in
  let delay_system = Token_bucket.delay_until_available tb_fresh 4.0 in

  Alcotest.(check (float 0.1))
    "System time delay should be approximately 2 seconds" 2.0 delay_system

let test_edge_cases () =
  let tb =
    Option.get
      (Token_bucket.create_with_timestamp Mtime.Span.zero ~burst_size:5.0
         ~fill_rate:1.0
      )
  in
  let success =
    Token_bucket.consume_with_timestamp (fun () -> Mtime.Span.zero) tb 0.0
  in
  Alcotest.(check bool) "Consuming zero tokens should succeed" true success ;

  let tb_small =
    Option.get
      (Token_bucket.create_with_timestamp Mtime.Span.zero ~burst_size:1.0
         ~fill_rate:0.1
      )
  in
  let success_small =
    Token_bucket.consume_with_timestamp
      (fun () -> Mtime.Span.zero)
      tb_small 0.001
  in
  Alcotest.(check bool)
    "Consuming very small amount should succeed" true success_small

let test_consume_quickcheck =
  let open QCheck.Gen in
  let gen_operations =
    let gen_operation =
      pair (float_range 0.0 1000.0) (int_range 0 1_000_000_000)
    in
    list_size (int_range 1 50) gen_operation
  in

  let fail_peek op_num time_ns time_delta expected current added actual diff =
    QCheck.Test.fail_reportf
      "Operation %d: peek failed\n\
      \  Time: %d ns (delta: %d ns)\n\
      \  Expected tokens: %.3f (current: %.3f + added: %.3f)\n\
      \  Actual tokens: %.3f\n\
      \  Diff: %.6f" op_num time_ns time_delta expected current added actual
      diff
  in

  let fail_consume op_num time_ns time_delta amount available success expected
      actual diff =
    QCheck.Test.fail_reportf
      "Operation %d: consume failed\n\
      \  Time: %d ns (delta: %d ns)\n\
      \  Consume amount: %.3f\n\
      \  Available before: %.3f\n\
      \  Success: %b\n\
      \  Expected after: %.3f\n\
      \  Actual after: %.3f\n\
      \  Diff: %.6f" op_num time_ns time_delta amount available success expected
      actual diff
  in

  let property (burst_size, fill_rate, operations) =
    let initial_time = Mtime.Span.of_uint64_ns 0L in
    let tb =
      Option.get
        (Token_bucket.create_with_timestamp initial_time ~burst_size ~fill_rate)
    in

    let rec check_operations op_num time_ns last_refill_ns current_tokens ops =
      match ops with
      | [] ->
          true
      | (consume_amount, time_delta_ns) :: rest ->
          let new_time_ns = time_ns + time_delta_ns in
          let current_time =
            Mtime.Span.of_uint64_ns (Int64.of_int new_time_ns)
          in
          let time_since_refill_seconds =
            float_of_int (new_time_ns - last_refill_ns) *. 1e-9
          in
          let tokens_added = time_since_refill_seconds *. fill_rate in
          let expected_available =
            min burst_size (current_tokens +. tokens_added)
          in
          let actual_before =
            Token_bucket.peek_with_timestamp current_time tb
          in
          let peek_diff = abs_float (actual_before -. expected_available) in

          if peek_diff >= 0.001 then
            fail_peek op_num new_time_ns time_delta_ns expected_available
              current_tokens tokens_added actual_before peek_diff
          else
            let success =
              Token_bucket.consume_with_timestamp
                (fun () -> current_time)
                tb consume_amount
            in
            let actual_after =
              Token_bucket.peek_with_timestamp current_time tb
            in
            let new_tokens =
              if success then
                expected_available -. consume_amount
              else
                expected_available
            in
            let after_diff = abs_float (actual_after -. new_tokens) in

            if after_diff >= 0.001 then
              fail_consume op_num new_time_ns time_delta_ns consume_amount
                expected_available success new_tokens actual_after after_diff
            else
              check_operations (op_num + 1) new_time_ns new_time_ns new_tokens
                rest
    in

    check_operations 1 0 0 burst_size operations
  in

  let gen_all =
    map3
      (fun burst fill ops -> (burst, fill, ops))
      pfloat (float_range 1e-9 1e9) gen_operations
  in

  let arb_all =
    QCheck.make
      ~print:(fun (burst, fill, ops) ->
        let ops_str =
          ops
          |> List.mapi (fun i (amount, delta) ->
                 Printf.sprintf "    Op %d: consume %.3f at +%d ns" (i + 1)
                   amount delta
             )
          |> String.concat "\n"
        in
        Printf.sprintf "burst_size=%.3f, fill_rate=%.3f, %d operations:\n%s"
          burst fill (List.length ops) ops_str
      )
      gen_all
  in

  QCheck.Test.make ~name:"Consume operations maintain correct token count"
    ~count:100 arb_all (fun (burst, fill, ops) -> property (burst, fill, ops)
  )

let test =
  [
    ( "A bucket with zero or negative fill rate cannot be created"
    , `Quick
    , test_bad_fill_rate
    )
  ; ( "Consume removes correct amount"
    , `Quick
    , test_consume_removes_correct_amount
    )
  ; ("Consume more than available", `Quick, test_consume_more_than_available)
  ; ( "Consume refills before removing"
    , `Quick
    , test_consume_refills_before_removing
    )
  ; ("Peek respects burst size", `Quick, test_peek_respects_burst_size)
  ; ("Concurrent access", `Quick, test_concurrent_access)
  ; ("Refill after sleep", `Slow, test_sleep)
  ; ("System time versions", `Quick, test_system_time_versions)
  ; ("Concurrent system time", `Quick, test_concurrent_system_time)
  ; ( "Consume more than available concurrent"
    , `Quick
    , test_consume_more_than_available_concurrent
    )
  ; ("Delay until available", `Quick, test_delay_until_available)
  ; ("Edge cases", `Quick, test_edge_cases)
  ; QCheck_alcotest.to_alcotest test_consume_quickcheck
  ]

let () = Alcotest.run "Token bucket library" [("Token bucket tests", test)]
