open Rate_limit

let test_create () =
  let table = Bucket_table.create () in
  Alcotest.(check (option (float 0.0)))
    "Empty table returns None for peek" None
    (Bucket_table.peek table ~user_agent:"test")

let test_add_bucket () =
  let table = Bucket_table.create () in
  let success =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:2.0
  in
  Alcotest.(check bool) "Adding valid bucket should succeed" true success ;
  Alcotest.(check (option (float 0.1)))
    "Peek should return burst_size" (Some 10.0)
    (Bucket_table.peek table ~user_agent:"agent1")

let test_add_bucket_invalid () =
  let table = Bucket_table.create () in
  let success =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:0.0
  in
  Alcotest.(check bool)
    "Adding bucket with zero fill rate should fail" false success ;
  let success_neg =
    Bucket_table.add_bucket table ~user_agent:"agent2" ~burst_size:10.0
      ~fill_rate:(-1.0)
  in
  Alcotest.(check bool)
    "Adding bucket with negative fill rate should fail" false success_neg

let test_delete_bucket () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:2.0
  in
  Alcotest.(check (option (float 0.1)))
    "Bucket exists before delete" (Some 10.0)
    (Bucket_table.peek table ~user_agent:"agent1") ;
  Bucket_table.delete_bucket table ~user_agent:"agent1" ;
  Alcotest.(check (option (float 0.0)))
    "Bucket removed after delete" None
    (Bucket_table.peek table ~user_agent:"agent1")

let test_delete_nonexistent () =
  let table = Bucket_table.create () in
  Bucket_table.delete_bucket table ~user_agent:"nonexistent" ;
  Alcotest.(check pass) "Deleting nonexistent bucket should not raise" () ()

let test_try_consume () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:2.0
  in
  let success = Bucket_table.try_consume table ~user_agent:"agent1" 3.0 in
  Alcotest.(check bool) "Consuming available tokens should succeed" true success ;
  Alcotest.(check (option (float 0.1)))
    "Tokens reduced after consume" (Some 7.0)
    (Bucket_table.peek table ~user_agent:"agent1")

let test_try_consume_insufficient () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:5.0
      ~fill_rate:1.0
  in
  let success = Bucket_table.try_consume table ~user_agent:"agent1" 10.0 in
  Alcotest.(check bool)
    "Consuming more than available should fail" false success ;
  Alcotest.(check (option (float 0.1)))
    "Tokens unchanged after failed consume" (Some 5.0)
    (Bucket_table.peek table ~user_agent:"agent1")

let test_try_consume_nonexistent () =
  let table = Bucket_table.create () in
  let success = Bucket_table.try_consume table ~user_agent:"nonexistent" 1.0 in
  Alcotest.(check bool)
    "Consuming from nonexistent bucket should fail" false success

let test_peek_nonexistent () =
  let table = Bucket_table.create () in
  Alcotest.(check (option (float 0.0)))
    "Peek nonexistent bucket returns None" None
    (Bucket_table.peek table ~user_agent:"nonexistent")

let test_multiple_agents () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:2.0
  in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent2" ~burst_size:20.0
      ~fill_rate:5.0
  in
  let _ = Bucket_table.try_consume table ~user_agent:"agent1" 5.0 in
  Alcotest.(check (option (float 0.1)))
    "Agent1 tokens reduced" (Some 5.0)
    (Bucket_table.peek table ~user_agent:"agent1") ;
  Alcotest.(check (option (float 0.1)))
    "Agent2 tokens unchanged" (Some 20.0)
    (Bucket_table.peek table ~user_agent:"agent2")

let test_consume_and_block () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:10.0
  in
  let _ = Bucket_table.try_consume table ~user_agent:"agent1" 10.0 in
  let start_counter = Mtime_clock.counter () in
  Bucket_table.consume_and_block table ~user_agent:"agent1" 5.0 ;
  let elapsed_span = Mtime_clock.count start_counter in
  let elapsed_seconds = Mtime.Span.to_float_ns elapsed_span *. 1e-9 in
  Alcotest.(check (float 0.1))
    "consume_and_block should wait for tokens" elapsed_seconds 0.5

let test_consume_and_block_nonexistent () =
  let table = Bucket_table.create () in
  Bucket_table.consume_and_block table ~user_agent:"nonexistent" 1.0 ;
  Alcotest.(check pass)
    "consume_and_block on nonexistent bucket should not block" () ()

let test_concurrent_access () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:100.0
      ~fill_rate:0.01
  in
  let successful_consumes = ref 0 in
  let counter_mutex = Mutex.create () in
  let threads =
    Array.init 20 (fun _ ->
        Thread.create
          (fun () ->
            let success =
              Bucket_table.try_consume table ~user_agent:"agent1" 5.0
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
    "Exactly 20 consumes should succeed" 20 !successful_consumes

let test =
  [
    ("Create empty table", `Quick, test_create)
  ; ("Add valid bucket", `Quick, test_add_bucket)
  ; ("Add invalid bucket", `Quick, test_add_bucket_invalid)
  ; ("Delete bucket", `Quick, test_delete_bucket)
  ; ("Delete nonexistent bucket", `Quick, test_delete_nonexistent)
  ; ("Try consume", `Quick, test_try_consume)
  ; ("Try consume insufficient", `Quick, test_try_consume_insufficient)
  ; ("Try consume nonexistent", `Quick, test_try_consume_nonexistent)
  ; ("Peek nonexistent", `Quick, test_peek_nonexistent)
  ; ("Multiple agents", `Quick, test_multiple_agents)
  ; ("Consume and block", `Slow, test_consume_and_block)
  ; ("Consume and block nonexistent", `Quick, test_consume_and_block_nonexistent)
  ; ("Concurrent access", `Quick, test_concurrent_access)
  ]

let () = Alcotest.run "Bucket table library" [("Bucket table tests", test)]
