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

let test_submit () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:10.0
  in
  let _ = Bucket_table.try_consume table ~user_agent:"agent1" 10.0 in
  let executed = ref false in
  let start_counter = Mtime_clock.counter () in
  Bucket_table.submit table ~user_agent:"agent1"
    ~callback:(fun () -> executed := true)
    5.0 ;
  let elapsed_span = Mtime_clock.count start_counter in
  let elapsed_seconds = Mtime.Span.to_float_ns elapsed_span *. 1e-9 in
  (* submit should return immediately (non-blocking) *)
  Alcotest.(check bool) "submit returns immediately" true (elapsed_seconds < 0.1) ;
  (* Wait for callback to be executed by worker *)
  Thread.delay 0.6 ;
  Alcotest.(check bool) "callback eventually executed" true !executed

let test_submit_nonexistent () =
  let table = Bucket_table.create () in
  let executed = ref false in
  Bucket_table.submit table ~user_agent:"nonexistent"
    ~callback:(fun () -> executed := true)
    1.0 ;
  Alcotest.(check bool)
    "submit on nonexistent bucket runs callback immediately" true !executed

let test_submit_fairness () =
  (* Test that callbacks are executed in FIFO order regardless of token cost *)
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:5.0
      ~fill_rate:5.0
  in
  (* Drain the bucket *)
  let _ = Bucket_table.try_consume table ~user_agent:"agent1" 5.0 in
  let execution_order = ref [] in
  let order_mutex = Mutex.create () in
  let record_execution id =
    Mutex.lock order_mutex ;
    execution_order := id :: !execution_order ;
    Mutex.unlock order_mutex
  in
  (* Submit callbacks with varying costs - order should be preserved *)
  Bucket_table.submit table ~user_agent:"agent1"
    ~callback:(fun () -> record_execution 1)
    1.0 ;
  Bucket_table.submit table ~user_agent:"agent1"
    ~callback:(fun () -> record_execution 2)
    3.0 ;
  Bucket_table.submit table ~user_agent:"agent1"
    ~callback:(fun () -> record_execution 3)
    1.0 ;
  Bucket_table.submit table ~user_agent:"agent1"
    ~callback:(fun () -> record_execution 4)
    2.0 ;
  (* Wait for all callbacks to complete (total cost = 7 tokens, rate = 5/s) *)
  Thread.delay 2.0 ;
  let order = List.rev !execution_order in
  Alcotest.(check (list int))
    "callbacks execute in FIFO order" [1; 2; 3; 4] order

let test_submit_sync () =
  let table = Bucket_table.create () in
  let _ =
    Bucket_table.add_bucket table ~user_agent:"agent1" ~burst_size:10.0
      ~fill_rate:10.0
  in
  (* Test 1: Returns callback result immediately when tokens available *)
  let result =
    Bucket_table.submit_sync table ~user_agent:"agent1"
      ~callback:(fun () -> 42)
      5.0
  in
  Alcotest.(check int) "returns callback result" 42 result ;
  (* Test 2: Blocks and waits for tokens, then returns result *)
  let _ = Bucket_table.try_consume table ~user_agent:"agent1" 5.0 in
  (* drain bucket *)
  let start_counter = Mtime_clock.counter () in
  let result2 =
    Bucket_table.submit_sync table ~user_agent:"agent1"
      ~callback:(fun () -> "hello")
      5.0
  in
  let elapsed_span = Mtime_clock.count start_counter in
  let elapsed_seconds = Mtime.Span.to_float_ns elapsed_span *. 1e-9 in
  Alcotest.(check string) "returns string result" "hello" result2 ;
  Alcotest.(check bool)
    "blocked waiting for tokens" true (elapsed_seconds >= 0.4)

let test_add_same_key_race () =
  (* Test the check-then-act race in add_bucket.
     add_bucket does: if not mem then add. Without locking, multiple threads
     could all pass the mem check and try to add, but only one should succeed.
     Note: OCaml 4's GIL makes races hard to trigger, but this test verifies
     the invariant holds under concurrent access and would catch races if the
     GIL is released at allocation points within the critical section. *)
  let iterations = 500 in
  let threads_per_iter = 10 in
  let failures = ref 0 in
  let failures_mutex = Mutex.create () in
  for _ = 1 to iterations do
    let table = Bucket_table.create () in
    let success_count = ref 0 in
    let count_mutex = Mutex.create () in
    let barrier = ref 0 in
    let barrier_mutex = Mutex.create () in
    let threads =
      Array.init threads_per_iter (fun _ ->
          Thread.create
            (fun () ->
              (* Increment barrier and wait for all threads *)
              Mutex.lock barrier_mutex ;
              incr barrier ;
              Mutex.unlock barrier_mutex ;
              while
                Mutex.lock barrier_mutex ;
                let b = !barrier in
                Mutex.unlock barrier_mutex ; b < threads_per_iter
              do
                Thread.yield ()
              done ;
              (* All threads try to add the same key simultaneously *)
              let success =
                Bucket_table.add_bucket table ~user_agent:"contested_key"
                  ~burst_size:10.0 ~fill_rate:1.0
              in
              if success then (
                Mutex.lock count_mutex ;
                incr success_count ;
                Mutex.unlock count_mutex
              )
            )
            ()
      )
    in
    Array.iter Thread.join threads ;
    (* Exactly one thread should succeed in adding the key *)
    if !success_count <> 1 then (
      Mutex.lock failures_mutex ;
      incr failures ;
      Mutex.unlock failures_mutex
    )
  done ;
  Alcotest.(check int)
    "Exactly one add should succeed for same key (across all iterations)" 0
    !failures

let test_concurrent_add_delete_stress () =
  (* Stress test: rapidly add and delete entries.
     Without proper locking, hashtable can get corrupted. *)
  let table = Bucket_table.create () in
  let iterations = 1000 in
  let num_keys = 10 in
  let errors = ref 0 in
  let errors_mutex = Mutex.create () in
  let add_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let key =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              let _ =
                Bucket_table.add_bucket table ~user_agent:key ~burst_size:10.0
                  ~fill_rate:1.0
              in
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
              let key =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              Bucket_table.delete_bucket table ~user_agent:key
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
              let key =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              (* This should never crash, even if key doesn't exist *)
              try
                let _ = Bucket_table.peek table ~user_agent:key in
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

let test_consume_during_delete_race () =
  (* Test that try_consume doesn't crash when bucket is being deleted.
     Without proper locking, we could try to access a deleted bucket. *)
  let iterations = 500 in
  let errors = ref 0 in
  let errors_mutex = Mutex.create () in
  for _ = 1 to iterations do
    let table = Bucket_table.create () in
    let _ =
      Bucket_table.add_bucket table ~user_agent:"target" ~burst_size:100.0
        ~fill_rate:1.0
    in
    let barrier = ref 0 in
    let barrier_mutex = Mutex.create () in
    let consumer =
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
            let _ = Bucket_table.try_consume table ~user_agent:"target" 1.0 in
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
          Bucket_table.delete_bucket table ~user_agent:"target"
        )
        ()
    in
    Thread.join consumer ; Thread.join deleter
  done ;
  Alcotest.(check int) "No crashes during consume/delete race" 0 !errors

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
  ; ("Submit", `Slow, test_submit)
  ; ("Submit nonexistent", `Quick, test_submit_nonexistent)
  ; ("Submit fairness", `Slow, test_submit_fairness)
  ; ("Submit sync", `Slow, test_submit_sync)
  ; ("Add same key race", `Quick, test_add_same_key_race)
  ; ("Concurrent add/delete stress", `Quick, test_concurrent_add_delete_stress)
  ; ("Consume during delete race", `Quick, test_consume_during_delete_race)
  ]

let () = Alcotest.run "Bucket table library" [("Bucket table tests", test)]
