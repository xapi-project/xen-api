open Locking_helpers

let test_kill_resource () =
  (* no-op *)
  kill_resource (lock "foo") ;

  Alcotest.check_raises "non-existent pid"
    (Unix.Unix_error (Unix.ESRCH, "kill", ""))
    (fun () ->
      (* non-existent pid, we cannot use max_int here as that will result in a negative overflow when converted to a C int *)
      kill_resource (process ("foo", 0x7FFF_FFFF))
    )

let test_is_process =
  [
    (lock "stunnel", false)
  ; (process ("stunnel", 100), true)
  ; (process ("other", 100), false)
  ]
  |> List.map @@ fun (resource, expected) ->
     let test () =
       Alcotest.(
         check' ~msg:"is_process" bool ~expected
           ~actual:(is_process "stunnel" resource)
       )
     in
     Alcotest.(test_case (string_of_resource resource) `Quick test)

let resource = Alcotest.testable (Fmt.of_to_string string_of_resource) ( = )

let main = Thread.id (Thread.self ())

let test_acquired_resources () =
  let r = lock "locktest" in
  let waiting = Thread_state.waiting_for r in
  let acquired = Thread_state.acquired r waiting in

  let lst = Thread_state.get_all_acquired_resources () in
  Alcotest.(
    check' (list resource) ~msg:"acquired resources" ~expected:[r] ~actual:lst
  ) ;

  let str = Thread_state.to_graphviz () in
  ( if Thread.id (Thread.self ()) = main then
      let expected =
        {|digraph Resources {
node [shape=Mrecord];
t0 [label="{} | {NULL} | {Lock(locktest) | 0}"];
node [shape=record];
r0 [style=filled label="{lock} | {locktest}"];
r0 -> t0
rankdir=LR
overlap=false
label="Threads and resources"
fontsize=12
}|}
      in
      Alcotest.(check' string ~msg:"graphviz" ~expected ~actual:str)
  ) ;

  Thread_state.released r acquired ;
  let lst = Thread_state.get_all_acquired_resources () in
  Alcotest.(
    check' (list resource) ~msg:"acquired resources (released)" ~expected:[]
      ~actual:lst
  )

let test_single_task () =
  (* cannot create it within the test since it'll be considered a leak *)
  let __context = Test_common.make_test_database () in

  let rpc, session_id = Test_common.make_client_params ~__context in
  let self =
    Client.Client.Task.create ~rpc ~session_id ~label:"task_label"
      ~description:"task_description"
  in
  let r = lock "bar" in
  Thread_state.with_named_thread "myname" self (fun () ->
      let waiting = Thread_state.waiting_for r in
      let acquired = Thread_state.acquired r waiting in

      let lst = Thread_state.get_acquired_resources_by_task self in
      Alcotest.(
        check' (list resource) ~msg:"acquired resources" ~expected:[r]
          ~actual:lst
      ) ;

      Thread_state.released r acquired ;
      let lst = Thread_state.get_acquired_resources_by_task self in
      Alcotest.(
        check' (list resource) ~msg:"acquired resources (0)" ~expected:[]
          ~actual:lst
      )
  ) ;
  Client.Client.Task.destroy ~rpc ~session_id ~self ;
  Db_gc.single_pass ()

let test_named_mutex_simple () =
  let name = "mytestmutex" in
  let r = lock name in
  let m = Named_mutex.create name in
  Named_mutex.execute m (fun () ->
      let lst = Thread_state.get_all_acquired_resources () in
      Alcotest.(
        check' (list resource) ~msg:"acquired resources (mutex)" ~expected:[r]
          ~actual:lst
      )
  ) ;
  let lst = Thread_state.get_all_acquired_resources () in
  Alcotest.(
    check' (list resource) ~msg:"acquired resources (mutex)" ~expected:[]
      ~actual:lst
  )

let test_named_mutex_finally () =
  let name = "mytestmutex2" in
  let r = lock name in
  let m = Named_mutex.create name in
  Alcotest.check_raises "exit" Exit (fun () ->
      Named_mutex.execute m (fun () ->
          let lst = Thread_state.get_all_acquired_resources () in
          Alcotest.(
            check' (list resource) ~msg:"acquired resources (mutex)"
              ~expected:[r] ~actual:lst
          ) ;
          raise Exit
      )
  ) ;
  let lst = Thread_state.get_all_acquired_resources () in
  Alcotest.(
    check' (list resource) ~msg:"acquired resources (mutex)" ~expected:[]
      ~actual:lst
  ) ;
  (* check that mutex got released, no deadlock *)
  Named_mutex.execute m ignore

module ThreadWrap = struct
  type t = {
      thread: Thread.t
    ; failure: (Printexc.raw_backtrace * exn) option Atomic.t
  }

  let create f arg =
    let failure = Atomic.make None in
    let wrap f =
      try f arg
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        Atomic.set failure (Some (bt, e))
    in
    {thread= Thread.create wrap f; failure}

  let join t =
    Thread.join t.thread ;
    match Atomic.get t.failure with
    | None ->
        ()
    | Some (bt, e) ->
        Printexc.raise_with_backtrace e bt
end

let other_thread f () =
  let thr = ThreadWrap.create f () in
  ThreadWrap.join thr

let full_gc () =
  (* see comment in Gc module: finalisers may allocate and need a 2nd run *)
  Gc.full_major () ; Gc.compact ()

let no_table_leak f () =
  (* could run 2 full_major, look at live words, but that is difficult to get working reliably across runtime versions
     (sometimes the allocated memory goes negative, e.g. if some global gets freed)
  *)
  full_gc () ;
  let before = Thread_state.known_threads () in
  f () ;
  full_gc () ;
  let after = Thread_state.known_threads () in
  Alcotest.(
    check' int ~msg:"leaked thread table entry" ~expected:before ~actual:after
  )

let test_named_mutex_many i =
  let name = "mytestmutex" ^ string_of_int i in
  let r = lock name in
  let m = Named_mutex.create name in
  Named_mutex.execute m (fun () ->
      let lst =
        Thread_state.get_all_acquired_resources () |> List.filter (( = ) r)
      in
      Alcotest.(
        check' (list resource) ~msg:"acquired resources (mutex)" ~expected:[r]
          ~actual:lst
      )
  ) ;
  let lst =
    Thread_state.get_all_acquired_resources () |> List.filter (( = ) r)
  in
  Alcotest.(
    check' (list resource) ~msg:"acquired resources (mutex)" ~expected:[]
      ~actual:lst
  )

let shared_mutex = Named_mutex.create "shared"

let shared_owner = Atomic.make (-1)

let holders = Atomic.make 0

let test_named_mutex_many_same j =
  for i = 1 to 1000 do
    Named_mutex.execute shared_mutex (fun () ->
        let old_holders = Atomic.fetch_and_add holders 1 in
        let actual = Atomic.exchange shared_owner j in
        if actual <> -1 then
          Fmt.failwith
            "Shared owner already set to: %d (I am %d). Old holders = %d" actual
            j old_holders ;

        if i mod 10 = 0 then Thread.yield () ;

        (* try to introduce more race conditions while holding the lock *)
        if not (Atomic.compare_and_set shared_owner j (-1)) then
          Fmt.failwith "Failed to restore shared owner" ;
        Atomic.decr holders ;

        if old_holders <> 0 then
          Fmt.failwith
            "Only one thread should be able to acquire the mutex at a time: %d"
            old_holders
    )
  done

let many_threads f () =
  let waiting = Atomic.make 0 in
  let n = 64 in
  let test_thread i =
    (* wait for all threads to start, to maximize race conditions *)
    Atomic.incr waiting ;
    while Atomic.get waiting <> n do
      Thread.yield ()
    done ;

    f i
  in

  let threads = Array.init n @@ ThreadWrap.create test_thread in
  Array.iter ThreadWrap.join threads

let () =
  Suite_init.harness_init () ;
  Alcotest.(
    run "Locking_helpers"
      [
        ("is_process", test_is_process)
      ; ("kill_resource", [test_case "kill_resource" `Quick test_kill_resource])
      ; ( "acquired resources"
        , [
            test_case "single thread" `Quick
            @@ no_table_leak test_acquired_resources
          ; test_case "single task" `Quick @@ no_table_leak test_single_task
          ; test_case "single thread (other)" `Quick
              (no_table_leak @@ other_thread test_acquired_resources)
          ; test_case "single task (other)" `Quick
              (no_table_leak @@ other_thread test_single_task)
          ]
        )
      ; ( "named mutex"
        , [
            test_case "without tracing" `Quick
            @@ no_table_leak test_named_mutex_simple
          ; test_case "finally" `Quick @@ no_table_leak test_named_mutex_finally
          ; test_case "race (64 threads)" `Slow
              (no_table_leak @@ many_threads test_named_mutex_many)
          ; test_case "race same (64 threads)" `Slow
              (no_table_leak @@ many_threads test_named_mutex_many_same)
          ; test_case "GC finalizer" `Quick
              (no_table_leak @@ Locking_helpers.Named_mutex.Private.test_locking)
          ]
        )
      ]
  )
