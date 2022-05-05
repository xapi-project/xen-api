let ps_start = ref false

let scheduler_mutex = Mutex.create ()

let start_periodic_scheduler () =
  Mutex.lock scheduler_mutex ;
  ( if !ps_start then
      ()
  else
    let period = Mtime.Span.(1 * min) in
    let start = Mtime.Span.zero in
    Xapi_periodic_scheduler.add_to_queue "dummy"
      (Xapi_periodic_scheduler.Periodic period) start (fun () -> ()
    ) ;
    Xapi_event.register_hooks () ;
    ignore (Thread.create Xapi_periodic_scheduler.loop ()) ;
    ps_start := true
  ) ;
  Mutex.unlock scheduler_mutex

let event_setup_common () =
  start_periodic_scheduler () ;
  let __context = Test_common.make_test_database () in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;
  let session_id = Test_common.make_session ~__context () in
  let __context = Mock.Context.make ~session_id "session context" in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;
  (__context, session_id)
