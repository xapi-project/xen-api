let ps_start = ref false

let scheduler_mutex = Mutex.create ()

module Scheduler = Xapi_stdext_threads_scheduler.Scheduler

let start_periodic_scheduler () =
  Mutex.lock scheduler_mutex ;
  if !ps_start then
    ()
  else (
    Scheduler.add_to_queue "dummy" (Scheduler.Periodic 60.0) 0.0 (fun () -> ()) ;
    Xapi_event.register_hooks () ;
    ignore (Thread.create Scheduler.loop ()) ;
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
