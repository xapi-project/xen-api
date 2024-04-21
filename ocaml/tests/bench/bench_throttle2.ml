open Bechamel

let () =
  Suite_init.harness_init () ;
  Debug.set_level Syslog.Warning

let __context, _ = Test_event_common.event_setup_common ()

let allocate_tasks n =
  ( __context
  , Array.init n @@ fun i ->
    let label = Printf.sprintf "task %d" i in
    Xapi_task.create ~__context ~label ~description:"test task"
  )

let free_tasks (__context, tasks) =
  let () =
    tasks |> Array.iter @@ fun self -> Xapi_task.destroy ~__context ~self
  in
  ()

let set_pending tasks =
  tasks
  |> Array.iter @@ fun self ->
     Xapi_task.set_status ~__context ~self ~value:`pending

let run_tasks _n (__context, tasks) =
  set_pending tasks ;
  let () =
    tasks
    |> Array.iter @@ fun self ->
       Xapi_task.set_status ~__context ~self ~value:`success
  in
  tasks |> Array.iter @@ fun t -> Helpers.Task.wait_for ~__context ~tasks:[t]

let run_tasks' _n (__context, tasks) =
  set_pending tasks ;
  let () =
    tasks
    |> Array.iter @@ fun self ->
       Xapi_task.set_status ~__context ~self ~value:`success
  in
  Helpers.Task.wait_for ~__context ~tasks:(Array.to_list tasks)

module D = Debug.Make (struct let name = __MODULE__ end)

let run_tasks'' n (__context, tasks) =
  set_pending tasks ;
  let finished = Atomic.make 0 in
  let (t : Thread.t) =
    Thread.create
      (fun () ->
        for _ = 1 to 10 do
          Thread.yield ()
        done ;
        tasks
        |> Array.iter @@ fun self ->
           Xapi_task.set_status ~__context ~self ~value:`success ;
           Atomic.incr finished
      )
      ()
  in
  Helpers.Task.wait_for ~__context ~tasks:(Array.to_list tasks) ;
  let f = Atomic.get finished in
  assert (f = n || f = n - 1) ;
  Thread.join t

let benchmarks =
  Test.make_grouped ~name:"Task latency"
    [
      Test.make_indexed_with_resource ~name:"task complete+wait latency"
        ~args:[1; 10; 100] Test.multiple ~allocate:allocate_tasks
        ~free:free_tasks (fun n -> Staged.stage (run_tasks n)
      )
    ; Test.make_indexed_with_resource ~name:"task complete+wait all latency"
        ~args:[1; 10; 100] Test.multiple ~allocate:allocate_tasks
        ~free:free_tasks (fun n -> Staged.stage (run_tasks' n)
      )
    ; Test.make_indexed_with_resource
        ~name:"task complete+wait all latency (thread)" ~args:[1; 10; 100]
        Test.multiple ~allocate:allocate_tasks ~free:free_tasks (fun n ->
          Staged.stage (run_tasks'' n)
      )
    ]

let () = Bechamel_simple_cli.cli benchmarks
