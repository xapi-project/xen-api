type 'a wrapped = ('a, Rresult.R.exn_trap) result

type (-'a, 'b) task = {f: 'a -> 'b; result: 'b wrapped Event.channel}

type ('a, 'b) t = {
    stop_all: unit -> unit
  ; tasks: ('a, 'b) task option Event.channel
}

let create ~name:_ acquire release workers =
  let tasks = Event.new_channel () in

  let rec worker_loop resource () =
    match Event.sync (Event.receive tasks) with
    | None ->
        () (* stop *)
    | Some task ->
        let result = Rresult.R.trap_exn task.f resource in
        Event.sync (Event.send task.result result) ;
        worker_loop resource ()
  in

  let worker _ =
    let resource = acquire () in
    let finally () = release resource in
    Fun.protect ~finally @@ worker_loop resource
  in
  let threads = Array.init workers @@ fun idx -> Thread.create worker idx in

  let stop_all () =
    (* queue up an abort job for each worker for a graceful shutdown *)
    for _ = 1 to workers do
      Event.(sync @@ send tasks None);
    done;
    Array.iter Thread.join threads
  
  in
  {stop_all; tasks}

let run_in_pool' pool f =
  let result = Event.new_channel () in
  Event.sync (Event.send pool.tasks (Some {f; result})) ;
  let wait () =
    match Event.sync (Event.receive result) with
    | Ok r ->
        r
    | Error (`Exn_trap (e, bt)) ->
        Printexc.raise_with_backtrace e bt
  in
  wait

let run_in_pool pool f = run_in_pool' pool f ()

let shutdown t = t.stop_all ()

(*let mutex_execute = Xapi_stdext_threads.Threadext.Mutex.execute


  type 'a queue = {
      tasks: ('a -> unit) Queue.t
    ; mutex: Mutex.t (* TODO: named mutex? *)
    ; cond: Condition.t
    ; cond_task_finished: Condition.t
    ; mutable shutdown: bool
    ; name: string
  }

  type 'a t = 'a queue * Thread.t array

  let rec wait_for_next_task pool =
    match Queue.take_opt pool.tasks with
    | None ->
        Condition.wait pool.cond pool.mutex ;
        if pool.shutdown then
          raise Thread.Exit ;
        wait_for_next_task pool
    | Some task ->
        task

  let create ~name acquire release workers =
    let t =
      {
        mutex= Mutex.create ()
      ; cond= Condition.create ()
      ; cond_task_finished= Condition.create ()
      ; tasks= Queue.create ()
      ; shutdown= false
      ; name
      }
    in
    let worker idx =
      let name = Printf.sprintf "%s worker #%d" name idx in
      ()
      |> Debug.with_thread_named name @@ fun () ->
         let resource = acquire () in
         D.debug "%s: acquired resource" name ;
         let finally () =
           D.debug "%s: releasing resource" name ;
           release resource ;
           D.debug "%s: released resource" name
         in
         Fun.protect ~finally @@ fun () ->
         while not t.shutdown do
           let task = mutex_execute t.mutex @@ fun () -> wait_for_next_task t in
           (* fast-path *)
           D.log_and_ignore_exn (fun () -> task resource)
          done
    in
    let workers = Array.init workers (Thread.create worker) in
    (t, workers)

  let run_in_pool' (pool, _) f =
    let result = ref None in
    (* TODO: might get better perf if we have a round-robin of auth workers and each with its own finished cond signaling.
       But we might get stuck behind a slow call that throttles
    *)
    let f r =
      result := Some (Rresult.R.trap_exn f r) ;
      (* fixme: thundering herd *)
      Condition.broadcast pool.cond_task_finished
    in
    mutex_execute pool.mutex @@ fun () ->
    Queue.push f pool.tasks ;
    Condition.signal pool.cond ;

    let rec wait () =
      match !result with
      | None ->
          Condition.wait pool.cond_task_finished pool.mutex ;
          wait ()
      | Some (Ok r) ->
          r
      | Some (Error (`Exn_trap (e, bt))) ->
          Printexc.raise_with_backtrace e bt
    in
    let wait () =
      mutex_execute pool.mutex wait
    in
    wait

  let run_in_pool t f =
    run_in_pool' t f ()

  let shutdown (pool, workers) =
    D.debug "Shutting down %s worker pool" pool.name ;
    let () = mutex_execute pool.mutex @@ fun () ->
      pool.shutdown <- true ;
      Condition.broadcast pool.cond ;
    in
    D.debug "Waiting for %s worker pool to exit" pool.name ;
    Array.iter Thread.join workers ;
    D.debug "Worker pool %s has exited" pool.name
*)
