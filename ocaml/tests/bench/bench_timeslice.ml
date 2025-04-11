open Bechamel

let test_maybe_thread_yield () =
  Sys.opaque_identity
    (Xapi_timeslice.Timeslice.Runtime.maybe_thread_yield
       ~global_slice_period:10_000_000
    )

let test_sched_global_slice () =
  Sys.opaque_identity
    (Xapi_timeslice.Timeslice.Runtime.sched_global_slice
       ~global_slice_period:10_000_000
    )

let test_tgroups_on ~name f =
  let allocate () =
    let () = Atomic.set Tgroup.Cgroup.cgroup_dir (Some "") in
    let g_cli = Some "cli" |> Tgroup.of_req_originator |> Option.get in
    let () = Tgroup.add g_cli in
    let tg_cli = Tgroup.group_of_description g_cli |> Option.get in
    let _ = Atomic.fetch_and_add tg_cli.thread_count 10 in
    let () = Tgroup.add Tgroup.Description.authenticated_root in
    let tg_authenticated_root =
      Tgroup.group_of_description Tgroup.Description.authenticated_root
      |> Option.get
    in
    let _ = Atomic.fetch_and_add tg_authenticated_root.thread_count 5 in
    ()
  in
  let free = Tgroup.destroy in
  Test.make_with_resource ~name ~allocate ~free Test.uniq f

let test_with_thread_classified ~name f =
  let allocate () =
    let () = Atomic.set Tgroup.Cgroup.cgroup_dir (Some "") in
    let g_cli = Some "cli" |> Tgroup.of_req_originator |> Option.get in
    let () = Tgroup.add g_cli in
    let tg_cli = Tgroup.group_of_description g_cli |> Option.get in
    let _ = Atomic.fetch_and_add tg_cli.thread_count 10 in
    let () = Tgroup.add Tgroup.Description.authenticated_root in
    let tg_authenticated_root =
      Tgroup.group_of_description Tgroup.Description.authenticated_root
      |> Option.get
    in
    let () = Atomic.incr tg_authenticated_root.thread_count in
    Xapi_stdext_threads.Threadext.ThreadRuntimeContext.(
      let thread_ctx = get () in
      update
        (fun thread_ctx ->
          {thread_ctx with tgroup= Tgroup.Description.authenticated_root}
        )
        thread_ctx
    )
  in
  let free () =
    Tgroup.destroy () ;
    Xapi_stdext_threads.Threadext.ThreadRuntimeContext.remove ()
  in
  Test.make_with_resource ~name ~allocate ~free Test.uniq f

let benchmarks =
  Test.make_grouped ~name:"timeslice"
    [
      test_with_thread_classified ~name:"maybe_thread_yield"
        (Staged.stage test_maybe_thread_yield)
    ; test_tgroups_on ~name:"sched_global_slice"
        (Staged.stage test_sched_global_slice)
    ]

let () = Bechamel_simple_cli.cli benchmarks
