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
    let tg_cli = Tgroup.ThreadGroup.create ~tgroup:g_cli in
    let () = tg_cli.thread_count <- Atomic.make 10 in
    let tg_authenticated_root =
      Tgroup.ThreadGroup.create ~tgroup:Tgroup.Group.authenticated_root
    in
    let () = tg_authenticated_root.thread_count <- Atomic.make 5 in
    Tgroup.ThreadGroup.(add tg_cli ; add tg_authenticated_root)
  in
  let free = Tgroup.ThreadGroup.destroy in
  Test.make_with_resource ~name ~allocate ~free Test.uniq f

let test_with_thread_classified ~name f =
  let allocate () =
    let () = Atomic.set Tgroup.Cgroup.cgroup_dir (Some "") in
    let g_cli = Some "cli" |> Tgroup.of_req_originator |> Option.get in
    let tg_cli = Tgroup.ThreadGroup.create ~tgroup:g_cli in
    let () = tg_cli.thread_count <- Atomic.make 10 in
    let tg_authenticated_root =
      Tgroup.ThreadGroup.create ~tgroup:Tgroup.Group.authenticated_root
    in
    let () = tg_authenticated_root.thread_count <- Atomic.make 1 in
    Xapi_stdext_threads.Threadext.ThreadRuntimeContext.(
      let thread_ctx = get () in
      update
        (fun thread_ctx ->
          {thread_ctx with tgroup= Tgroup.Group.authenticated_root}
        )
        thread_ctx
    ) ;
    Tgroup.ThreadGroup.(add tg_cli ; add tg_authenticated_root)
  in
  let free () =
    Tgroup.ThreadGroup.destroy () ;
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
