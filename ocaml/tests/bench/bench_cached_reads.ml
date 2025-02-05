open Bechamel

let run () =
  let _ : bool = Sys.opaque_identity (Pool_role.is_master ()) in
  ()

let mutex_workload =
  Bechamel_simple_cli.thread_workload ~before:ignore ~after:ignore ~run

let free_threads (stop, t) = Atomic.set stop true ; Array.iter Thread.join t

let benchmarks =
  Test.make_grouped ~name:"Cached reads"
    [Test.make ~name:"Pool_role.is_master" (Staged.stage Pool_role.is_master)]

let () = Bechamel_simple_cli.cli ~workloads:[mutex_workload] benchmarks
