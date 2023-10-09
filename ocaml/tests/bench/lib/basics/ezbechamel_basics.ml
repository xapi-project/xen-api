open Bechamel

external bench_fixed_work : int -> int = "caml_bench_fixed_work"

(* Performs a fixed amount of work, see also the comment in the C stub.
 
  We could calibrate how long this takes on a given system and adjust loop count accordingly, but that means
   that when rerunning the executable we'll get slightly different numbers (e.g. due to unpredictability of time slices inside a VM, etc.)
  Use a fixed number here, the actual value doesn't matter much as long as the time is reasonably short (e.g. on the order of 10ms, comparable to PAM) and deterministic.
  This will change with different compilers, or when run on different CPU architectures, but for a given binary and system it should perform a fixed amount of work.

  The goal is to use this fixed amount of work to evaluate how well OCaml (4.x) is able to dispatch parallel work to C functions, and
  how much intereference there is from the master lock.
*)
let parallel_c_work () =
  let (_ : int) = Sys.opaque_identity @@ bench_fixed_work @@ 10_000_000 in
  ()

let () =
  Ezbechamel_alcotest_notty.run [
    Test.make ~name:"overhead" (Staged.stage ignore)
  ; Test.make ~name:"fixedwork" (Staged.stage parallel_c_work) 
  ]