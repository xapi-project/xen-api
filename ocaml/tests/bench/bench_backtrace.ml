open Bechamel

let finally () = Sys.opaque_identity ()

let fun_protect f = Fun.protect ~finally f

let stdext_finally f = Xapi_stdext_pervasives.Pervasiveext.finally f finally

exception Test

let raise_local () = raise Test

let test wrapper f () = try wrapper (Sys.opaque_identity f) with Test -> ()

let test_backtrace_is_important () =
  try (Sys.opaque_identity raise_local) () with e -> Backtrace.is_important e

let benchmarks =
  [
    Test.make ~name:"Fun.protect noop" (Staged.stage @@ test fun_protect ignore)
  ; Test.make ~name:"finally noop" (Staged.stage @@ test stdext_finally ignore)
  ; Test.make ~name:"Fun.protect raise"
      (Staged.stage @@ test fun_protect raise_local)
  ; Test.make ~name:"finally raise"
      (Staged.stage @@ test stdext_finally raise_local)
  ; Test.make ~name:"Backtrace.is_important"
      (Staged.stage @@ test_backtrace_is_important)
  ]

let () =
  Printexc.record_backtrace true ;
  ()
  |> Debug.with_thread_associated "main" @@ fun () ->
     Bechamel_simple_cli.cli benchmarks
