let foo () : unit = raise (Failure "foo")

let bar () =
  try foo () with Failure _ as e -> Backtrace.reraise e (Failure "bar")


let no_backtraces = ref false

let test_no_backtraces () =
  Printexc.record_backtrace false ;
  try Test_lib.Log.with_thread_associated "Backtrace lab" foo () with _ -> ()

let reraise = ref false

let test_reraise () =
  Printexc.record_backtrace true ;
  try Test_lib.Log.with_thread_associated "Backtrace lab" bar () with _ -> ()

let v1_with_backtrace = ref false

let test_v1_with_backtrace () =
  Printexc.record_backtrace false ;
  try Test_lib.Log.with_thread_associated_old "Backtrace lab" foo () with _ -> ()

let usage = Printf.sprintf "%s" Sys.argv.(0)
let speclist = [
  ("-no-backtraces", Arg.Set no_backtraces, "Test no-backtraces")
; ("-reraise", Arg.Set reraise, "Test reraise")
; ("-v1-with-backtrace", Arg.Set v1_with_backtrace, "Test v1-with-backtrace")
] 

let () =
  Arg.parse speclist (Fun.const ()) usage ;
  if !no_backtraces then test_no_backtraces () ;
  if !reraise then test_reraise () ;
  if !v1_with_backtrace then test_v1_with_backtrace ()
