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

let usage = Printf.sprintf "%s" Sys.argv.(0)
let speclist = [
  ("-no-backtraces", Arg.Set no_backtraces, "Test no-backtraces")
; ("-reraise", Arg.Set reraise, "Test reraise")
] 

let () =
  Arg.parse speclist (Fun.const ()) usage ;
  if !no_backtraces then test_no_backtraces () ;
  if !reraise then test_reraise ()
