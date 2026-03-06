let foo () : unit = raise (Failure "foo")

let bar () =
  try foo () with Failure _ as e -> Backtrace.reraise e (Failure "bar")

let baz () = try foo () with exn -> raise exn

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

let raise_again = ref false

let test_raise_again () =
  Printexc.record_backtrace true ;
  try Test_lib.Log.with_thread_associated "Backtrace lab" baz () with _ -> ()

let usage = Printf.sprintf "%s" Sys.argv.(0)
let speclist = [
  ("-no-backtraces", Arg.Set no_backtraces, "Test no-backtraces")
; ("-reraise", Arg.Set reraise, "Test reraise")
; ("-v1-with-backtrace", Arg.Set v1_with_backtrace, "Test v1-with-backtrace")
; ("-raise-again", Arg.Set raise_again, "Test raise-again")
] 

let () =
  Arg.parse speclist (Fun.const ()) usage ;
  if !no_backtraces then test_no_backtraces () ;
  if !reraise then test_reraise () ;
  if !v1_with_backtrace then test_v1_with_backtrace () ;
  if !raise_again then test_raise_again ()
