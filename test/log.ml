let ( let@ ) f x = f x

let output_log s = Printf.printf "%s\n%!" s

let log_backtrace_exn exn bt =
  (* We already got the backtrace in the `bt` argument when called from
     with_thread_associated. Log that, and remove `exn` from the backtraces
     table. If with_backtraces was not nested then looking at `bt` is the only
     way to get a proper backtrace, otherwise exiting from `with_backtraces`
     would've removed the backtrace from the thread-local backtraces table, and
     we'd always just log a message complaining about with_backtraces not being
     called, which is not true because it was.
  *)
  let bt' = Backtrace.remove exn in
  (* bt could be empty, but bt' would contain a non-empty warning, so compare 'bt' here *)
  let bt =
    if bt = Backtrace.empty then
      bt'
    else
      bt
  in
  let all = String.split_on_char '\n' Backtrace.(to_string_hum bt) in
  output_log (Printf.sprintf "Raised %s" (Printexc.to_string exn)) ;
  List.iter output_log all

let with_thread_associated desc f x =
  let result = 
    let@ () = Backtrace.with_backtraces in
    try f x with e -> Backtrace.is_important e ; raise e
  in
  match result with
  | `Ok result ->
      result
  | `Error (exn, bt) ->
      output_log
        (Printf.sprintf "%s failed with exception %s" desc
           (Printexc.to_string exn)
        ) ;
      log_backtrace_exn exn bt ;
      raise exn
