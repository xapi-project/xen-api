module D = Debug.Make (struct let name = Filename.basename __FILE__ end)

let m = Mutex.create ()

let a = [||]

let buggy () = a.(1) <- 0

let () =
  Printexc.record_backtrace true ;
  Debug.log_to_stdout () ;
  ()
  |> Debug.with_thread_associated "main" @@ fun () ->
     try Xapi_stdext_threads.Threadext.Mutex.execute m buggy
     with e ->
       D.log_backtrace e ;
       D.warn "Got exception: %s" (Printexc.to_string e)
