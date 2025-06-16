module D = Debug.Make (struct let name = Filename.basename __FILE__ end)

let m = Mutex.create ()

let a = [||]

let buggy () = a.(1) <- 0

let with_lock mutex f =
  let finally () = Mutex.unlock mutex in
  Mutex.lock mutex ; Fun.protect ~finally f

let () =
  Printexc.record_backtrace true ;
  Debug.log_to_stdout () ;
  ()
  |> Debug.with_thread_associated "main" @@ fun () ->
     try with_lock m buggy
     with e ->
       D.log_backtrace e ;
       D.warn "Got exception: %s" (Printexc.to_string e)
