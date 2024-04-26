module type MTEST = sig
  type +'a io

  val mutex_provides_mutal_exclusion : unit -> unit io
end

module Make =
functor
  (M : S.BACKEND)
  ->
  struct
    open M.IO

    type 'a io = 'a M.IO.t

    let ocaml_lock = Mutex.create ()

    let mu = M.Mutex.create ()

    let cond = M.Condition.create ()

    let broadcast () = M.Condition.broadcast cond ()

    let mutex_provides_mutal_exclusion () : unit io =
      let promises =
        List.init 100 (fun _ ->
            M.Condition.wait cond >>= fun () ->
            M.Mutex.with_lock mu (fun () ->
                M.IO.return_unit >>= fun () ->
                (* the with_lock implementation should ensure that only one
                   monad can try to acquire this lock *)
                assert (Mutex.try_lock ocaml_lock) ;
                M.IO.return_unit >>= fun () ->
                Mutex.unlock ocaml_lock ; M.IO.return_unit
            )
        )
      in
      broadcast () ;
      ignore @@ all promises ;
      Printf.printf "%s test.\n" (M.whoami ()) ;
      M.IO.return_unit
  end
