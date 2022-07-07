module Make (V : sig
  type t

  val safe_release : t -> unit

  val move_out_exn : t -> t

  val with_moved_exn : t -> (t -> 'd) -> 'd
end) =
struct
  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  type 'a t = ('a, V.t) Hashtbl.t * Mutex.t

  let create n : 'a t = (Hashtbl.create n, Mutex.create ())

  let release (t, m) =
    with_lock m (fun () -> Hashtbl.iter (fun _ v -> V.safe_release v) t)

  let reset (t, m) =
    release (t, m) ;
    Hashtbl.reset t

  let copy (t, m) = (with_lock m (fun () -> Hashtbl.copy t), Mutex.create ())

  let length (t, m) = with_lock m (fun () -> Hashtbl.length t)

  let move_into (t, m) k v =
    let v = V.move_out_exn v in
    with_lock m (fun () ->
        match Hashtbl.find_opt t k with
        | None ->
            Hashtbl.add t k v
        | Some old ->
            V.safe_release old ; Hashtbl.replace t k v
    )

  let remove (t, m) k =
    with_lock m (fun () ->
        Option.iter V.safe_release (Hashtbl.find_opt t k) ;
        Hashtbl.remove t k
    )

  let find (t, m) k = with_lock m (fun () -> Hashtbl.find t k)

  let with_find_moved_exn (t, m) k =
    let v =
      with_lock m (fun () ->
          let r = Hashtbl.find t k in
          Hashtbl.remove t k ; r
      )
    in
    V.with_moved_exn v

  let fold (t, m) f init = with_lock m (fun () -> Hashtbl.fold f t init)

  let iter (t, m) f = with_lock m (fun () -> Hashtbl.iter f t)
end
