open Core_kernel
open Deferred_std

module T = struct

  type 'a t =
    { start  : unit Ivar.t
    ; result : 'a Or_error.t Deferred.t }

  let create f =
    let start = Ivar.create () in
    { start
    ; result =
        let%bind () = Ivar.read start in
        Monitor.try_with_or_error f }
  ;;

  let wait t = t.result

  let wait_exn t = wait t >>| ok_exn

  let start t = Ivar.fill_if_empty t.start ()

  let force t = start t; wait t

  let force_exn t = force t >>| ok_exn

  let return a = create (fun () -> return a)

  let bind t ~f =
    create (fun () ->
      let%bind a = force_exn t in
      force_exn (f a))
  ;;

  let map t ~f = create (fun () -> force_exn t >>| f)
  let map = `Custom map

end

include T

include Monad.Make (T)

let bind' t f = bind t ~f:(fun a -> create (fun () -> f a))

let is_forced t = Ivar.is_full t.start

let is_determined t = Deferred.is_determined t.result

let peek t = Deferred.peek t.result

let peek_exn t = Option.map (peek t) ~f:ok_exn

let%test_module _ =
  (module struct

    let stabilize = Scheduler.run_cycles_until_no_jobs_remain

    let%test_unit _ =
      let def = create Deferred.return in
      stabilize();
      let consumer = wait def in
      stabilize();
      assert (not (Deferred.is_determined consumer));
      assert (not (is_forced def));
      assert (not (is_determined def))
    ;;

    let%test_unit _ =
      let def = create Deferred.return in
      stabilize();
      let opt = peek def in
      stabilize();
      assert (opt = None);
      assert (not (is_forced def));
      assert (not (is_determined def))
    ;;

    let%test_unit _ =
      let def = create Deferred.return in
      stabilize();
      let consumer = force def in
      stabilize();
      assert (Deferred.is_determined consumer);
      assert (is_determined def);
      assert (is_forced def)
    ;;

    let%test_unit _ =
      let def = create Deferred.return in
      stabilize();
      let consumer = wait def in
      stabilize();
      assert (not (Deferred.is_determined consumer));
      assert (peek def = None);
      assert (not (is_forced def));
      let consumer2 = force def in
      stabilize();
      assert (peek def <> None);
      assert (Deferred.is_determined consumer);
      assert (Deferred.is_determined consumer2);
      assert (is_forced def)
    ;;

    let determined def value =
      match Deferred.peek def with
      | Some v -> value = v
      | None -> false
    ;;

    let make_bind_test make final =
      let def1 = create Deferred.return in
      let def2 = make def1 in
      stabilize();
      assert (not (is_forced def1));
      assert (not (is_forced def2));
      let consumer1 = wait_exn def1 in
      let consumer2 = wait_exn def2 in
      stabilize();
      assert (not (Deferred.is_determined consumer1));
      assert (not (Deferred.is_determined consumer2));
      let force1 = force_exn def1 in
      stabilize();
      assert (determined consumer1 ());
      assert (determined force1 ());
      assert (not (Deferred.is_determined consumer2));
      let force2 = force_exn def2 in
      stabilize();
      assert (determined force2 final);
    ;;

    (* bind' *)
    let%test_unit _ =
      let final = "foo" in
      let make def1 =
        bind' def1 (fun () -> Deferred.return final)
      in
      make_bind_test make final
    ;;

    let%test_unit _ =
      let final = "foo" in
      let make def1 =
        bind def1 ~f:(fun () -> create (fun () -> Deferred.return "foo"))
      in
      make_bind_test make final
    ;;

    exception E_for_test

    let determined_as_E_for_test def =
      match Deferred.peek def with
      | None | Some (Ok _) -> false
      | Some (Error err)   ->
        String.is_substring ~substring:(Exn.to_string E_for_test) (Error.to_string_hum err)
    ;;

    let%test_unit _ =
      let def = create (fun _ -> raise E_for_test) in
      stabilize();
      assert (not (is_determined def));
      let def = force def in
      stabilize();
      assert (determined_as_E_for_test def)
    ;;

    let%test_unit _ =
      let def = create (fun () -> Deferred.return "foo") in
      let def = bind def ~f:(fun _ -> raise E_for_test) in
      stabilize();
      assert (not (is_determined def));
      let def = force def in
      stabilize();
      assert (determined_as_E_for_test def)
    ;;

    let%test_unit _ =
      let def = create (fun _ -> raise E_for_test) in
      stabilize();
      assert (not (is_determined def));
      let def = Monitor.try_with_or_error ~extract_exn:true (fun () -> force_exn def) in
      stabilize();
      assert (determined_as_E_for_test def)
    ;;

    let%test_unit _ =
      let def = create (fun () -> Deferred.return "foo") in
      let def = bind def ~f:(fun _ -> raise E_for_test) in
      stabilize();
      assert (not (is_determined def));
      let def = Monitor.try_with_or_error ~extract_exn:true (fun () -> force_exn def) in
      stabilize();
      assert (determined_as_E_for_test def)
    ;;

  end)
