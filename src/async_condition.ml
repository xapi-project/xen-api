open Core_kernel

type 'a t =
  { waits : 'a Ivar.t Queue.t }
[@@deriving sexp_of]

let create () =
  { waits = Queue.create () }
;;

let wait t = Deferred.create (fun ivar -> Queue.enqueue t.waits ivar)

let signal t a =
  Option.iter (Queue.dequeue t.waits) ~f:(fun ivar -> Ivar.fill ivar a);
;;

let broadcast t a =
  Queue.iter t.waits ~f:(fun ivar -> Ivar.fill ivar a);
  Queue.clear t.waits;
;;

let%test_module _ =
  (module struct

    let stabilize = Scheduler.run_cycles_until_no_jobs_remain

    let%test_unit _ =
      let cond = create () in
      let consumer = wait cond in
      assert (not (Deferred.is_determined consumer));
      stabilize();
      assert (not (Deferred.is_determined consumer));
      signal cond ();
      stabilize();
      assert (Deferred.is_determined consumer)
    ;;

    let%test_unit _ =
      let cond = create () in
      signal cond ();
      let consumer = wait cond in
      let consumer2 = wait cond in
      assert (not (Deferred.is_determined consumer));
      stabilize();
      assert (not (Deferred.is_determined consumer));
      signal cond ();
      stabilize();
      assert (Deferred.is_determined consumer);
      assert (not (Deferred.is_determined consumer2))
    ;;

    let%test_unit _ =
      let n = 10 in
      let m = 5 in
      let cond = create () in
      let consumers = Array.init n ~f:(fun _ -> wait cond) in
      stabilize();
      Array.iter consumers ~f:(fun consumer ->
        assert (not (Deferred.is_determined consumer)));
      signal cond ();
      stabilize();
      assert (Deferred.is_determined consumers.(0));
      for i = 1 to pred n do
        assert (not (Deferred.is_determined consumers.(i)));
      done;
      for _ = 1 to m do signal cond () done;
      stabilize();
      for i = 1 to m do
        assert (Deferred.is_determined consumers.(i));
      done;
      for i = succ m to pred n do
        assert (not (Deferred.is_determined consumers.(i)));
      done
    ;;

    let determined def value =
      match Deferred.peek def with
      | Some v -> value = v
      | None -> false
    ;;

    let%test_unit _ =
      let n = 10 in
      let cond = create () in
      let consumers = Array.init n ~f:(fun _ -> wait cond) in
      stabilize();
      Array.iter consumers ~f:(fun consumer ->
        assert (not (Deferred.is_determined consumer)));
      broadcast cond "foo";
      stabilize();
      for i = 0 to pred n do
        assert (determined consumers.(i) "foo");
      done
    ;;

    let%test_unit _ =
      let n = 10 in
      let cond = create () in
      let consumers = Array.init n ~f:(fun _ -> wait cond) in
      stabilize();
      Array.iter consumers ~f:(fun consumer ->
        assert (not (Deferred.is_determined consumer)));
      for i = 0 to pred n do
        signal cond i;
      done;
      stabilize();
      for i = 0 to pred n do
        assert (determined consumers.(i) i);
      done
    ;;
  end)
