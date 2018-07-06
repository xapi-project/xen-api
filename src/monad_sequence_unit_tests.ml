open! Core_kernel
open! Import
open! Deferred_std

module Deferred = Deferred1

module type Comparison_sequence = sig
  type 'a t [@@deriving compare, sexp_of]

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b
  val fold  : 'a t -> init:'b -> f:(       'b -> 'a -> 'b) -> 'b

  val init       :  int -> f:(int       ->         'a) -> 'a t
  val iter       : 'a t -> f:(       'a ->       unit) -> unit
  val iteri      : 'a t -> f:(int -> 'a ->       unit) -> unit
  val map        : 'a t -> f:(       'a ->         'b) -> 'b t
  val mapi       : 'a t -> f:(int -> 'a ->         'b) -> 'b t
  val filter     : 'a t -> f:(       'a ->       bool) -> 'a t
  val filteri    : 'a t -> f:(int -> 'a ->       bool) -> 'a t
  val filter_map : 'a t -> f:(       'a  -> 'b option) -> 'b t
  val filter_mapi: 'a t -> f:(int -> 'a  -> 'b option) -> 'b t
  val concat_map : 'a t -> f:(       'a  -> 'b      t) -> 'b t
  val concat_mapi: 'a t -> f:(int -> 'a  -> 'b      t) -> 'b t

  val find     : 'a t -> f:(       'a -> bool     ) -> 'a option
  val findi    : 'a t -> f:(int -> 'a -> bool     ) -> (int * 'a) option
  val find_map : 'a t -> f:(       'a -> 'b option) -> 'b option
  val find_mapi: 'a t -> f:(int -> 'a -> 'b option) -> 'b option

  val exists  : 'a t -> f:(       'a -> bool     ) -> bool
  val existsi : 'a t -> f:(int -> 'a -> bool     ) -> bool
  val for_all : 'a t -> f:(       'a -> bool     ) -> bool
  val for_alli: 'a t -> f:(int -> 'a -> bool     ) -> bool
end

module Make
    (M : Comparison_sequence)
    (S : Deferred.Monad_sequence with type 'a t := 'a M.t)
  : sig include Monad_sequence.S with type 'a monad := 'a Deferred.t end =
struct
  type 'a t = 'a M.t

  let deferred_result (d : unit Deferred.t) =
    let deferred_result = ref None in
    upon d (fun v -> deferred_result := Some v);
    Scheduler.run_cycles_until_no_jobs_remain ();
    assert (is_some !deferred_result);
  ;;

  let init = S.init

  (* A function that attempts to test whether [method_deferred], a method of the
     monad sequence S, behaves the same as [method_base], the equivalent method of
     the non-monad version of the sequence M.

     [f] is the function to pass as an argument to [method_base].
     [transform_f] should be a function intended to transform [f] into
     the function to pass to [method_deferred].
     [handle_results] should compare the results of the two methods.

     For example, you might have:
     method_base = List.map
     method_deferred = Deferred.List.map
     f: (fun x -> x * 2)
     transform_f: A function that performs the transformation
     (fun x -> x * 2)  ->  (fun x -> return (x * 2))
     handle_results: A function that asserts that two lists are equal.

     Also, [transform_f] should have the scheduler yield during each
     call to the transformed f, so that [test] can test whether parallelism
     and throttling is working correctly.
  *)
  let test
        ~method_deferred
        ~method_base
        ~f
        ~transform_f
        ~skip_parallel_check
        ~handle_results =
    let hows = [
      `Sequential;
      `Parallel;
      `Max_concurrent_jobs 1;
      `Max_concurrent_jobs 2;
      `Max_concurrent_jobs 3;
      `Max_concurrent_jobs 4;
      `Max_concurrent_jobs 5;
      `Max_concurrent_jobs 6;
      `Max_concurrent_jobs 7;
      `Max_concurrent_jobs 8;
      `Max_concurrent_jobs 9;
      `Max_concurrent_jobs 10;
    ]
    in
    let rand = Random.State.make [| 12345 |] in
    List.iter hows ~f:(fun how ->
      let max_concurrent_jobs =
        match how with
        | `Sequential -> 1
        | `Parallel -> 100000
        | `Max_concurrent_jobs x -> x
      in
      for length = 0 to 8 do
        for _reps = 0 to 50 do
          deferred_result (
            let parallel_cur = ref 0 in
            let parallel_max = ref 0 in
            let array = Array.init length ~f:(fun i ->
              let r = Random.State.int rand 10 in
              if r = 0 then (i) else (r - 1))
            in
            let%bind deferred_sequence =
              S.init ~how length ~f:(fun i ->
                incr parallel_cur;
                parallel_max := max !parallel_max !parallel_cur;
                let%bind () = Scheduler.yield (Scheduler.t ()) in
                decr parallel_cur;
                return array.(i)
              )
            in
            [%test_result: int] !parallel_max ~expect:(min max_concurrent_jobs length);

            let parallel_cur = ref 0 in
            let parallel_max = ref 0 in
            let base_sequence = M.init length ~f:(fun i -> array.(i)) in
            let%map deferred_result =
              let f = transform_f ~f ~parallel_cur ~parallel_max in
              method_deferred ?how:(Some how) deferred_sequence ~f
            in
            let base_result = method_base base_sequence ~f in
            handle_results ~deferred_result ~base_result;

            if not skip_parallel_check
            then begin
              [%test_result: int] !parallel_max ~expect:(min max_concurrent_jobs length);
            end;
          )
        done
      done
    )
  ;;

  (* Specialize to [f] having 1 argument *)
  let test1 method_deferred method_base ~f ~handle_results =
    test ~method_deferred ~method_base ~f ~handle_results
      ~skip_parallel_check:false
      ~transform_f:(fun ~f ~parallel_cur ~parallel_max ->
        (fun x ->
           incr parallel_cur;
           parallel_max := max !parallel_max !parallel_cur;
           let%bind () = Scheduler.yield (Scheduler.t ()) in
           decr parallel_cur;
           return (f x)))

  (* Specialize to [f] having 2 arguments *)
  let test2 method_deferred method_base ~f ~handle_results =
    test ~method_deferred ~method_base ~f ~handle_results
      ~skip_parallel_check:false
      ~transform_f:(fun ~f ~parallel_cur ~parallel_max ->
        (fun x y ->
           incr parallel_cur;
           parallel_max := max !parallel_max !parallel_cur;
           let%bind () = Scheduler.yield (Scheduler.t ()) in
           decr parallel_cur;
           return (f x y)))

  let add_how f = (fun ?how:_ -> f)

  (* Specialize to [f] having 1 argument and being sequential *)
  let test1_sequential method_deferred method_base ~f ~handle_results =
    test ~method_deferred:(add_how method_deferred) ~method_base ~f ~handle_results
      ~skip_parallel_check:true
      ~transform_f:(fun ~f ~parallel_cur:_ ~parallel_max:_ ->
        (fun x -> return (f x)))

  (* Specialize to [f] having 2 arguments and being sequential *)
  let test2_sequential method_deferred method_base ~f ~handle_results =
    test ~method_deferred:(add_how method_deferred) ~method_base ~f ~handle_results
      ~skip_parallel_check:true
      ~transform_f:(fun ~f ~parallel_cur:_ ~parallel_max:_ ->
        (fun x y -> return (f x y)))

  (* Specialize to [f] having 3 arguments and being sequential *)
  let test3_sequential method_deferred method_base ~f ~handle_results =
    test ~method_deferred:(add_how method_deferred) ~method_base ~f ~handle_results
      ~skip_parallel_check:true
      ~transform_f:(fun ~f ~parallel_cur:_ ~parallel_max:_ ->
        (fun x y z -> return (f x y z)))


  let map = S.map
  let%test_unit _ =
    test1 S.map M.map ~f:(fun x -> x * 2)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let mapi = S.mapi
  let%test_unit _ =
    test2 S.mapi M.mapi ~f:(fun i x -> x * 2 lxor i)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let filter = S.filter
  let%test_unit _ =
    test1 S.filter M.filter ~f:(fun x -> x mod 2 = 0)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let filteri = S.filteri
  let%test_unit _ =
    test2 S.filteri M.filteri ~f:(fun i x -> x mod 2 = i mod 2)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let filter_map = S.filter_map
  let%test_unit _ =
    test1 S.filter_map M.filter_map
      ~f:(fun x -> if x mod 2 = 0 then (Some (x+1)) else None)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let filter_mapi = S.filter_mapi
  let%test_unit _ =
    test2 S.filter_mapi M.filter_mapi
      ~f:(fun i x -> if x mod 2 = i mod 2 then (Some (x+i)) else None)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let concat_map = S.concat_map
  let%test_unit _ =
    test1 S.concat_map M.concat_map ~f:(fun x -> M.init (x mod 3) ~f:(fun j -> x + j))
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let concat_mapi = S.concat_mapi
  let%test_unit _ =
    test2 S.concat_mapi M.concat_mapi
      ~f:(fun i x -> M.init ((i+x) mod 3) ~f:(fun j -> x + i + j))
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int M.t] deferred_result ~expect:base_result;
      )
  ;;

  let find = S.find
  let%test_unit _ =
    test1_sequential S.find M.find ~f:(fun x -> x mod 2 = 0)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int option] deferred_result ~expect:base_result;
      )
  ;;

  let findi = S.findi
  let%test_unit _ =
    test2_sequential S.findi M.findi ~f:(fun i x -> x mod 2 = i mod 2)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: (int * int) option] deferred_result ~expect:base_result;
      )
  ;;


  let find_map = S.find_map
  let%test_unit _ =
    test1_sequential S.find_map M.find_map
      ~f:(fun x -> if x mod 2 = 0 then (Some (x+1)) else None)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int option] deferred_result ~expect:base_result;
      )
  ;;

  let find_mapi = S.find_mapi
  let%test_unit _ =
    test2_sequential S.find_mapi M.find_mapi
      ~f:(fun i x -> if x mod 2 = i mod 2 then (Some (x+i)) else None)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int option] deferred_result ~expect:base_result;
      )
  ;;

  let fold = S.fold
  let%test_unit _ =
    test2_sequential (S.fold ~init:10) (M.fold ~init:10) ~f:(fun acc x -> acc + x)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int] deferred_result ~expect:base_result;
      )
  ;;

  let foldi = S.foldi
  let%test_unit _ =
    test3_sequential (S.foldi ~init:10) (M.foldi ~init:10)
      ~f:(fun i acc x -> acc + (x lxor i))
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: int] deferred_result ~expect:base_result;
      )
  ;;

  let for_all = S.for_all
  let%test_unit _ =
    test1_sequential S.for_all M.for_all ~f:(fun x -> x mod 5 <> 0)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: bool] deferred_result ~expect:base_result;
      )
  ;;

  let for_alli = S.for_alli
  let%test_unit _ =
    test2_sequential S.for_alli M.for_alli ~f:(fun i x -> x mod 5 <> i mod 5)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: bool] deferred_result ~expect:base_result;
      )
  ;;

  let exists = S.exists
  let%test_unit _ =
    test1_sequential S.exists M.exists ~f:(fun x -> x mod 5 = 0)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: bool] deferred_result ~expect:base_result;
      )
  ;;

  let existsi = S.existsi
  let%test_unit _ =
    test2_sequential S.existsi M.existsi ~f:(fun i x -> x mod 5 = i mod 5)
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: bool] deferred_result ~expect:base_result;
      )

  (* These just test the parallelism behavor of iter and iteri *)
  let%test_unit _ =
    test1 S.iter M.iter ~f:(fun _ -> ())
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: unit] deferred_result ~expect:base_result;
      )
  ;;
  let%test_unit _ =
    test2 S.iteri M.iteri ~f:(fun _ _ -> ())
      ~handle_results:(fun ~deferred_result ~base_result ->
        [%test_result: unit] deferred_result ~expect:base_result;
      )
  ;;

  let iter_lots_of_random_sequences ~f =
    let rand = Random.State.make [| 67890 |] in
    for length = 0 to 10 do
      for _reps = 0 to 500 do
        let array = Array.init length ~f:(fun i ->
          let r = Random.State.int rand 10 in
          if r = 0 then (i) else (r - 1))
        in
        let sequence = M.init length ~f:(fun i -> array.(i)) in
        f sequence
      done
    done
  ;;

  let iter = S.iter
  let%test_unit _ =
    iter_lots_of_random_sequences ~f:(fun sequence ->
      let elts = ref [] in
      let elts2 = ref [] in
      deferred_result (
        S.iter sequence ~f:(fun x ->
          elts := x :: !elts;
          Deferred.unit)
      );
      M.iter sequence ~f:(fun x ->
        elts2 := x :: !elts2;
      );
      [%test_result: int list] !elts ~expect:!elts2
    )

  let iteri = S.iteri
  let%test_unit _ =
    iter_lots_of_random_sequences ~f:(fun sequence ->
      let elts = ref [] in
      let elts2 = ref [] in
      deferred_result (
        S.iteri sequence ~f:(fun i x ->
          elts := (i,x) :: !elts;
          Deferred.unit)
      );
      M.iteri sequence ~f:(fun i x ->
        elts2 := (i,x) :: !elts2;
      );
      [%test_result: (int * int) list] !elts ~expect:!elts2
    )
  ;;

  let all = S.all
  let all_unit = S.all_unit

  let%test_unit _ =
    iter_lots_of_random_sequences ~f:(fun sequence ->
      let sequence2 = M.map sequence ~f:return in
      deferred_result (
        let%map sequence2 = S.all sequence2 in
        [%test_result: int M.t] sequence2 ~expect:sequence
      )
    )
  let%test_unit _ =
    iter_lots_of_random_sequences ~f:(fun sequence ->
      let length = M.fold sequence ~init:0 ~f:(fun acc _ -> acc + 1) in
      let max = M.fold sequence ~init:0 ~f:(fun acc x -> Int.max acc x) in
      let ivars = Array.init length ~f:(fun _ -> Ivar.create ()) in
      let deferreds = M.mapi sequence ~f:(fun i _ -> Ivar.read ivars.(i)) in
      let done_waiting = S.all_unit deferreds in

      assert (length = 0 || not (Deferred.is_determined done_waiting));
      deferred_result (
        M.iteri sequence ~f:(fun i x -> if x <> max then (Ivar.fill ivars.(i) ()));
        Deferred.unit
      );
      assert (Deferred.is_determined done_waiting = (length = 0));
      deferred_result (
        M.iteri sequence ~f:(fun i _ -> Ivar.fill_if_empty ivars.(i) ());
        Deferred.unit
      );
      assert (Deferred.is_determined done_waiting);
    )

end

