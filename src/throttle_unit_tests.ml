open Core_kernel
open Import
open Deferred_std

module Stream = Async_stream

module Throttle = Throttle_debug.Debug (Throttle)
open Throttle

module Deferred = Deferred

let stabilize = Scheduler.run_cycles_until_no_jobs_remain

module T2 = T2

type nonrec 'a outcome = 'a outcome [@@deriving sexp_of]

type nonrec 'a t = 'a t [@@deriving sexp_of]

let invariant = invariant

module Sequencer = Sequencer

let monad_sequence_how  = monad_sequence_how
let monad_sequence_how2 = monad_sequence_how2

let create              = create
let enqueue'            = enqueue'
let is_dead             = is_dead
let max_concurrent_jobs = max_concurrent_jobs

let%test _ =
  try
    ignore (create ~continue_on_error:false ~max_concurrent_jobs:0 : _ t);
    false
  with _ -> true
;;

(* [enqueue] does not start the job immediately. *)
let%test_unit _ =
  let t = create ~continue_on_error:false ~max_concurrent_jobs:1 in
  let i = ref 0 in
  let (_ : unit Deferred.t) = enqueue t (fun () -> incr i; return ()) in
  assert (!i = 0);
  stabilize ();
  assert (!i = 1)
;;

let%test_unit _ =
  (* Check [~continue_on_error:false]. *)
  let t = create ~continue_on_error:false ~max_concurrent_jobs:1 in
  assert (max_concurrent_jobs t = 1);
  assert (not (is_dead t));
  let d1 = enqueue' t (fun () -> raise_s [%message [%here]]) in
  let d2 = enqueue' t (fun () -> assert false) in
  stabilize ();
  assert (num_jobs_waiting_to_start t = 0);
  assert (is_dead t);
  assert (match Deferred.peek d1 with Some (`Raised _) -> true | _ -> false);
  assert (match Deferred.peek d2 with Some `Aborted -> true | _ -> false)
;;

let%test_unit _ =
  (* Check [~continue_on_error:true]. *)
  let t = create ~continue_on_error:true ~max_concurrent_jobs:1 in
  let d1 = enqueue' t (fun () -> raise_s [%message [%here]]) in
  let d2 = enqueue' t (fun () -> return 13) in
  stabilize ();
  assert (not (is_dead t));
  assert (match Deferred.peek d1 with Some (`Raised _) -> true | _ -> false);
  assert (match Deferred.peek d2 with Some (`Ok 13) -> true | _ -> false)
;;

let enqueue = enqueue

let%test_unit _ =
  (* Check that jobs are started in the order they are enqueued. *)
  let t = create ~continue_on_error:false ~max_concurrent_jobs:2 in
  assert (max_concurrent_jobs t = 2);
  let r = ref [] in
  for i = 0 to 99 do
    don't_wait_for (enqueue t (fun () -> r := i :: !r; return ()));
  done;
  stabilize ();
  assert (!r = List.rev (List.init 100 ~f:Fn.id))
;;

let num_jobs_running          = num_jobs_running
let num_jobs_waiting_to_start = num_jobs_waiting_to_start

let%test_unit _ =
  let t = create ~continue_on_error:false ~max_concurrent_jobs:2 in
  assert (num_jobs_waiting_to_start t = 0);
  let add_job () =
    let ivar = Ivar.create () in
    don't_wait_for (enqueue t (fun () -> Ivar.read ivar));
    ivar;
  in
  let i1 = add_job () in
  assert (num_jobs_waiting_to_start t + num_jobs_running t = 1);
  stabilize ();
  assert (num_jobs_waiting_to_start t = 0);
  assert (num_jobs_running t = 1);
  let _i2 = add_job () in
  assert (num_jobs_waiting_to_start t + num_jobs_running t = 2);
  stabilize ();
  assert (num_jobs_waiting_to_start t = 0);
  assert (num_jobs_running t = 2);
  let _i3 = add_job () in
  assert (num_jobs_waiting_to_start t = 1);
  assert (num_jobs_running t = 2);
  stabilize ();
  assert (num_jobs_waiting_to_start t = 1);
  assert (num_jobs_running t = 2);
  Ivar.fill i1 ();
  stabilize ();
  assert (num_jobs_waiting_to_start t = 0);
  assert (num_jobs_running t = 2)
;;

let capacity_available = capacity_available

let%test_unit _ =
  let t = create ~continue_on_error:false ~max_concurrent_jobs:2 in
  let r = capacity_available t in
  stabilize ();
  assert (Option.is_some (Deferred.peek r));
  let i1 = Ivar.create () in
  don't_wait_for (enqueue t (fun () -> Ivar.read i1));
  let r = capacity_available t in
  stabilize ();
  assert (Option.is_some (Deferred.peek r));
  let i2 = Ivar.create () in
  don't_wait_for (enqueue t (fun () -> Ivar.read i2));
  let r = capacity_available t in
  stabilize ();
  assert (Option.is_none (Deferred.peek r));
  Ivar.fill i1 ();
  stabilize ();
  assert (Option.is_some (Deferred.peek r))
;;

let create_with     = create_with
let prior_jobs_done = prior_jobs_done

let%test_unit _ =
  (* Check that [max_concurrent_jobs] limit works, and is fully utilized. *)
  List.iter [ 1; 10; 100; 1000 ] ~f:(fun num_jobs ->
    List.iter [ 1; 10; 100; 1000 ] ~f:(fun max_concurrent_jobs ->
      let resources = List.init max_concurrent_jobs ~f:(fun _ -> ref false) in
      let max_observed_concurrent_jobs = ref 0 in
      let num_concurrent_jobs = ref 0 in
      let job_starts = ref [] in
      let t = create_with ~continue_on_error:false resources in
      assert (Throttle.max_concurrent_jobs t = max_concurrent_jobs);
      let d = prior_jobs_done t in
      stabilize ();
      assert (Deferred.is_determined d);
      let continue = Ivar.create () in
      let jobs = ref [] in
      for i = 0 to num_jobs - 1; do
        let job =
          enqueue t (fun r ->
            assert (not !r); (* ensure no one else is accessing the resource *)
            r := true;
            job_starts := i :: !job_starts;
            incr num_concurrent_jobs;
            max_observed_concurrent_jobs :=
              max !max_observed_concurrent_jobs !num_concurrent_jobs;
            assert (!num_concurrent_jobs <= max_concurrent_jobs);
            let%map () = Ivar.read continue in
            decr num_concurrent_jobs;
            r := false)
        in
        jobs := job :: !jobs
      done;
      let all_done = prior_jobs_done t in
      let jobs = !jobs in
      let jobs_finished = Deferred.all_unit jobs in
      stabilize ();
      assert (not (Deferred.is_determined all_done));
      let num_initial_jobs = min num_jobs max_concurrent_jobs in
      assert (!num_concurrent_jobs = num_initial_jobs);
      assert (List.length !job_starts = num_initial_jobs);
      if max_concurrent_jobs = 1
      then (assert (!job_starts = List.init num_initial_jobs ~f:Fn.id));
      Ivar.fill continue ();
      stabilize ();
      assert (Deferred.is_determined all_done);
      assert (!max_observed_concurrent_jobs = min num_jobs max_concurrent_jobs);
      assert (Deferred.is_determined jobs_finished);
      if max_concurrent_jobs = 1
      then (assert (List.rev !job_starts = List.init num_jobs ~f:Fn.id)); ))
;;

let at_kill = at_kill
let cleaned = cleaned

let%test_unit _ =
  let test ~num_resources ~num_jobs_before_fail ~num_jobs_after_fail =
    let resources = List.init num_resources ~f:(fun _ -> ref true) in
    let t = create_with ~continue_on_error:false resources in
    let continue_at_kill = Ivar.create () in
    let num_at_kill_started = ref 0 in
    let num_jobs_run = ref 0 in
    try
      at_kill t (fun resource ->
        incr num_at_kill_started;
        let%map () = Ivar.read continue_at_kill in
        resource := false);
      let continue_jobs = Ivar.create () in
      let enqueue num_jobs =
        Deferred.all
          (List.init num_jobs ~f:(fun _ ->
             enqueue' t (fun _ ->
               let%map () = Ivar.read continue_jobs in
               incr num_jobs_run)))
      in
      let before_fail = enqueue num_jobs_before_fail in
      let fail = enqueue' t (fun _ -> raise_s [%message [%here]]) in
      let after_fail = enqueue num_jobs_after_fail in
      let cleaned = cleaned t in
      let is_cleaned () = Deferred.is_determined cleaned in
      stabilize ();
      assert (not (is_cleaned ()));
      (* We should have starting freeing all resources that aren't occupied by jobs. *)
      assert (!num_at_kill_started >=
              Int.max 0
                (num_resources - 1 - (num_jobs_before_fail + num_jobs_after_fail)));
      Ivar.fill continue_jobs ();
      stabilize ();
      assert (not (is_cleaned ()));
      assert (Deferred.is_determined before_fail);
      assert (Deferred.is_determined fail);
      assert (Deferred.is_determined after_fail);
      (* We should have started freeing all resources. *)
      assert (!num_at_kill_started = num_resources);
      Ivar.fill continue_at_kill ();
      stabilize ();
      assert (is_cleaned ());
      assert (List.for_all resources ~f:(fun r -> not !r));
      kill t; (* should have no effect *)
      assert (is_cleaned ());
    with exn ->
      raise_s [%message
        "failure"
          (exn : exn)
          (num_resources : int)
          (num_jobs_before_fail : int)
          (num_jobs_after_fail : int)
          (num_at_kill_started : int ref)
          ~throttle:(t : bool ref t)]
  in
  for num_resources = 1 to 3 do
    for num_jobs_before_fail = 0 to num_resources + 1 do
      for num_jobs_after_fail = 0 to num_resources + 1 do
        if false
        then (
          Debug.log "test"
            (`num_resources num_resources,
             `num_jobs_before_fail num_jobs_before_fail,
             `num_jobs_after_fail num_jobs_after_fail)
            [%sexp_of: ([ `num_resources of int ]
                        * [ `num_jobs_before_fail of int ]
                        * [ `num_jobs_after_fail of int ])]);
        test ~num_resources ~num_jobs_before_fail ~num_jobs_after_fail;
      done;
    done;
  done
;;

(* check that exceptions raised by clean function go to the correct monitor *)
let%test_unit _ =
  let t = create ~continue_on_error:false ~max_concurrent_jobs:1 in
  let monitor = Monitor.create () in
  let cleanup_ran = ref false in
  let got_error = ref false in
  Stream.iter (Monitor.detach_and_get_error_stream monitor) ~f:(fun _ -> got_error := true);
  Scheduler.within ~monitor (fun () ->
    at_kill t (fun () -> cleanup_ran := true; raise_s [%message [%here]]));
  let c = cleaned t in
  kill t;
  stabilize ();
  assert !cleanup_ran;
  assert !got_error;
  assert (not (Deferred.is_determined c))
;;

let kill = kill

(* jobs started before [kill] finish. *)
let%test_unit _ =
  let d =
    let t = create ~continue_on_error:false ~max_concurrent_jobs:1 in
    let started = Ivar.create () in
    let finished = Ivar.create () in
    let enqueue_result =
      enqueue' t (fun () -> Ivar.fill started (); Ivar.read finished)
    in
    let%bind () = Ivar.read started in
    kill t;
    Ivar.fill finished ();
    match%bind enqueue_result with
    | `Ok () -> return ()
    | `Aborted | `Raised _ -> assert false
  in
  stabilize ();
  assert (Deferred.is_determined d)
;;

(* jobs enqueued after [kill] are aborted. *)
let%test_unit _ =
  let t = create ~continue_on_error:false ~max_concurrent_jobs:1 in
  kill t;
  let r = ref true in
  let d = enqueue' t (fun () -> r := false; return ()) in
  stabilize ();
  assert (Deferred.peek d = Some `Aborted);
  assert !r
;;

let%test_unit _ = (* enqueueing withing a job doesn't lead to monitor nesting *)
  let seq = Sequencer.create () in
  let rec loop n =
    if n = 0
    then (return ())
    else (
      enqueue seq (fun () ->
        assert (Monitor.depth (Monitor.current ()) < 5);
        don't_wait_for (loop (n - 1));
        return ()))
  in
  let d = loop 100 in
  stabilize ();
  assert (Deferred.peek d = Some ())
;;
