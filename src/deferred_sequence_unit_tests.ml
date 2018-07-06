open! Core_kernel
open! Import
open! Deferred_std

let stabilize = Scheduler.run_cycles_until_no_jobs_remain

let numbers = Sequence.range 1 100

let assert_sequences_equal s1 s2 =
  assert (Sequence.to_list s1 = Sequence.to_list s2)
;;

let deferred_result d =
  let deferred_result = ref None in
  upon d (fun v -> deferred_result := Some v);
  stabilize ();
  Option.value_exn !deferred_result
;;

open Deferred.Sequence

let fold = fold

let%test_unit _ =
  let init = 0 in
  let f acc v = v + acc in
  let deferred_acc =
    deferred_result
      (Deferred.Sequence.fold numbers ~init ~f:(fun acc v -> return (f acc v)))
  in
  assert (Sequence.fold numbers ~init ~f = deferred_acc)
;;

let foldi = foldi

let%test_unit _ =
  let init = 0 in
  let f i acc _v = i + acc in
  let deferred_acc =
    deferred_result
      (Deferred.Sequence.foldi numbers ~init ~f:(fun i acc v -> return (f i acc v)))
  in
  assert (Sequence.foldi numbers ~init ~f = deferred_acc)
;;

let filter = filter

let%test_unit _ =
  let f i = i % 2 = 0 in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.filter numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.filter numbers ~f) deferred_result
;;

let filteri = filteri

let%test_unit _ =
  let f i j = (i % 2 = 0) = (j % 3 = 0) in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.filteri numbers ~f:(fun i j -> return (f i j)))
  in
  assert_sequences_equal (Sequence.filteri numbers ~f) deferred_result
;;

let filter_map = filter_map

let%test_unit _ =
  let f i = if i % 2 = 0 then (Some i) else None in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.filter_map numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.filter_map numbers ~f) deferred_result
;;

let filter_mapi = filter_mapi

let%test_unit _ =
  let f i j = if (i % 2 = 0) = (j % 3 = 0) then (Some j) else None in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.filter_mapi numbers ~f:(fun i j -> return (f i j)))
  in
  assert_sequences_equal (Sequence.filter_mapi numbers ~f) deferred_result
;;

let concat_map = concat_map

let%test_unit _ =
  let f i = Sequence.init i ~f:(fun j -> i + j) in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.concat_map numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.concat_map numbers ~f) deferred_result
;;

let concat_mapi = concat_mapi

let%test_unit _ =
  let f i j = Sequence.init j ~f:(fun k -> 100 * i + 10 * j + k) in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.concat_mapi numbers ~f:(fun i j -> return (f i j)))
  in
  assert_sequences_equal (Sequence.concat_mapi numbers ~f) deferred_result
;;

let map = map

let%test_unit _ =
  let f i = i * 2 in
  let serial_result =
    deferred_result
      (Deferred.Sequence.map ~how:`Sequential numbers ~f:(fun i -> return (f i)))
  in
  let parallel_result =
    deferred_result
      (Deferred.Sequence.map ~how:`Parallel   numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.map numbers ~f) serial_result;
  assert_sequences_equal (Sequence.map numbers ~f) parallel_result
;;

let mapi = mapi

let%test_unit _ =
  let f i j = i % 3 + j * 2 in
  let serial_result =
    deferred_result
      (Deferred.Sequence.mapi ~how:`Sequential numbers ~f:(fun i j -> return (f i j)))
  in
  let parallel_result =
    deferred_result
      (Deferred.Sequence.mapi ~how:`Parallel   numbers ~f:(fun i j -> return (f i j)))
  in
  assert_sequences_equal (Sequence.mapi numbers ~f) serial_result;
  assert_sequences_equal (Sequence.mapi numbers ~f) parallel_result
;;

let iter = iter

let%test_unit _ =
  let side_effect = ref 0 in
  deferred_result
    (Deferred.Sequence.iter numbers ~f:(fun _ ->
       incr side_effect;
       return ()));
  assert (!side_effect <> 0);
  Sequence.iter numbers ~f:(fun _ -> decr side_effect);
  assert (!side_effect = 0)
;;

let iteri = iteri

let%test_unit _ =
  let side_effect = ref 0 in
  deferred_result
    (Deferred.Sequence.iteri numbers ~f:(fun i _ ->
       side_effect := !side_effect + i;
       return ()));
  assert (!side_effect <> 0);
  Sequence.iteri numbers ~f:(fun i _ -> side_effect := !side_effect - i);
  assert (!side_effect = 0)
;;

let all       = all
let all_unit  = all_unit
let find      = find
let findi     = findi
let find_map  = find_map
let find_mapi = find_mapi
let for_all   = for_all
let exists    = exists
let for_alli  = for_alli
let existsi   = existsi
let init      = init

let%test_unit _ =
  for n = 0 to 5 do
    [%test_result: int Sequence.t] ~expect:(Sequence.init n ~f:Fn.id)
      (deferred_result (all (Sequence.init n ~f:return)));
    [%test_result: unit] ~expect:()
      (deferred_result (all_unit (Sequence.init n ~f:(fun _ -> return ()))));
    [%test_result: int option] ~expect:(if n = 0 then None else (Some (n - 1)))
      (deferred_result (find (Sequence.init n ~f:Fn.id) ~f:(fun i -> return (i = n-1))));
    [%test_result: (int * int) option] ~expect:(if n <= 1 then None else (Some (n-n/2,n-n/2)))
      (deferred_result (findi (Sequence.init n ~f:Fn.id) ~f:(fun i j -> return (i+j >= n))));
    [%test_result: string option] ~expect:(if n = 0 then None else (Some "yes"))
      (deferred_result (find_map (Sequence.init n ~f:Fn.id) ~f:(fun i ->
         return (if i = n-1 then (Some "yes") else None))));
    [%test_result: int option] ~expect:(if n = 0 || n % 2 = 1 then None else (Some (n/2)))
      (deferred_result (find_mapi (Sequence.init n ~f:Fn.id) ~f:(fun i j ->
         return (if i+j = n then (Some j) else None))));
    [%test_result: bool] ~expect:(n <= 2)
      (deferred_result (for_all (Sequence.init n ~f:Fn.id) ~f:(fun i ->
         return (i < 2))));
    [%test_result: bool] ~expect:(n >= 3)
      (deferred_result (exists (Sequence.init n ~f:Fn.id) ~f:(fun i ->
         return (i % 3 = 2))));
    [%test_result: bool] ~expect:(n = 0 || n % 2 = 1)
      (deferred_result (for_alli (Sequence.init n ~f:Fn.id) ~f:(fun i j ->
         return (i + j <> n))));
    [%test_result: bool] ~expect:(n <> 0 && n % 2 = 0)
      (deferred_result (existsi (Sequence.init n ~f:Fn.id) ~f:(fun i j ->
         return (i + j = n))));
    [%test_result: int Sequence.t] ~expect:(Sequence.init n ~f:Fn.id)
      (deferred_result (init n ~f:return));
    [%test_result: int Sequence.t] ~expect:(Sequence.init n ~f:(fun i -> i - 1))
      (deferred_result (map (Sequence.init n ~f:Fn.id) ~f:(fun i -> return (i - 1))));
  done
;;
