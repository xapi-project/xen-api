open! Core_kernel
open! Import

module Deferred = Eager_deferred0

module Deferred_result = Eager_deferred_result

module Monitor = struct
  let try_with = Monitor.try_with ~run:`Now
end

(* Copied from [deferred_or_error.ml].  There should be no diffs below this line. *)

include (Deferred_result : Monad.S2
         with type ('a, 'b) t := ('a, 'b) Deferred_result.t
         with module Let_syntax := Deferred_result.Let_syntax)

type 'a t = 'a Or_error.t Deferred.t

include Applicative.Make (struct
    type nonrec 'a t = 'a t
    let return = return
    let apply f x =
      Deferred_result.combine f x
        ~ok:(fun f x -> f x)
        ~err:(fun e1 e2 -> Error.of_list [e1; e2])
    let map = `Custom map
  end)

module Let_syntax = struct
  let return = return
  include Monad_infix
  module Let_syntax = struct
    let return = return
    let map    = map
    let bind   = bind
    let both   = both (* from Applicative.Make *)
    module Open_on_rhs  = struct end
  end
end

open Let_syntax

let ignore = ignore_m

let fail error = Deferred.return (Result.fail error)

let ok_exn t = Deferred.map t ~f:Or_error.ok_exn

let of_exn exn = Deferred.return (Or_error.of_exn exn)

let of_exn_result t = Deferred.map t ~f:Or_error.of_exn_result

let error msg v sexp_of = Deferred.return (Or_error.error msg v sexp_of)

let error_s sexp = Deferred.return (Or_error.error_s sexp)

let error_string msg = Deferred.return (Or_error.error_string msg)

let errorf format = ksprintf error_string format

let tag t ~tag = Deferred.map t ~f:(Or_error.tag ~tag)

let tag_arg t message a sexp_of_a =
  Deferred.map t ~f:(fun t -> Or_error.tag_arg t message a sexp_of_a)
;;

let unimplemented msg = Deferred.return (Or_error.unimplemented msg)

let combine_errors l =
  Deferred.map (Deferred.all l) ~f:Or_error.combine_errors
;;

let combine_errors_unit l =
  Deferred.map (Deferred.all l) ~f:Or_error.combine_errors_unit
;;

let find_map_ok l ~f =
  Deferred.repeat_until_finished (l, []) (fun (l, errors) ->
    match l with
    | [] ->
      let errors = Error.of_list (List.rev errors) in
      Deferred.return (`Finished (Error errors))
    | hd :: tl ->
      Deferred.map (f hd) ~f:(function
        | Error current_error -> `Repeat   (tl, current_error :: errors)
        | Ok    result        -> `Finished (Ok  result)))
;;

let ok_unit = return ()

let never = Deferred.never

let try_with ?extract_exn ?here ?name f =
  Deferred.map (Monitor.try_with ?extract_exn ?here ?name f) ~f:(function
    | Error exn -> Error (Error.of_exn exn)
    | Ok _ as ok -> ok)
;;

let try_with_join ?extract_exn ?here ?name f =
  Deferred.map (try_with ?here ?extract_exn ?name f) ~f:Or_error.join
;;

module List = struct

  let foldi list ~init:acc ~f =
    let rec loop i acc = function
      | [] -> return acc
      | hd :: tl ->
        let%bind acc = f i acc hd in
        loop (i + 1) acc tl
    in
    loop 0 acc list
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let seqmapi t ~f =
    foldi t ~init:[] ~f:(fun i bs a -> let%map b = f i a in b :: bs)
    >>| List.rev
  ;;

  let all = all
  let all_unit = all_unit

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Parallel | `Max_concurrent_jobs _ as how ->
      all_unit (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential ->
      foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let mapi ?(how=`Sequential) t ~f =
    match how with
    | `Parallel | `Max_concurrent_jobs _ as how ->
      all (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential -> seqmapi t ~f
  ;;

  let filter_mapi ?how t ~f = mapi t ?how ~f >>| List.filter_opt
  let concat_mapi ?how t ~f = mapi t ?how ~f >>| List.concat

  let filteri ?how t ~f =
    filter_mapi ?how t ~f:(fun i x ->
      let%map b = f i x in
      if b then (Some x) else None)
  ;;

  let find_mapi t ~f =
    let rec find_mapi t ~f i =
      match t with
      | [] -> return None
      | hd :: tl ->
        match%bind f i hd with
        | None -> find_mapi tl ~f (i+1)
        | Some _ as some -> return some
    in
    find_mapi t ~f 0
  ;;
  let find_map t ~f =
    find_mapi t ~f:(fun _ a -> f a)
  ;;

  let findi t ~f =
    find_mapi t ~f:(fun i elt -> let%map b = f i elt in if b then (Some (i,elt)) else None)
  ;;
  let find t ~f =
    find_map t ~f:(fun elt -> let%map b = f elt in if b then (Some elt) else None)
  ;;

  let existsi t ~f =
    match%map find_mapi t ~f:(fun i elt -> let%map b = f i elt in if b then (Some ()) else None) with
    | Some () -> true
    | None -> false

  let for_alli t ~f =
    match%map find_mapi t ~f:(fun i elt -> let%map b = f i elt in if not b then (Some ()) else None) with
    | Some () -> false
    | None -> true

  let iter       ?how t ~f = iteri       ?how t ~f:(fun _ a -> f a)
  let map        ?how t ~f = mapi        ?how t ~f:(fun _ a -> f a)
  let filter     ?how t ~f = filteri     ?how t ~f:(fun _ a -> f a)
  let filter_map ?how t ~f = filter_mapi ?how t ~f:(fun _ a -> f a)
  let concat_map ?how t ~f = concat_mapi ?how t ~f:(fun _ a -> f a)
  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
  let exists   t ~f = existsi   t ~f:(fun _ a -> f a)
  let for_all  t ~f = for_alli  t ~f:(fun _ a -> f a)

  let init ?how n ~f = map ?how (List.init n ~f:Fn.id) ~f

end

let%test_module _ =
  (module struct

    (* Ounit generates code using [List.rev], but we rebound [List] above, so we need to
       [open Core_kernel] to get [List].  But that shadows a couple other things we need, so we
       bind them first. *)
    module Seqlist = List

    module List = Core_kernel.List

    let tasks = Queue.create ()

    let flush () =
      let rec aux () =
        match Queue.dequeue tasks with
        | None -> ()
        | Some task -> task (); aux ()
      in
      aux ()
    ;;

    let return a =
      let ivar = Ivar.create () in
      let task () = Ivar.fill ivar (Ok a) in
      Queue.enqueue tasks task;
      Ivar.read ivar;
    ;;

    let rec stabilize () =
      flush ();
      Scheduler.run_cycles_until_no_jobs_remain ();
      if not (Queue.is_empty tasks) then (stabilize ());
    ;;

    let determined def value =
      match Deferred.peek def with
      | Some (Ok v) -> value = v
      | Some (Error _)
      | None -> false
    ;;

    let%test_unit _ =
      let def = return 123 in
      stabilize ();
      assert (determined def 123)
    ;;

    let%test_unit _ =
      let def = never () in
      stabilize ();
      assert (Deferred.peek def = None)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.fold [ 0 ; 1 ; 2 ] ~init:"" ~f:(fun acc value ->
          return (acc ^ Int.to_string value))
      in
      stabilize ();
      assert (determined def "012")
    ;;

    let%test_unit _ =
      let def = Seqlist.init 3 ~f:(fun value -> return (string_of_int value)) in
      stabilize ();
      assert (determined def [ "0" ; "1" ; "2" ])
    ;;

    let%test_unit _ =
      let r = ref 0 in
      let n = 3 in
      let def =
        Seqlist.iter (List.init ~f:ident n)
          ~f:(fun value -> r := !r + value; return ())
      in
      stabilize ();
      assert (determined def ());
      assert (!r = n * (n - 1) / 2)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.map [ 0 ; 1 ; 2 ]
          ~f:(fun value -> return (succ value))
      in
      stabilize ();
      assert (determined def [ 1 ; 2 ; 3 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.mapi [ 2 ; 1 ; 0 ]
          ~f:(fun i value -> return (i * 10 + value))
      in
      stabilize ();
      assert (determined def [ 2 ; 11 ; 20 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.filter [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value -> return (value mod 2 = 0))
      in
      stabilize ();
      assert (determined def [ 0 ; 2 ; 4 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.filteri [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value -> return (i > value))
      in
      stabilize ();
      assert (determined def [ 1 ; 0 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.filter_map [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              if value mod 2 = 0 then (Some (succ value)) else None))
      in
      stabilize ();
      assert (determined def [ 1 ; 3 ; 5 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.filter_mapi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              if i > value then (Some (i,value)) else None))
      in
      stabilize ();
      assert (determined def [ (3,1) ; (4,0) ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.concat_map [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              List.init (4-value) ~f:Fn.id))
      in
      stabilize ();
      assert (determined def [ 0 ; 1 ; 2 ; 3 ; 0 ; 1 ; 2 ; 0 ; 1 ; 0 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.concat_mapi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              List.init value ~f:(fun j -> i+j)))
      in
      stabilize ();
      assert (determined def [ 0 ; 1 ; 2 ; 3 ; 1 ; 2 ; 3 ; 2 ; 3 ; 3 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.foldi [ 0 ; 1 ; 2 ] ~init:"" ~f:(fun i acc value ->
          return (acc ^ Int.to_string value ^ Int.to_string (i+3)))
      in
      stabilize ();
      assert (determined def "031425")
    ;;

    let%test_unit _ =
      let list = List.init 3 ~f:(fun i -> return i) in
      let def = Seqlist.all list in
      stabilize ();
      assert (determined def [ 0 ; 1 ; 2 ])
    ;;

    let%test_unit _ =
      let def =
        Seqlist.find [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              value = 3))
      in
      stabilize ();
      assert (determined def (Some 3))
    ;;

    let%test_unit _ =
      let def =
        Seqlist.find [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              value = 5))
      in
      stabilize ();
      assert (determined def None)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.findi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              i > value))
      in
      stabilize ();
      assert (determined def (Some (3,1)))
    ;;

    let%test_unit _ =
      let def =
        Seqlist.findi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              i + value = 5))
      in
      stabilize ();
      assert (determined def None)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.find_map [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              if value = 3 then (Some (succ value)) else None))
      in
      stabilize ();
      assert (determined def (Some 4))
    ;;

    let%test_unit _ =
      let def =
        Seqlist.find_map [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              if value = 5 then (Some (succ value)) else None))
      in
      stabilize ();
      assert (determined def None)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.find_mapi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              if value = 3 then (Some (i,succ value)) else None))
      in
      stabilize ();
      assert (determined def (Some (1,4)))
    ;;

    let%test_unit _ =
      let def =
        Seqlist.find_mapi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              if value = 5 then (Some (i,succ value)) else None))
      in
      stabilize ();
      assert (determined def None)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.exists [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              value = 3))
      in
      stabilize ();
      assert (determined def true)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.exists [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              value = 5))
      in
      stabilize ();
      assert (determined def false)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.existsi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              i > value))
      in
      stabilize ();
      assert (determined def true)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.existsi [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              i + value = 5))
      in
      stabilize ();
      assert (determined def false)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.for_all [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              value <> 3))
      in
      stabilize ();
      assert (determined def false)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.for_all [ 0 ; 1 ; 2 ; 3 ; 4 ]
          ~f:(fun value ->
            return (
              value <> 5))
      in
      stabilize ();
      assert (determined def true)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.for_alli [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              i <= value))
      in
      stabilize ();
      assert (determined def false)
    ;;

    let%test_unit _ =
      let def =
        Seqlist.for_alli [ 4 ; 3 ; 2 ; 1 ; 0 ]
          ~f:(fun i value ->
            return (
              i + value <> 5))
      in
      stabilize ();
      assert (determined def true)
    ;;


    let%test _ =
      let f _ = Deferred.return (Error (Error.of_string "error")) in
      let def = try_with (fun () -> Seqlist.iter ~f [0]) in
      stabilize ();
      match Deferred.peek def with
      | Some (Ok (Error _)) -> true
      | _ -> false
    ;;

    let%test _ =
      let f _ = raise Caml.Not_found in
      let def = try_with (fun () -> Seqlist.iter ~f [0]) in
      stabilize ();
      match Deferred.peek def with
      | Some (Error _) -> true
      | _ -> false
    ;;

    (* tests for non-list functions *)

    let err = Error.of_string "foo"

    let eq' deferred expected =
      stabilize ();
      Option.value_map (Deferred.peek deferred) ~default:false ~f:(fun got -> got = expected)
    ;;

    let eq deferred expected =
      stabilize ();
      match Deferred.peek deferred, expected with
      | Some (Error err), Error expected ->
        let expected, got = Error.to_string_hum expected, Error.to_string_hum err in
        String.(=) expected got
        || (
          eprintf "expected %s, got %s\n%!" expected got;
          false)
      | Some (Ok x), Ok x' -> x = x'
      | Some (Error _), _ -> true
      | _ -> false
    ;;

    let%test _ =
      eq (fail err) (Error err)
    ;;

    let%test_unit _ =
      assert (eq' (ok_exn (return 1)) 1);
      assert (
        let rv = Monitor.try_with (fun () -> ok_exn (fail err)) in
        stabilize ();
        match Deferred.peek rv with
        | Some (Error _) -> true
        | _ -> false);
    ;;

    let%test _ =
      eq (of_exn (Failure "foo")) (Or_error.of_exn (Failure "foo"))
    ;;

    let%test_unit _ =
      assert (eq (of_exn_result (return 1)) (Ok 1));
      let exn_result = Error (Failure "foo") in
      assert (eq (of_exn_result (Deferred.return exn_result))
                (Or_error.of_exn_result exn_result))
    ;;

    let%test _ =
      eq (error "foo" "bar" String.sexp_of_t) (Or_error.error "foo" "bar" String.sexp_of_t)
    ;;

    let%test _ =
      eq (error_string "foo") (Or_error.error_string "foo")
    ;;

    let%test _ =
      eq (unimplemented "foo") (Or_error.unimplemented "foo")
    ;;

    let check deferred_f immediate_f =
      let check l =
        let deferred_l = List.map l ~f:(function true -> return () | false -> fail err) in
        let immediate_l = List.map l ~f:(function true -> Ok () | false -> Error err) in
        assert (eq (deferred_f deferred_l) (immediate_f immediate_l))
      in
      check [ true; true ];
      check [];
      check [ true; false ];
      check [ true; false; false];
    ;;

    let%test_unit _ =
      check combine_errors Or_error.combine_errors
    ;;

    let%test_unit _ =
      check combine_errors_unit Or_error.combine_errors_unit
    ;;

    let%test _ =
      eq ok_unit (Ok ())
    ;;

    let%test _ =
      let rv = never () in
      stabilize ();
      Option.is_none (Deferred.peek rv)
    ;;

    let expect_failure_with_prefix ~prefix deferred =
      stabilize ();
      match Deferred.peek deferred with
      | Some (Error err) ->
        let s = Error.to_string_hum err in
        if String.is_prefix ~prefix s
        then true
        else (
          eprintf "expected %s, got %s\n%!" prefix s;
          false);
      | _ -> false
    ;;

    let%test_unit _ =
      assert (eq (try_with (fun () -> Deferred.return 1)) (Ok 1));
      assert (expect_failure_with_prefix (try_with (fun () -> raise_s [%message "foo"]))
                ~prefix:"(monitor.ml.Error foo");
    ;;

    let%test_unit _ =
      assert (eq (try_with_join (fun () -> return 1)) (Ok 1));
      assert (eq (try_with_join (fun () -> fail err)) (Error err));
      assert (expect_failure_with_prefix (try_with (fun () -> raise_s [%message "foo"]))
                ~prefix:"(monitor.ml.Error foo");
    ;;

  end)
