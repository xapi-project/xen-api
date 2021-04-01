open Safe_resources

module Counting : sig
  type t

  val create : unit -> t

  val release : t -> unit

  val get : t -> int
end = struct
  type t = int ref

  let create () = ref 0

  let release t = incr t

  let get t = !t
end

module SafeCounting = struct
  type t = Counting.t Safe.t

  let create () = Safe.create ~release:Counting.release (Counting.create ())
end

let test_can_borrow t =
  let b = Safe.borrow_exn t in
  Alcotest.(check int "no drop" 0 (Counting.get b))

let test_can_release t =
  let counter = Safe.borrow_exn t in
  Safe.safe_release t ;
  Alcotest.(check int "dropped" 1 (Counting.get counter)) ;
  Alcotest.(
    check_raises "use-after-release" Safe.UseAfterMoveOrRelease (fun () ->
        let (_ : Counting.t) = Safe.borrow_exn t in
        ()))

let test_can_release2 t =
  let counter = Safe.borrow_exn t in
  Safe.safe_release t ;
  Safe.safe_release t ;
  Alcotest.(check int "dropped" 1 (Counting.get counter)) ;
  Alcotest.(
    check_raises "use-after-release" Safe.UseAfterMoveOrRelease (fun () ->
        let (_ : Counting.t) = Safe.borrow_exn t in
        ()))

let test_can_move_release t =
  let counter = Safe.borrow_exn t in
  let t' = Safe.move_exn t in
  Alcotest.(check int "dropped" 0 (Counting.get counter)) ;
  Safe.safe_release t' ;
  Alcotest.(check int "dropped" 1 (Counting.get counter)) ;
  (* here we access [counter] after [t] has been dropped.
   * In general one shouldn't do this, but for this unit test it is ok *)
  Alcotest.(check int "dropped" 1 (Counting.get counter))

let test_borrow_exn_after_release t =
  Safe.safe_release t ;
  Alcotest.check_raises "use-after-release" Safe.UseAfterMoveOrRelease
    (fun () -> Safe.borrow_exn t |> Counting.get |> ignore)

let test_move_exn_after_release t =
  Safe.safe_release t ;
  Alcotest.check_raises "use-after-release" Safe.UseAfterMoveOrRelease
    (fun () -> Safe.move_exn t |> ignore)

let test_release_after_move t =
  let counter = Safe.borrow_exn t in
  let t' = Safe.move_exn t in
  Alcotest.check_raises "use-after-move" Safe.UseAfterMoveOrRelease (fun () ->
      Safe.borrow_exn t |> Counting.get |> ignore) ;
  Safe.safe_release t ;
  Alcotest.(check int "dropped" 0 (Counting.get counter)) ;
  Safe.safe_release t' ;
  Alcotest.(check int "dropped" 1 (Counting.get counter))

let tests =
  List.map
    (fun (loc, f) ->
      let t = SafeCounting.create () in
      (loc, `Quick, fun () -> f t))
    [
      (__LOC__, test_can_borrow)
    ; (__LOC__, test_can_release)
    ; (__LOC__, test_can_release2)
    ; (__LOC__, test_can_move_release)
    ; (__LOC__, test_move_exn_after_release)
    ; (__LOC__, test_release_after_move)
    ; (__LOC__, test_borrow_exn_after_release)
    ]
