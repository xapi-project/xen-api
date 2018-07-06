(** Simple implementation of a polymorphic functional double-ended queue. *)

(** Invariants:
    - queue.length = List.length queue.front + List.length queue.back
    - if queue has >= 2 elements, neither front nor back are empty
*)

open! Import
open Std_internal

exception Empty [@@deriving sexp]

type 'a t = { front : 'a list; back : 'a list; length : int }

let length t = t.length

let is_empty t = t.length = 0

let invariant f t =
  let n_front = List.length t.front in
  let n_back  = List.length t.back  in
  assert (t.length = n_front + n_back);
  assert (t.length < 2 || (n_front <> 0 && n_back <> 0));
  List.iter t.front ~f;
  List.iter t.back  ~f
;;

let make ~length ~front ~back =
  match front, back with
  | [], [] | [_], [] | [], [_] | _::_, _::_ ->
    { front ; back ; length }
  | [], _ :: _ :: _ ->
    let back, rev_front = List.split_n back (length/2) in
    { front = List.rev rev_front ; back ; length }
  | _ :: _ :: _, [] ->
    let front, rev_back = List.split_n front (length/2) in
    { front ; back = List.rev rev_back ; length }
;;

let empty = { front = [] ; back = [] ; length = 0 }

let enqueue_front t x =
  make
    ~length:(t.length + 1)
    ~front:(x :: t.front)
    ~back:t.back
;;

let enqueue_back t x =
  make
    ~length:(t.length + 1)
    ~back:(x :: t.back)
    ~front:t.front
;;

let raise_front_invariant () = raise (Bug "Fdeque: |front| = 0, |back| >= 2")
let raise_back_invariant  () = raise (Bug "Fdeque: |back| = 0, |front| >= 2")

let peek_front_exn t =
  match t.front with
  | x :: _ -> x
  | [] ->
    match t.back with
    | [] -> raise Empty
    | [x] -> x
    | _ :: _ :: _ -> raise_front_invariant ()
;;

let peek_back_exn t =
  match t.back with
  | x :: _ -> x
  | [] ->
    match t.front with
    | [] -> raise Empty
    | [x] -> x
    | _ :: _ :: _ -> raise_back_invariant ()
;;

let drop_front_exn t =
  match t.front with
  | _ :: xs -> make ~length:(t.length - 1) ~front:xs ~back:t.back
  | [] ->
    match t.back with
    | [] -> raise Empty
    | [_] -> empty
    | _ :: _ :: _ -> raise_front_invariant ()
;;

let drop_back_exn t =
  match t.back with
  | _ :: xs -> make ~length:(t.length - 1) ~back:xs ~front:t.front
  | [] ->
    match t.front with
    | [] -> raise Empty
    | [_] -> empty
    | _ :: _ :: _ -> raise_front_invariant ()
;;

let dequeue_front_exn t = peek_front_exn t, drop_front_exn t
let dequeue_back_exn  t = peek_back_exn  t, drop_back_exn  t

let optional f t =
  match f t with
  | x -> Some x
  | exception Empty -> None
;;

let peek_front t = optional peek_front_exn t
let peek_back  t = optional peek_back_exn  t

let drop_front t = optional drop_front_exn t
let drop_back  t = optional drop_back_exn  t

let dequeue_front t = optional dequeue_front_exn t
let dequeue_back  t = optional dequeue_back_exn  t

let enqueue t side x =
  match side with
  | `front -> enqueue_front t x
  | `back  -> enqueue_back  t x
;;

let peek t side =
  match side with
  | `front -> peek_front t
  | `back  -> peek_back  t
;;

let peek_exn t side =
  match side with
  | `front -> peek_front_exn t
  | `back  -> peek_back_exn  t
;;

let drop t side =
  match side with
  | `front -> drop_front t
  | `back  -> drop_back  t
;;

let drop_exn t side =
  match side with
  | `front -> drop_front_exn t
  | `back  -> drop_back_exn  t
;;

let dequeue t side =
  match side with
  | `front -> dequeue_front t
  | `back  -> dequeue_back  t
;;

let dequeue_exn t side =
  match side with
  | `front -> dequeue_front_exn t
  | `back  -> dequeue_back_exn  t
;;

let rev t = { t with front = t.back ; back = t.front }

let%bench_module "accessors" =
  (module struct

    module Make (M : sig val t : int t end) = struct

      let%bench "rev" = rev M.t

      let%bench "enqueue_front" = enqueue_front M.t 0
      let%bench "enqueue_back"  = enqueue_back  M.t 0

      let%bench "peek_front"     = peek_front     M.t
      let%bench "peek_back"      = peek_back      M.t
      let%bench "peek_front_exn" = peek_front_exn M.t
      let%bench "peek_back_exn"  = peek_back_exn  M.t

      let%bench "drop_front"     = drop_front     M.t
      let%bench "drop_back"      = drop_back      M.t
      let%bench "drop_front_exn" = drop_front_exn M.t
      let%bench "drop_back_exn"  = drop_back_exn  M.t

      let%bench "dequeue_front"     = dequeue_front     M.t
      let%bench "dequeue_back"      = dequeue_back      M.t
      let%bench "dequeue_front_exn" = dequeue_front_exn M.t
      let%bench "dequeue_back_exn"  = dequeue_back_exn  M.t

    end

    let build front back =
      let length = List.length front + List.length back in
      let t = { length ; front ; back } in
      invariant ignore t;
      t
    ;;

    let list n = List.init n ~f:Int.succ

    let%bench_module "balanced"    = (module Make (struct let t = build (list 50) (list 50) end))
    let%bench_module "short back"  = (module Make (struct let t = build (list 99) (list  1) end))
    let%bench_module "short front" = (module Make (struct let t = build (list  1) (list 99) end))

  end)

module Arbitrary_order = struct

  let is_empty = is_empty
  let length   = length

  let to_list t = List.rev_append t.front t.back

  let to_array t = Array.of_list (to_list t)

  let sum (type a) (module M : Commutative_group.S with type t = a) t ~f =
    let open M in
    List.sum (module M) t.front ~f +
    List.sum (module M) t.back  ~f
  ;;

  let count      t ~f = List.count      t.front ~f +  List.count      t.back  ~f
  let for_all    t ~f = List.for_all    t.front ~f && List.for_all    t.back  ~f
  let exists     t ~f = List.exists     t.front ~f || List.exists     t.back  ~f
  let mem  t x ~equal = List.mem ~equal t.front  x || List.mem ~equal t.back   x
  let iter       t ~f = List.iter       t.front ~f ;  List.iter       t.back  ~f
  ;;

  let fold t ~init ~f =
    List.fold t.front ~init ~f |> fun init ->
    List.fold t.back  ~init ~f
  ;;

  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
  ;;
  let fold_until  t ~init ~f = Container.fold_until  ~fold ~init ~f t
  ;;

  let find t ~f =
    match     List.find t.front ~f with
    | None -> List.find t.back  ~f
    | some -> some
  ;;

  let find_map t ~f =
    match     List.find_map t.front ~f with
    | None -> List.find_map t.back  ~f
    | some -> some
  ;;

  let max_elt t ~compare =
    match List.max_elt t.front ~compare, List.max_elt t.back ~compare with
    | None, opt | opt, None -> opt
    | (Some x as some_x), (Some y as some_y) ->
      if compare x y >= 0 then some_x else some_y
  ;;

  let min_elt t ~compare =
    match List.min_elt t.front ~compare, List.min_elt t.back ~compare with
    | None, opt | opt, None -> opt
    | (Some x as some_x), (Some y as some_y) ->
      if compare x y <= 0 then some_x else some_y
  ;;

end

module Make_container(F : sig val to_list : 'a t -> 'a list end) = struct
  let to_list  = F.to_list
  let is_empty = is_empty
  let length   = length

  let mem t x ~equal = List.mem ~equal (to_list t) x

  let iter       t       ~f = List.iter       (to_list t)       ~f
  let fold       t ~init ~f = List.fold       (to_list t) ~init ~f
  let exists     t       ~f = List.exists     (to_list t)       ~f
  let for_all    t       ~f = List.for_all    (to_list t)       ~f
  let count      t       ~f = List.count      (to_list t)       ~f
  let sum m      t       ~f = List.sum m      (to_list t)       ~f
  let find       t       ~f = List.find       (to_list t)       ~f
  let find_map   t       ~f = List.find_map   (to_list t)       ~f
  let to_array   t          = List.to_array   (to_list t)
  let min_elt    t ~compare = List.min_elt    (to_list t) ~compare
  let max_elt    t ~compare = List.max_elt    (to_list t) ~compare
  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
  let fold_until  t ~init ~f = Container.fold_until  ~fold ~init ~f t

end

module Front_to_back = struct

  let of_list list = make ~length:(List.length list) ~front:list ~back:[]
  let to_list t = t.front @ List.rev t.back

  let%test_unit _ =
    [%test_result: int list]
      ~expect:[1;2;3]
      (to_list (of_list [1;2;3]))

  include Make_container(struct let to_list = to_list end)

end

module Back_to_front = struct

  let to_list t = t.back @ List.rev t.front
  let of_list list = make ~length:(List.length list) ~back:list ~front:[]

  let%test_unit _ =
    [%test_result: int list]
      ~expect:[1;2;3]
      (to_list (of_list [1;2;3]))

  include Make_container(struct let to_list = to_list end)

end

include Front_to_back

let singleton x = of_list [x]

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let bind t ~f =
      fold t ~init:empty ~f:(fun t elt ->
        fold (f elt) ~init:t ~f:enqueue_back)

    let return = singleton

    let map = `Custom (fun t ~f ->
      { front  = List.map t.front ~f
      ; back   = List.map t.back  ~f
      ; length = t.length
      })
  end)

let%test_unit _ =
  [%test_result: int list]
    ~expect:[1;2;3;4]
    (bind (of_list [[1;2];[3;4]]) ~f:of_list
     |> to_list)

let%test_unit _ =
  [%test_result: int list]
    ~expect:[2;3;4;5]
    (map (of_list [1;2;3;4]) ~f:succ
     |> to_list)

let compare cmp t1 t2 =
  List.compare cmp
    (to_list t1)
    (to_list t2)

let hash_fold_t hash_fold_a state t =
  fold ~f:hash_fold_a ~init:([%hash_fold: int] state (length t)) t

module Stable = struct

  module V1 = struct

    type nonrec 'a t = 'a t

    let compare = compare

    let sexp_of_t sexp_of_elt t =
      [%sexp_of: elt list] (to_list t)

    let t_of_sexp elt_of_sexp sexp =
      of_list ([%of_sexp: elt list] sexp)

    let map = map

    let%test_unit _ =
      [%test_result: int list]
        ~expect:[1;2;3]
        (to_list
           (t_of_sexp Int.t_of_sexp
              (sexp_of_t Int.sexp_of_t
                 (of_list [1;2;3]))))
    ;;

    include Bin_prot.Utils.Make_iterable_binable1 (struct
        type nonrec 'a t = 'a t
        type 'a el = 'a [@@deriving bin_io]
        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "83f96982-4992-11e6-919d-fbddcfdca576"
        let module_name = Some "Core_kernel.Fdeque"
        let length = length
        let iter t ~f = List.iter (to_list t) ~f
        let init ~len ~next =
          let rec loop next acc n =
            if len = n
            then acc
            else
              begin
                assert (n = length acc);
                let x = next () in
                loop next (enqueue_back acc x) (n + 1)
              end
          in loop next empty 0
      end)

  end

end

include (Stable.V1 : module type of Stable.V1 with type 'a t := 'a t)
