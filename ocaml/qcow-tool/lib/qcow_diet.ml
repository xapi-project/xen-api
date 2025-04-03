(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
(*
#require "ppx_sexp_conv";;
#require "lwt";;
*)
open Sexplib.Std

module type ELT = sig
  type t [@@deriving sexp]

  val compare : t -> t -> int

  val zero : t

  val pred : t -> t

  val succ : t -> t

  val sub : t -> t -> t

  val add : t -> t -> t
end

exception Interval_pairs_should_be_ordered of string

exception Intervals_should_not_overlap of string

exception Intervals_should_not_be_adjacent of string

exception Height_not_equals_depth of string

exception Unbalanced of string

exception Cardinal of string

let _ =
  Printexc.register_printer (function
    | Interval_pairs_should_be_ordered txt ->
        Some ("Pairs within each interval should be ordered: " ^ txt)
    | Intervals_should_not_overlap txt ->
        Some ("Intervals should be ordered without overlap: " ^ txt)
    | Intervals_should_not_be_adjacent txt ->
        Some ("Intervals should not be adjacent: " ^ txt)
    | Height_not_equals_depth txt ->
        Some ("The height is not being maintained correctly: " ^ txt)
    | Unbalanced txt ->
        Some ("The tree has become imbalanced: " ^ txt)
    | Cardinal txt ->
        Some ("The cardinal value stored in the node is wrong: " ^ txt)
    | _ ->
        None
    )

module Make (Elt : ELT) = struct
  type elt = Elt.t [@@deriving sexp]

  module Elt = struct
    include Elt

    let ( - ) = sub

    let ( + ) = add
  end

  type interval = elt * elt

  module Interval = struct
    let make x y =
      if x > y then invalid_arg "Interval.make" ;
      (x, y)

    let x = fst

    let y = snd
  end

  let ( > ) x y = Elt.compare x y > 0

  let ( >= ) x y = Elt.compare x y >= 0

  let ( < ) x y = Elt.compare x y < 0

  let ( <= ) x y = Elt.compare x y <= 0

  let eq x y = Elt.compare x y = 0

  let succ, pred = (Elt.succ, Elt.pred)

  type t = Empty | Node : node -> t

  and node = {x: elt; y: elt; l: t; r: t; h: int; cardinal: elt}
  [@@deriving sexp]

  let height = function Empty -> 0 | Node n -> n.h

  let cardinal = function Empty -> Elt.zero | Node n -> n.cardinal

  let create x y l r =
    let h = max (height l) (height r) + 1 in
    let cardinal = Elt.(succ (y - x) + cardinal l + cardinal r) in
    Node {x; y; l; r; h; cardinal}

  let rec node x y l r =
    let hl = height l and hr = height r in
    let open Stdlib in
    if hl > hr + 2 then
      match l with
      | Empty ->
          assert false
      | Node {x= lx; y= ly; l= ll; r= lr; _} -> (
          if height ll >= height lr then
            node lx ly ll (node x y lr r)
          else
            match lr with
            | Empty ->
                assert false
            | Node {x= lrx; y= lry; l= lrl; r= lrr; _} ->
                node lrx lry (node lx ly ll lrl) (node x y lrr r)
        )
    else if hr > hl + 2 then
      match r with
      | Empty ->
          assert false
      | Node {x= rx; y= ry; l= rl; r= rr; _} -> (
          if height rr >= height rl then
            node rx ry (node x y l rl) rr
          else
            match rl with
            | Empty ->
                assert false
            | Node {x= rlx; y= rly; l= rll; r= rlr; _} ->
                node rlx rly (node x y l rll) (node rx ry rlr rr)
        )
    else
      create x y l r

  let depth tree =
    let rec depth tree k =
      match tree with
      | Empty ->
          k 0
      | Node n ->
          depth n.l (fun dl -> depth n.r (fun dr -> k (1 + max dl dr)))
    in
    depth tree (fun d -> d)

  let to_string_internal t = Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t t

  module Invariant = struct
    (* The pairs (x, y) in each interval are ordered such that x <= y *)
    let rec ordered t =
      match t with
      | Empty ->
          ()
      | Node {x; y; l; r; _} ->
          if x > y then
            raise (Interval_pairs_should_be_ordered (to_string_internal t)) ;
          ordered l ;
          ordered r

    (* The intervals don't overlap *)
    let rec no_overlap t =
      match t with
      | Empty ->
          ()
      | Node {x; y; l; r; _} ->
          ( match l with
          | Empty ->
              ()
          | Node left ->
              if left.y >= x then
                raise (Intervals_should_not_overlap (to_string_internal t))
          ) ;
          ( match r with
          | Empty ->
              ()
          | Node right ->
              if right.x <= y then
                raise (Intervals_should_not_overlap (to_string_internal t))
          ) ;
          no_overlap l ; no_overlap r

    let rec no_adjacent t =
      let biggest = function Empty -> None | Node {y; _} -> Some y in
      let smallest = function Empty -> None | Node {x; _} -> Some x in
      match t with
      | Empty ->
          ()
      | Node {x; y; l; r; _} ->
          ( match biggest l with
          | Some ly when Elt.succ ly >= x ->
              raise (Intervals_should_not_be_adjacent (to_string_internal t))
          | _ ->
              ()
          ) ;
          ( match smallest r with
          | Some rx when Elt.pred rx <= y ->
              raise (Intervals_should_not_be_adjacent (to_string_internal t))
          | _ ->
              ()
          ) ;
          no_adjacent l ; no_adjacent r

    (* The height is being stored correctly *)
    let rec height_equals_depth t =
      if height t <> depth t then
        raise (Height_not_equals_depth (to_string_internal t)) ;
      match t with
      | Empty ->
          ()
      | Node {l; r; _} ->
          height_equals_depth l ; height_equals_depth r

    let rec balanced = function
      | Empty ->
          ()
      | Node {l; r; _} as t ->
          let diff = height l - height r in
          let open Stdlib in
          if diff > 2 || diff < -2 then (
            Printf.fprintf stdout "height l = %d = %s\n" (height l)
              (to_string_internal l) ;
            Printf.fprintf stdout "height r = %d = %s\n" (height r)
              (to_string_internal r) ;
            raise (Unbalanced (to_string_internal t))
          ) ;
          balanced l ;
          balanced r

    let rec check_cardinal = function
      | Empty ->
          ()
      | Node {x; y; l; r; cardinal= c; _} as t ->
          check_cardinal l ;
          check_cardinal r ;
          if Elt.(c - cardinal l - cardinal r - y + x) <> Elt.(succ zero) then
            raise (Cardinal (to_string_internal t))

    let check t =
      ordered t ;
      no_overlap t ;
      height_equals_depth t ;
      balanced t ;
      check_cardinal t ;
      no_adjacent t
  end

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec mem elt = function
    | Empty ->
        false
    | Node n ->
        (* consider this interval *)
        (elt >= n.x && elt <= n.y)
        ||
        (* or search left or search right *)
        if elt < n.x then mem elt n.l else mem elt n.r

  let rec min_elt = function
    | Empty ->
        raise Not_found
    | Node {x; y; l= Empty; _} ->
        (x, y)
    | Node {l; _} ->
        min_elt l

  let rec max_elt = function
    | Empty ->
        raise Not_found
    | Node {x; y; r= Empty; _} ->
        (x, y)
    | Node {r; _} ->
        max_elt r

  let choose = function Empty -> raise Not_found | Node {x; y; _} -> (x, y)

  (* fold over the maximal contiguous intervals *)
  let rec fold f t acc =
    match t with
    | Empty ->
        acc
    | Node n ->
        let acc = fold f n.l acc in
        let acc = f (n.x, n.y) acc in
        fold f n.r acc

  let rec fold_s f t acc =
    match t with
    | Empty ->
        Lwt.return acc
    | Node n ->
        let open Lwt.Infix in
        fold_s f n.l acc >>= fun acc ->
        f (n.x, n.y) acc >>= fun acc -> fold_s f n.r acc

  (* fold over individual elements *)
  let fold_individual f t acc =
    let range (from, upto) acc =
      let rec loop acc x =
        if eq x (succ upto) then acc else loop (f x acc) (succ x)
      in
      loop acc from
    in
    fold range t acc

  let elements t = fold_individual (fun x acc -> x :: acc) t [] |> List.rev

  (* return (x, y, l) where (x, y) is the maximal interval and [l] is
     the rest of the tree on the left (whose intervals are all smaller). *)
  let rec splitMax = function
    | {x; y; l; r= Empty; _} ->
        (x, y, l)
    | {r= Node r; _} as n ->
        let u, v, r' = splitMax r in
        (u, v, node n.x n.y n.l r')

  (* return (x, y, r) where (x, y) is the minimal interval and [r] is
     the rest of the tree on the right (whose intervals are all larger) *)
  let rec splitMin = function
    | {x; y; l= Empty; r; _} ->
        (x, y, r)
    | {l= Node l; _} as n ->
        let u, v, l' = splitMin l in
        (u, v, node n.x n.y l' n.r)

  let addL = function
    | {l= Empty; _} as n ->
        n
    | {l= Node l; _} as n ->
        (* we might have to merge the new element with the maximal interval from
           the left *)
        let x', y', l' = splitMax l in
        if eq (succ y') n.x then {n with x= x'; l= l'} else n

  let addR = function
    | {r= Empty; _} as n ->
        n
    | {r= Node r; _} as n ->
        (* we might have to merge the new element with the minimal interval on
           the right *)
        let x', y', r' = splitMin r in
        if eq (succ n.y) x' then {n with y= y'; r= r'} else n

  let rec add (x, y) t =
    if y < x then invalid_arg "interval reversed" ;
    match t with
    | Empty ->
        node x y Empty Empty
    (* completely to the left *)
    | Node n when y < Elt.pred n.x ->
        let l = add (x, y) n.l in
        node n.x n.y l n.r
    (* completely to the right *)
    | Node n when Elt.succ n.y < x ->
        let r = add (x, y) n.r in
        node n.x n.y n.l r
    (* overlap on the left only *)
    | Node n when x < n.x && y <= n.y ->
        let l = add (x, pred n.x) n.l in
        let n = addL {n with l} in
        node n.x n.y n.l n.r
    (* overlap on the right only *)
    | Node n when y > n.y && x >= n.x ->
        let r = add (succ n.y, y) n.r in
        let n = addR {n with r} in
        node n.x n.y n.l n.r
    (* overlap on both sides *)
    | Node n when x < n.x && y > n.y ->
        let l = add (x, pred n.x) n.l in
        let r = add (succ n.y, y) n.r in
        let n = addL {(addR {n with r}) with l} in
        node n.x n.y n.l n.r
    (* completely within *)
    | Node n ->
        Node n

  let union a b =
    let a' = cardinal a and b' = cardinal b in
    if a' > b' then
      fold add b a
    else
      fold add a b

  let merge l r =
    match (l, r) with
    | l, Empty ->
        l
    | Empty, r ->
        r
    | Node l, r ->
        let x, y, l' = splitMax l in
        node x y l' r

  let rec remove (x, y) t =
    if y < x then invalid_arg "interval reversed" ;
    match t with
    | Empty ->
        Empty
    (* completely to the left *)
    | Node n when y < n.x ->
        let l = remove (x, y) n.l in
        node n.x n.y l n.r
    (* completely to the right *)
    | Node n when n.y < x ->
        let r = remove (x, y) n.r in
        node n.x n.y n.l r
    (* overlap on the left only *)
    | Node n when x < n.x && y < n.y ->
        let n' = node (succ y) n.y n.l n.r in
        remove (x, pred n.x) n'
    (* overlap on the right only *)
    | Node n when y > n.y && x > n.x ->
        let n' = node n.x (pred x) n.l n.r in
        remove (succ n.y, y) n'
    (* overlap on both sides *)
    | Node n when x <= n.x && y >= n.y ->
        let l = remove (x, n.x) n.l in
        let r = remove (n.y, y) n.r in
        merge l r
    (* completely within *)
    | Node n when eq y n.y ->
        node n.x (pred x) n.l n.r
    | Node n when eq x n.x ->
        node (succ y) n.y n.l n.r
    | Node n ->
        assert (n.x <= pred x) ;
        assert (succ y <= n.y) ;
        let r = node (succ y) n.y Empty n.r in
        node n.x (pred x) n.l r

  let diff a b = fold remove b a

  let inter a b = diff a (diff a b)

  let take t n =
    let rec loop acc free n =
      if n = Elt.zero then
        Some (acc, free)
      else
        match
          try
            let i = choose free in
            let x, y = Interval.(x i, y i) in
            let len = Elt.(succ @@ (y - x)) in
            let will_use = if Stdlib.(Elt.compare n len < 0) then n else len in
            let i' = Interval.make x Elt.(pred @@ (x + will_use)) in
            Some (add i' acc, remove i' free, Elt.(n - will_use))
          with Not_found -> None
        with
        | Some (acc', free', n') ->
            loop acc' free' n'
        | None ->
            None
    in
    loop empty t n
end

module Int = struct
  type t = int [@@deriving sexp]

  let compare (x : t) (y : t) = Stdlib.compare x y

  let zero = 0

  let succ x = x + 1

  let pred x = x - 1

  let add x y = x + y

  let sub x y = x - y
end

module IntDiet = Make (Int)
module IntSet = Set.Make (Int)

module Test = struct
  let check_depth n =
    let init = IntDiet.add (IntDiet.Interval.make 0 n) IntDiet.empty in
    (* take away every other block *)
    let rec sub m acc =
      (* Printf.printf "acc = %s\n%!" (IntDiet.to_string_internal acc); *)
      if m <= 0 then
        acc
      else
        sub (m - 2) IntDiet.(remove (Interval.make m m) acc)
    in
    let set = sub n init in
    let d = IntDiet.height set in
    if d > int_of_float (log (float_of_int n) /. log 2.) + 1 then
      failwith "Depth larger than expected" ;
    let set = sub (n - 1) set in
    let d = IntDiet.height set in
    assert (d == 1)

  let make_random n m =
    let rec loop set diet = function
      | 0 ->
          (set, diet)
      | m ->
          let r = Random.int n in
          let r' = Random.int (n - r) + r in
          let add = Random.bool () in
          let rec range from upto =
            if from > upto then [] else from :: range (from + 1) upto
          in
          let set =
            List.fold_left
              (fun set elt ->
                (if add then IntSet.add else IntSet.remove) elt set
              )
              set (range r r')
          in
          let diet' =
            (if add then IntDiet.add else IntDiet.remove)
              (IntDiet.Interval.make r r')
              diet
          in
          ( try IntDiet.Invariant.check diet'
            with e ->
              Printf.fprintf stderr "%s %d\nBefore: %s\nAfter: %s\n"
                (if add then "Add" else "Remove")
                r
                (IntDiet.to_string_internal diet)
                (IntDiet.to_string_internal diet') ;
              raise e
          ) ;
          loop set diet' (m - 1)
    in
    loop IntSet.empty IntDiet.empty m
  (*
  let set_to_string set =
    String.concat "; " @@ List.map string_of_int @@ IntSet.elements set
  let diet_to_string diet =
    String.concat "; " @@ List.map string_of_int @@ IntDiet.elements diet
    *)

  let check_equals set diet =
    let set' = IntSet.elements set in
    let diet' = IntDiet.elements diet in
    if set' <> diet' then
      (*
      Printf.fprintf stderr "Set contains: [ %s ]\n" @@ set_to_string set;
      Printf.fprintf stderr "Diet contains: [ %s ]\n" @@ diet_to_string diet;
      *)
      failwith "check_equals"

  let test_adds () =
    for _ = 1 to 100 do
      let set, diet = make_random 1000 1000 in
      ( try IntDiet.Invariant.check diet
        with e ->
          (*
          Printf.fprintf stderr "Diet contains: [ %s ]\n" @@ IntDiet.to_string_internal diet;
          *)
          raise e
      ) ;
      check_equals set diet
    done

  let test_operator set_op diet_op () =
    for _ = 1 to 100 do
      let set1, diet1 = make_random 1000 1000 in
      let set2, diet2 = make_random 1000 1000 in
      check_equals set1 diet1 ;
      check_equals set2 diet2 ;
      let set3 = set_op set1 set2 in
      let diet3 = diet_op diet1 diet2 in
      (*
      Printf.fprintf stderr "diet1 = %s\n" (IntDiet.to_string_internal diet1);
      Printf.fprintf stderr "diet3 = %s\n" (IntDiet.to_string_internal diet2);
      Printf.fprintf stderr "diet2 = %s\n" (IntDiet.to_string_internal diet3);
      *)
      check_equals set3 diet3
    done

  let test_add_1 () =
    let open IntDiet in
    assert (elements @@ add (3, 4) @@ add (3, 3) empty = [3; 4])

  let test_remove_1 () =
    let open IntDiet in
    assert (elements @@ remove (6, 7) @@ add (7, 8) empty = [8])

  let test_remove_2 () =
    let open IntDiet in
    assert (
      elements @@ diff (add (9, 9) @@ add (5, 7) empty) (add (7, 9) empty)
      = [5; 6]
    )

  let test_adjacent_1 () =
    let open IntDiet in
    let set = add (9, 9) @@ add (8, 8) empty in
    IntDiet.Invariant.check set

  let test_depth () = check_depth 1048576

  let all =
    [
      ("adding an element to the right", test_add_1)
    ; ("removing an element on the left", test_remove_1)
    ; ("removing an elements from two intervals", test_remove_2)
    ; ("test adjacent intervals are coalesced", test_adjacent_1)
    ; ("logarithmic depth", test_depth)
    ; ("adding and removing elements acts like a Set", test_adds)
    ; ("union", test_operator IntSet.union IntDiet.union)
    ; ("diff", test_operator IntSet.diff IntDiet.diff)
    ; ("intersection", test_operator IntSet.inter IntDiet.inter)
    ]
end
