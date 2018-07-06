(* This code is based on the MLton library set/disjoint.fun, which has the
   following copyright notice.
*)
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
*)

open! Import

include struct
  let phys_equal = (==)
end

(*
   {v
           Root
             |
           Inner
        / .. | .. \
     Inner Inner Inner
     /|\   /|\   /|\
     ...   ...   ...
   v}

   We construct the `inverted' tree in the ML representation.
   The direction of the edges is UPWARDS.
   Starting with any ['a t] we can step directly to its parent.
   But we can't (and don't need to) start from the root and step to its children.
*)

(*
   [rank] is an upper bound on the depth of any node in the up-tree.

   Imagine an unlucky sequence of operations in which you create N
   individual [t]-values and then union them together in such a way
   that you always pick the root of each tree to union together, so that
   no path compression takes place.  If you don't take care to somehow
   balance the resulting up-tree, it is possible that you end up with one
   big long chain of N links, and then calling [representative] on the
   deepest node takes Theta(N) time.  With the balancing scheme of never
   increasing the rank of a node unnecessarily, it would take O(log N).
*)
type 'a root = {
  mutable value: 'a;
  mutable rank: int;
}

type 'a t = { mutable node : 'a node; }

and 'a node =
  | Inner of 'a t (* [Inner x] is a node whose parent is [x]. *)
  | Root of 'a root

let invariant t =
  let rec loop t depth =
    match t.node with
    | Inner t -> loop t (depth + 1)
    | Root r -> assert (depth <= r.rank)
  in
  loop t 0

let create v = { node = Root { value = v; rank = 0; }; }

(* invariants:
   [inner.node] = [inner_node] = [Inner t].
   [descendants] are the proper descendants of [inner] we've visited.
*)
let rec compress t ~inner_node ~inner ~descendants =
  match t.node with
  | Root r ->
    (* t is the root of the tree.
       Re-point all descendants directly to it by setting them to [Inner t].
       Note: we don't re-point [inner] as it already points there. *)
    List.iter descendants ~f:(fun t -> t.node <- inner_node); (t, r)
  | Inner t' as node ->
    compress t' ~inner_node:node ~inner:t ~descendants:(inner :: descendants)

let representative t =
  match t.node with
  | Root r -> (t, r)
  | Inner t' as node -> compress t' ~inner_node:node ~inner:t ~descendants:[]

let root t = snd (representative t)

let get t = (root t).value

let set t v = (root t).value <- v

let same_class t1 t2 = phys_equal (root t1) (root t2)

let union t1 t2 =
  let (t1, r1) = representative t1 in
  let (t2, r2) = representative t2 in
  if phys_equal r1 r2 then
    ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then
      t1.node <- Inner t2
    else begin
      t2.node <- Inner t1;
      if n1 = n2 then r1.rank <- r1.rank + 1;
    end

let%test_module _ =
  (module struct

    let is_compressed t =
      invariant t;
      match t.node with
      | Root _ -> true
      | Inner t ->
        match t.node with
        | Root _ -> true
        | Inner _ -> false
    ;;

    (* invariant checking wrapper functions *)

    let create x =
      let t = create x in
      assert (is_compressed t);
      t

    let union t1 t2 =
      union t1 t2;
      invariant t1;
      invariant t2;
      assert (is_compressed t1 || is_compressed t2);
    ;;

    let get t =
      let x = get t in
      assert (is_compressed t);
      x
    ;;

    let set t x =
      set t x;
      assert (is_compressed t);
    ;;

    let same_class t1 t2 =
      let b = same_class t1 t2 in
      assert (is_compressed t1);
      assert (is_compressed t2);
      b
    ;;

    let%test_unit "union" =
      let a = create 1 in
      let b = create 2 in
      assert (not (same_class a b));
      union a b;
      assert (same_class a b);
      let c = create 3 in
      assert (not (same_class a c));
      assert (not (same_class b c));
      union b c;
      assert (same_class a c);
      assert (same_class b c);
      let d = create 1 in
      let e = create 2 in
      let f = create 3 in
      union d e;
      union d f;
      assert (same_class d e);
      assert (same_class d f);
      assert (same_class e f)
    ;;

    let%test_unit "union" =
      let a = create 1 in
      let b = create 2 in
      union a b;
      let c = create 1 in
      let d = create 2 in
      union c d;
      union b d;
      assert (same_class a c)
    ;;

    let%test_unit "set/get" =
      let a = create 1 in
      let b = create 2 in
      assert (get a = 1);
      assert (get b = 2);
      union a b;
      set a 3;
      assert (get a = 3);
      assert (get b = 3)
    ;;

    let%test_unit "compressed" =
      let n = 1000 in
      let ts = List.init n ~f:create in
      let t = List.reduce_exn ts ~f:(fun a b -> union a b; b) in
      let max_rank = List.fold ts ~init:0 ~f:(fun acc t -> max acc (root t).rank) in
      assert (max_rank = 1);
      set t 42;
      assert (List.for_all ts ~f:(fun t' -> same_class t t' && get t' = 42))
    ;;

    let%test_unit "balanced" =
      let log2 n = int_of_float (ceil (log (float_of_int n) /. log 2.)) in
      let n = 1000 in
      let ts = Array.init n ~f:create in
      let rec sub i j =
        if i = j then ts.(i) else begin
          let k = (i + j) / 2 in
          let a = sub i k in
          if k+1 > j then a else begin
            let b = sub (k+1) j in
            union a b;
            a
          end
        end
      in
      let t = sub 0 (pred n) in
      Array.iter ts ~f:invariant;
      assert (Array.exists ts ~f:(fun t -> not (is_compressed t)));
      let max_rank = Array.fold ts ~init:0 ~f:(fun acc t -> max acc (root t).rank) in
      assert (max_rank <= log2 n);
      set t 42;
      assert (Array.for_all ts ~f:(fun t' -> same_class t t' && get t' = 42));
      assert (Array.for_all ts ~f:is_compressed)
    ;;
  end)
