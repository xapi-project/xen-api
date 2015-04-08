(*
 * Copyright (c) Citrix Systems Inc.
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
 *)

module Make = functor(A: Map.OrderedType) -> functor(B: Map.OrderedType) -> struct
  (** Store a (a: A.t, b:B.t) relation R such that given
      		an a, finding the largest set bs such that
           \forall b \in bs. (a, b)\in R
      		and v.v. on b are efficient. *)

  module A_Set = Set.Make(A)
  module A_Map = Map.Make(A)
  module B_Set = Set.Make(B)
  module B_Map = Map.Make(B)

  type t = {
    a_to_b: B_Set.t A_Map.t;
    b_to_a: A_Set.t B_Map.t;
  }
  let empty = {
    a_to_b = A_Map.empty;
    b_to_a = B_Map.empty;
  }
  let get_bs a t =
    if A_Map.mem a t.a_to_b
    then A_Map.find a t.a_to_b
    else B_Set.empty
  let get_as b t =
    if B_Map.mem b t.b_to_a
    then B_Map.find b t.b_to_a
    else A_Set.empty

  let add a b t =
    {
      a_to_b = A_Map.add a (B_Set.add b (get_bs a t)) t.a_to_b;
      b_to_a = B_Map.add b (A_Set.add a (get_as b t)) t.b_to_a;
    }
  let of_list = List.fold_left (fun t (a, b) -> add a b t) empty
  let to_list t = A_Map.fold (fun a bs acc -> B_Set.fold (fun b acc -> (a, b) :: acc) bs acc) t.a_to_b []

  let remove_a a t =
    let bs = get_bs a t in
    {
      a_to_b = A_Map.remove a t.a_to_b;
      b_to_a =
        B_Set.fold
          (fun b acc ->
             let as' =
               if B_Map.mem b acc
               then B_Map.find b acc
               else A_Set.empty in
             let as' = A_Set.remove a as' in
             if as' = A_Set.empty
             then B_Map.remove b acc
             else B_Map.add b as' acc
          ) bs t.b_to_a;
    }
  let remove_b b t =
    let as' = get_as b t in
    {
      a_to_b =
        A_Set.fold
          (fun a acc ->
             let bs =
               if A_Map.mem a acc
               then A_Map.find a acc
               else B_Set.empty in
             let bs = B_Set.remove b bs in
             if bs = B_Set.empty
             then A_Map.remove a acc
             else A_Map.add a bs acc
          ) as' t.a_to_b;
      b_to_a = B_Map.remove b t.b_to_a;
    }

  let equal t1 t2 =
    true
    && A_Map.equal B_Set.equal t1.a_to_b t2.a_to_b
    && B_Map.equal A_Set.equal t1.b_to_a t2.b_to_a

end
