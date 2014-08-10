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

module Make : functor (A : Map.OrderedType) -> functor (B : Map.OrderedType) -> sig

  type t
  (** a relation (A.t * B.t) relation R such that given an a: A.t,
      finding the largest set B_Set.t such that
      \forall b \in B_Set.t (a: A.t, b: B.t) \in R
      and v.v on B are efficient. *)

  val empty: t
  (** an empty relation *)

  val add: A.t -> B.t -> t -> t
  (** [add a b t] returns the relation t' including all elements of t
      plus (a, b) *)

  val equal: t -> t -> bool
  (** [equal t t'] true if both t and t' represent the same relation *)

  module A_Set : module type of Set.Make(A)

  module B_Set : module type of Set.Make(B)

  val get_as: B.t -> t -> A_Set.t
  (** [get_as b t] returns the largest set of elements A such that
      \forall a\in A. (a, b) \in t *)

  val get_bs: A.t -> t -> B_Set.t
  (** [get_bs a t] returns the largest set of elements B such that
      for all b\in B. (a, b) \in t *)

  val of_list: (A.t * B.t) list -> t
  (** [of_list abs] returns the smallest relation t where
      \forall (a, b) \in abs. (a, b) \in t *)

  val to_list: t -> (A.t * B.t) list
  (** [to_list abs] returns a list of pairs such that
      equal x (of_list (to_list x)) *)

  val remove_a: A.t -> t -> t
  (** [remove_a a' t] creates the relation R such that
      (a, b) \in R iff (a, b) \in t and a <> a' *)

  val remove_b: B.t -> t -> t
  (** [remove_b b' t] creates the relation R such that
      (a, b) \in R iff (a, b) \in t and b <> b' *)

end
