(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
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
