(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Copyright © 2015 Thibault Suzanne <thi.suzanne (@) gmail.com>
 * École Normale Supérieure, Département d'Informatique
 * Paris Sciences et Lettres
*)

(* Original algorithm by François Bourdoncle. See :
 * "Efficient chaotic iteration strategies with widenings",
 * Formal Methods in Programming and their Applications,
 * Springer Berlin Heidelberg, 1993.
*)

(** Weak topological ordering of the vertices of a graph, as described
    by François Bourdoncle.

    Weak topological ordering is an extension of topological ordering
    for potentially cyclic graphs.

    A hierarchical ordering of a set is a well-parenthesized
    permutation of its elements with no consecutive [(]. The elements
    between two parentheses are called a {e component}, and the first
    elements of a component is called the {e head}. A weak topological
    ordering of a graph is a hierarchical ordering of its vertices
    such that for every edge [u -> v] of the graph, either [u] comes
    (strictly) before [v], or [v] is the head of a component
    containing [u].

    One may notice that :
    - For an acyclic graph, every topological ordering is also a weak
      topological ordering.
    - For any graph with the vertices [v1, ..., vN], the following
      trivial weak topological ordering is valid : [(v1 (v2
      (... (vN))...))].

    Weak topological ordering are useful for fixpoint computation (see
    {!ChaoticIteration}). This module aims at computing weak
    topological orderings which improve the precision and the
    convergence speed of these analyses.

    @author Thibault Suzanne
    @see "Efficient chaotic iteration strategies with widenings",
    François Bourdoncle,
    Formal Methods in Programming and their Applications,
    Springer Berlin Heidelberg, 1993
*)

(** Minimal graph signature for the algorithm *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

(** The type of the elements of a weak topological ordering over a set
    of ['a].

    - [Vertex v] represents a single vertex.
    - [Component (head, cs)] is a component of the wto, that is
    a sequence of elements between two parentheses. [head] is the head
    of the component, that is the first element, which is guaranteed to
    be a vertex by definition. [cs] is the rest of the component.
*)
type 'a element =
  | Vertex of 'a
  | Component of 'a * 'a t

and 'a t
(** The type of a sequence of outermost elements in a weak topological
    ordering. This is also the type of a weak topological ordering
    over a set of ['a].
*)

val fold_left : ('a -> 'b element -> 'a) -> 'a -> 'b t -> 'a
(** Folding over the elements of a weak topological ordering. They are
    given to the accumulating function according to their order.

    Note that as [element]s present in an ordering of type [t] can
    contain values of type [t] itself due to the [Component] variant,
    this function should be used by defining a recursive function [f],
    which will call [fold_left] with [f] used to define the first
    parameter.
*)

module Make : functor (G : G) -> sig

  val recursive_scc : G.t -> G.V.t -> G.V.t t
  (** [recursive_scc g root_g] computes a weak topological ordering of
      the vertices of [g], with the general algorithm recursively
      computing the strongly connected components of [g]. [root_g] is
      taken as the root of the graph and must be a valid vertex of
      [g].
  *)

end
