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

(** Fixpoint computation with widenings using weak topological
    orderings as defined by François Bourdoncle and implemented
    in {!WeakTopological}.

    {!Fixpoint} is another (simpler) fixpoint computation module, with
    general references.

    The general idea of fixpoint computation is to iteratively compute
    the result of the analysis a vertex from the results of its
    predecessors, until stabilisation is achieved on every vertex. The
    way to determine, at each step, the next vertex to analyse is
    called a {e chaotic iteration strategy}. A good strategy can make
    the analysis much faster. To enforce the termination of the
    analyse and speed it up when it terminates in too many steps, one
    can also use a {e widening}, to ensure that there is no infinite
    (nor too big) sequence of intermediary results for a given
    vertex. However, it usually results in a loss of precision, which
    is why choosing a good widening set (the set of points on which
    the widening will be performed) is mandatory.

    This module computes a fixpoint over a graph using weak
    topological ordering, which can be used to get both the iteration
    strategy and the widening set. The module {!WeakTopological} aims
    to compute weak topological orderings which are known to be
    excellent decompositions w.r.t these two critical points.

    @author Thibault Suzanne

    @see "Efficient chaotic iteration strategies with widenings",
    François Bourdoncle,
    Formal Methods in Programming and their Applications,
    Springer Berlin Heidelberg, 1993

*)


(** How to determine which vertices are to be considered as widening
    points.

    - [FromWto] indicates to use as widening points the heads of the
      weak topological ordering given as a parameter of the analysis
      function. This will always be a safe choice, and in most cases
      it will also be a good one with respect to the precision of the
      analysis.

    - [Predicate f] indicates to use [f] as the characteristic
      function of the widening set. [Predicate (fun _ -> false)] can
      be used if a widening is not needed. This variant can be used
      when there is a special knowledge of the graph to achieve
      a better precision of the analysis. For instance, if the graph
      happens to be the flow graph of a program, the predicate should
      be true for control structures heads. In any case, a condition
      for a safe widening predicate is that every cycle of the graph
      should go through at least one widening point. Otherwise, the
      analysis may not terminate. Note that even with a safe
      predicate, ensuring the termination does still require a correct
      widening definition.
*)
type 'a widening_set =
  | FromWto
  | Predicate of ('a -> bool)

(** Minimal graph signature for the algorithm.
    Sub-signature of [Traverse.G]. *)
module type G = sig
  type t

  module V : Sig.COMPARABLE

  module E : sig
    type t
    val src : t -> V.t
  end

  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

(** Parameters of the analysis. *)
module type Data = sig
  type t
  (** Information stored at each vertex. *)

  type edge
  (** Edge of the graph. *)

  val join : t -> t -> t
  (** Operation to join data when several paths meet. *)

  val equal : t -> t -> bool
  (** Equality test for data. *)

  val analyze : edge -> t -> t
  (** How to analyze one edge: given an edge and the data stored at
      its origin, it must compute the resulting data to be stored at
      its destination. *)

  val widening : t -> t -> t
  (** The widening operator. [fun _ x -> x] is correct and is
      equivalent to not doing widening. Note that to enforce
      termination, the following property should hold: for all
      sequence [x_0, x_1, ...] of data, the sequence defined by [y_0 =
      x_0; y_{i+1} = widening y_i x_i] stabilizes in finite time. *)
end

module Make
    (G : G)
    (D : Data with type edge = G.E.t) :
sig
  module M : Map.S with type key = G.V.t
  (** Map used to store the result of the analysis *)

  val recurse :
    G.t ->
    G.V.t WeakTopological.t ->
    (G.V.t -> D.t) ->
    G.V.t widening_set ->
    int ->
    D.t M.t
    (** [recurse g wto init widening_set widening_delay] computes the
        fixpoint of the analysis of a graph. This function uses the
        recursive iteration strategy: it recursively stabilizes the
        subcomponents of every component every time the component is
        stabilized (cf. Bourdoncle's paper).

        @param g The graph to analyse.

        @param wto A weak topological ordering of the vertices of [g].

        @param widening_set On which points to do the widening.

        @param widening_delay How many computations steps will be done
        before using widening to speed up the stabilisation. This
        counter is reset when entering each component, and is shared
        between all outermost vertices of this component. A negative
        value means [0].

        @param init How to compute the initial analysis data.

        @return A map from vertices of [g] to their analysis result.

    *)
end
