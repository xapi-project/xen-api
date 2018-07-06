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

let ( |> ) x f = f x

type 'a widening_set =
  | FromWto
  | Predicate of ('a -> bool)

module type G = sig
  type t

  module V : Sig.COMPARABLE

  module E : sig
    type t
    val src : t -> V.t
  end

  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module type Data = sig
  type t

  type edge

  val join : t -> t -> t
  val equal : t -> t -> bool

  val analyze : edge -> t -> t

  val widening : t -> t -> t
end

module Make
    (G : G)
    (D : Data with type edge = G.E.t)
=
struct
  module M = Map.Make (G.V)

  let recurse g wto init widening_set widening_delay =

    (* The following two functions are predicates used to know whether
       to compute a widening when analysing a vertex which is not
       (resp. is) the head of a WTO component. They will be called
       only if the specified number of steps before widening has been
       done. *)
    let do_nonhead_widen v = match widening_set with
      | FromWto -> false
      | Predicate f -> f v
    in

    let do_head_widen v = match widening_set with
      | FromWto -> true
      | Predicate f -> f v
    in

    let find vertex data =
      try M.find vertex data
      with Not_found -> init vertex
    in

    let analyze_vertex widening_steps do_widen v data =
      (* Computes the result of the analysis for one vertex *)
      let result = G.fold_pred_e
          (fun edge acc ->
             let src = G.E.src edge in
             let data_src = find src data in
             let data_dst = D.analyze edge data_src in
             D.join data_dst acc)
          g v (init v) in
      if widening_steps <= 0 && do_widen v
      then D.widening (find v data) result
      else result
    in

    let rec analyze_elements widening_steps comp data =
      (* Computes the result of one iteration of the analysis of the
         elements of a component. *)
      WeakTopological.fold_left (analyze_element widening_steps) data comp

    and stabilize can_stop widening_steps head comps data =
      (* Iterates the analysis of a component until
         stabilisation. [can_stop] is [false] if no iteration has been
         made so far, since at least one is needed before ending with
         stabilisation. *)
      let old_data_head =
        find head data in
      let new_data_head =
        analyze_vertex widening_steps do_head_widen head data in
      if can_stop && D.equal old_data_head new_data_head
      then data
      else
        data
        |> M.add head new_data_head
        |> analyze_elements widening_steps comps
        |> stabilize true (widening_steps - 1) head comps

    and analyze_element widening_steps data = function
      (* Computes the result of the analysis of one element *)
      | WeakTopological.Vertex v ->
        M.add v (analyze_vertex widening_steps do_nonhead_widen v data) data
      | WeakTopological.Component (head, comps) ->
        stabilize false widening_delay head comps data
    in

    analyze_elements widening_delay wto M.empty
end
