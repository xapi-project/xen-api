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

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

type 'a element =
  | Vertex of 'a
  | Component of 'a * 'a t

and 'a t = 'a element list

let fold_left = List.fold_left

module Make (G : G) = struct

  let recursive_scc g root_g =
    (* Straight OCaml implementation of the Section 4.3,
       fig. 4 algorithm in Bourdoncle's paper *)
    let stack = Stack.create () in
    let dfn = Hashtbl.create 1024 in
    let num = ref 0 in
    let partition = ref [] in

    G.iter_vertex (fun v -> Hashtbl.add dfn v 0) g;

    let rec visit vertex partition =
      let head = ref 0 in
      let loop = ref false in
      Stack.push vertex stack;
      incr num;
      Hashtbl.replace dfn vertex !num;
      head := !num;
      G.iter_succ
        (fun succ ->
           let dfn_succ = Hashtbl.find dfn succ in
           let min =
             if dfn_succ = 0
             then visit succ partition
             else dfn_succ in
           if min <= !head then begin
             head := min;
             loop := true
           end)
        g vertex;
      if !head = Hashtbl.find dfn vertex
      then begin
        Hashtbl.replace dfn vertex max_int;
        let element = ref (Stack.pop stack) in
        if !loop then begin
          while G.V.compare !element vertex <> 0 do
            Hashtbl.replace dfn !element 0;
            element := Stack.pop stack;
          done;
          partition := component vertex :: !partition;
        end
        else partition := Vertex vertex :: !partition
      end;
      !head

    and component vertex =
      let partition = ref [] in
      G.iter_succ
        (fun succ ->
           if Hashtbl.find dfn succ = 0
           then ignore (visit succ partition : int))
        g vertex;
      Component (vertex, !partition)

    in
    let (_ : int) = visit root_g partition in
    !partition

end
