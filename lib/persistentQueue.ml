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

(** Purely applicative queues implemented in a standard way
    using a pair of lists (Hood & Melville 1981) *)

type 'a t = 'a list * 'a list
  (** a queue is a pair (prefix, xiffus), with elements popped from prefix
      and inserted into xiffus
      invariant: prefix=[] -> xiffus=[] *)

let empty = [], []

let is_empty (prefix, _) =
  prefix = []

let add queue elt = match queue with
  | [], [] -> [elt], []
  | prefix, xiffus -> prefix, elt :: xiffus

let head = function
  | head :: _, _-> head
  | [], _ -> raise Not_found

let tail = function
  | [_], xiffus -> List.rev xiffus, []
  | _ :: prefix, xiffus -> prefix, xiffus
  | [], _ -> raise Not_found
