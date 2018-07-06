(* We implement a weak pointer using a [Weak.t]. *)

open! Import
open! Std_internal

type 'a t = 'a Weak.t

let create () = Weak.create ~len:1

(* We use a weak set of length 1, so the weak pointer is at index 0. *)
let index = 0

let get t = Weak.get t index

let sexp_of_t sexp_of_a t = [%sexp (get t : a Heap_block.t option)]

let is_none t = Weak.is_none t index
let is_some t = Weak.is_some t index

let set t block = Weak.set t index (Some block)
