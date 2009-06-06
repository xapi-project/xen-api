(* 
 * Copyright (c) Citrix Systems 2008. All rights reserved 
 * Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>
 *)

(** Node names *)

(** Xenstore nodes names are often the same, ie. "local", "domain", "device", ... so it is worth to 
    manipulate them through the use of small identifiers that we call symbols. These symbols can be 
    compared in constant time (as opposite to strings) and should help the ocaml GC. *)

type t
(** The type of symbols. *)

val of_string : string -> t
(** Convert a string into a symbol. *)

val to_string : t -> string
(** Convert a symbol into a string. *)

(** {6 Garbage Collection} *)

(** Symbols need to be regulary garbage collected. The following steps should be followed:
-     mark all the knowns symbols as unused (with [mark_all_as_unused]);
-     mark all the symbols really usefull as used (with [mark_as_used]); and
-     finally, call [garbage] *)

val mark_all_as_unused : unit -> unit
val mark_as_used : t -> unit
val garbage : unit -> unit

(** {6 Statistics } *)

val stats : unit -> int
(** Get the number of used symbols. *)

val created : unit -> int
(** Returns the number of symbols created since the last GC. *)

val used : unit -> int
(** Returns the number of existing symbols used since the last GC *)
