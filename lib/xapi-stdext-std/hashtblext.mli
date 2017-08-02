(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

(* this is not a fold ... *)
val fold_keys : ('a, 'b) Hashtbl.t -> 'a list

(* ... neither is this *)
val fold_values : ('a, 'b) Hashtbl.t -> 'b list

val add_empty : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
val add_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list -> unit
val remove_other_keys : ('a, 'b) Hashtbl.t -> 'a list -> unit
val of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
