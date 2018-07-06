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

(** [is_all_zeroes x len] returns true if the substring is all zeroes *)
external is_all_zeros : string -> int -> bool = "is_all_zeros"

(** [find_a_zero x len offset] returns the offset in [x] of a zero
    character after [offset], or None if no zero was detected.
    Note this function is approximate and is not guaranteed to find
    strictly the first zero. *)
val find_a_zero: string -> int -> int -> int option

(** [find_a_nonzero x len offset] returns the offset in [x] of a 
    nonzero character after [offset], or None if none could be detected.
    Note this function is approximate and is not guaranteed to find
    strictly the first nonzero. *)
val find_a_nonzero: string -> int -> int -> int option

type substring = {
  buf: string;
  offset: int;
  len: int
}

(** [fold_over_nonzeros buf len rounddown roundup f initial] folds [f] over all 
    (start, length) pairs of non-zero data in string [buf] up to [len]. 
    	The start of each pair is rounded down with [rounddown] and 
    the end offset of each pair is rounded up with [roundup] (e.g. to 
    potential block boudaries. *)
val fold_over_nonzeros: string -> int -> (int -> int) -> (int -> int) -> ('a -> substring -> 'a) -> 'a -> 'a
