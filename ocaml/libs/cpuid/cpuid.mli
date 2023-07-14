(*
 * Copyright (C) Cloud Software Group.
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

exception InvalidFeatureString of string

val string_of_features : int64 array -> string

val features_of_string : string -> int64 array

val extend : int64 array -> int64 array -> int64 array

val zero_extend : int64 array -> int -> int64 array

val intersect : int64 array -> int64 array -> int64 array

val diff : int64 array -> int64 array -> int64 array

val is_equal : int64 array -> int64 array -> bool

val is_subset : int64 array -> int64 array -> bool

val is_strict_subset : int64 array -> int64 array -> bool
