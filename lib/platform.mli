(*
 * Copyright (C) Citrix Systems Inc.
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

(* This module defines interpretation of boolean platform values. It
 * matches the interpretation implemented in XenAPI for these values.
*)

type platformdata = (string * string) list

(** [is_valid key platformdata] returns true if:
    	* 1. The key is _not_ in platformdata (absence of key is valid) or
    	* 2. The key is in platformdata, associated with a booleanish value *)
val is_valid: key:string -> platformdata:platformdata -> bool

(** [is_true key platformdata default] returns true, if the platformdata
    	*  contains a value for key that is "true" or "1". It returns false, if
    	* a value "0" or "false" exists. If the key doesn't exist or contains
    	* none of the values above, [default] is returned.
    	*)
val is_true:
  key:string ->
  platformdata:platformdata ->
  default:bool ->
  bool

