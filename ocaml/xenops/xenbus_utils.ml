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
open Xenstore

type state = Unknown | Initialising | InitWait | Initialised | Connected
	     | Closing | Closed


let int_of = function
  | Unknown      -> 0
  | Initialising -> 1
  | InitWait     -> 2
  | Initialised  -> 3
  | Connected    -> 4
  | Closing      -> 5
  | Closed       -> 6

let of_int = function
  | 0 -> Unknown
  | 1 -> Initialising
  | 2 -> InitWait
  | 3 -> Initialised
  | 4 -> Connected
  | 5 -> Closing
  | 6 -> Closed
  | _ -> Unknown

let of_string x = of_int (int_of_string x)
let string_of x = string_of_int (int_of x)

let to_string_desc = function
	| Unknown      -> "unknown"
	| Initialising -> "initialising"
	| InitWait     -> "initwait"
	| Initialised  -> "initialised"
	| Connected    -> "connected"
	| Closing      -> "closing"
	| Closed       -> "closed"

(** Allows a guest to read/write this node and children *)
let rwperm_for_guest domid = 
	(domid, Xsraw.PERM_NONE, [])

(** Dom0 can read/write this node and children, domU can only read children *)
let roperm_for_guest domid =
	(0, Xsraw.PERM_NONE, [ (domid, Xsraw.PERM_READ) ])
