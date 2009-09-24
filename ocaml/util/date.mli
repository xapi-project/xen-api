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
type iso8601
val of_float : float -> iso8601
val to_float : iso8601 -> float
val to_string : iso8601 -> string
val of_string : string -> iso8601
val never: iso8601

type rfc822
val rfc822_of_float : float -> rfc822
val rfc822_to_string : rfc822 -> string
