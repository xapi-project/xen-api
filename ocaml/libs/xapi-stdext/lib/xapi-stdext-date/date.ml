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

include Clock.Date

let never = epoch

let of_string = of_iso8601

let to_string = to_rfc3339

let of_float = of_unix_time

let to_float = to_unix_time

let rfc822_of_float = of_unix_time

let rfc822_to_string = to_rfc822

let eq = equal

type iso8601 = t

type rfc822 = t
