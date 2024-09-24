(*
 * Copyright (C) Cloud Software Group
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

let loadavg () =
  let split_colon line = Astring.String.fields ~empty:false line in
  let all = Xapi_stdext_unix.Unixext.string_of_file "/proc/loadavg" in
  try float_of_string (List.hd (split_colon all)) with _ -> -1.
