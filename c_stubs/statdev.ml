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
external _get_major_minor : string -> int * int * int = "stub_statdev_get_major_minor"

let get_major_minor path =
  let errno, major, minor = _get_major_minor path in
  if errno <> 0 then failwith (Printf.sprintf "Cannot stat path: %s (errno = %d)" path errno);
  major, minor
