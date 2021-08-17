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

(* Kerberos support several different encrytion types
 * winbind support it as strong, legacy and all
 * details, https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html
 * *)

module Winbind = struct
  type t = Strong | Legacy | All

  let to_string = function
    | Strong ->
        "strong"
    | Legacy ->
        "legacy"
    | All ->
        "all"

  let of_string = function
    | "all" ->
        Some All
    | "legacy" ->
        Some Legacy
    | "strong" ->
        Some Strong
    | _ ->
        None
end
