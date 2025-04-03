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

  let to_encoding = function
    (*
     * [X] 0x00000001 DES-CBC-CRC
     * [X] 0x00000002 DES-CBC-MD5
     * [X] 0x00000004 RC4-HMAC
     * [X] 0x00000008 AES128-CTS-HMAC-SHA1-96
     * [X] 0x00000010 AES256-CTS-HMAC-SHA1-96
     * *)
    | Strong ->
        0x08 lor 0x10
    | Legacy ->
        0x01 lor 0x02 lor 0x04
    | All ->
        0x01 lor 0x02 lor 0x4 lor 0x08 lor 0x10

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
