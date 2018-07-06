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
let encode = B64.encode ?pad:None ?alphabet:None
let decode s =
  let sanitize s =
    (* ignore control characters: see RFC4648.1 and RFC4648.3
     * https://tools.ietf.org/html/rfc4648#section-3
     * Note: \t = \009, \n = \012, \r = \015, \s = \032 *)
    let result = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      if (String.unsafe_get s i >= '\000' && String.unsafe_get s i <= '\032')
      || String.unsafe_get s i = '\127'
      then ()
      else Buffer.add_char result (String.unsafe_get s i)
    done;
    Buffer.contents result
  in
  B64.decode ?alphabet:None (sanitize s)
