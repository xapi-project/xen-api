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
let xml = Xml.parse_file "sql_msg_example.txt"
let str = Xml.to_string_fmt xml

let parse() = ignore (Xml.parse_string str)
let to_string() = ignore (Xml.to_string_fmt xml)

let rec repeat f i =
  if i = 0 then ()
  else (f(); repeat f (i-1))

let _ = repeat parse 1000
(* let _ = print_string str *)
