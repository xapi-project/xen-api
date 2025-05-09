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

exception XML_unmarshall_error

exception Expression_error of (string * exn)

val expr_of_xml : XMLRPC.xmlrpc -> Db_filter_types.expr

val expr_of_string : string -> Db_filter_types.expr

val xml_of_expr : Db_filter_types.expr -> XMLRPC.xmlrpc

val eval_expr : (Db_filter_types._val -> string) -> Db_filter_types.expr -> bool
