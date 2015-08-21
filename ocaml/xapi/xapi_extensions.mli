(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

(* The Xapi_globs.xapi_extensions_root contain scripts which are invoked with
   XenAPI XMLRPC on stdin, and must respond with XenAPI XMLRPC on stdout. *)

val find_extension: string -> string
(** [find_extension] returns the full path to extension [name], or raises
    the API error [message_method_unknown[ *)

val call_extension: Rpc.call -> Rpc.response
(** [call_extension call] invokes the named extension, returning the response.
    Note this function never raises; but may return a XenAPI error like
    [message_method_unknown] *)
