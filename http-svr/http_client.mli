(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
(* A very simple HTTP client *)

(** Thrown when we get a non-HTTP response *)
exception Http_request_rejected

(** Thrown when we get a specific HTTP failure *)
exception Http_error of string

(** [rpc fd request body f] marshals the HTTP request represented by [request] and [body]
    through file descriptor [fd] and then applies the response to [f]. On failure an 
    exception is thrown. *)
val rpc : Unix.file_descr -> Http.request -> string -> (Http.Response.t -> Unix.file_descr -> 'a) -> 'a