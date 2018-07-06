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

(** Thrown when no data is received from the remote HTTP server. This could happen if
    (eg) an stunnel accepted the connection but xapi refused the forward causing stunnel
    to immediately close. *)
exception Empty_response_from_server

(** Thrown when we get a non-HTTP response *)
exception Http_request_rejected of string

(** Thrown when we get a specific HTTP failure *)
exception Http_error of string * string

(** [response_of_fd fd] returns an HTTP response read from fd, or None *)
val response_of_fd: ?use_fastpath:bool -> Unix.file_descr -> Http.Response.t option

(** [response_of_fd fd] returns an HTTP response read from fd, or throws an exception *)
val response_of_fd_exn: Unix.file_descr -> Http.Response.t

(** [rpc fd request body f] marshals the HTTP request represented by [request] and [body]
    through file descriptor [fd] and then applies the response to [f]. On failure an 
    exception is thrown. *)
val rpc : ?use_fastpath:bool -> Unix.file_descr -> Http.Request.t -> (Http.Response.t -> Unix.file_descr -> 'a) -> 'a

(** See perftest/tests.ml *)
val last_content_length: int64 ref
