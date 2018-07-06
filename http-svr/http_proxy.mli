(*
 * Copyright (C) Citrix Systems Inc.
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

(** [one request input output] proxies the single HTTP request [request]
    from [input] to [output] *)
val one: Http.Request.t -> Unix.file_descr -> Unix.file_descr -> unit

(** [http_proxy ip port transport] establishes an HTTP proxy on [ip]:[port]
    which forwards all requests via [transport] *)
val http_proxy: string -> int -> Xmlrpc_client.transport -> unit
