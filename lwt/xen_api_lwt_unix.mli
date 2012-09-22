(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

type t
(** An active xen-api connection *)

val make: Unix.sockaddr -> t
(** [of_sockaddr addr] creates a plaintext xen-api connection to [addr] *)

val rpc: ?timeout:float -> t -> Xml.xml -> (Xml.xml, exn) Xen_api.result Lwt.t
(** performs (and optionally retries) an RPC request *)
