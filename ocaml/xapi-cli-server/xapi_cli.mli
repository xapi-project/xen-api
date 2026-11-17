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

val register_rpc_fun :
  (Http.Request.t -> Unix.file_descr option -> Rpc.call -> Rpc.response) -> unit
(** [register_rpc_fun rpc] sets the function the CLI server uses to make API
    calls. *)

val handler : Http.Request.t -> Unix.file_descr -> 'a -> unit
(** [handler req s _] runs the xe command received in [req] on [s]. *)

val exception_handler : Unix.file_descr -> exn -> unit
(** [exception_handler s e] reports [e] to the xe client on [s]. Exposed for
    testing the messages the client prints. *)
