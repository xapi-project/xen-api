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

type handler = {
  name: string;                                  (** used for naming the thread *)
  body: Unix.sockaddr -> Unix.file_descr -> unit (** function called in a thread for each connection*)
}

type server = { 
  shutdown : unit -> unit                        (** clean shutdown, blocks until thread has gone *)
}

(** Creates a server given a bound socket and a handler *)
val server : handler -> Unix.file_descr -> server

