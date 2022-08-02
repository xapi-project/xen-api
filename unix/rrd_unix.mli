(*
  Copyright (C) Citrix Systems Inc.
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation; version 2.1 only. with the special
  exception on linking described in file LICENSE.
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
 *)
(** RRD Unix module
    This module provides Unix tools for dealing with RRDs
 *)

val to_fd : ?json:bool -> Rrd.rrd -> Unix.file_descr -> unit
(** Serialize the rrd to xml / json and offer it through a file descriptor *)
