(* Copyright (C) Cloud Software Group Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module Mcache = Monitor_dbcalls_cache

val update : Mcache.StringSet.elt list -> unit
(** [update rrd_files] Reads rrd_files and update the host and VM memory
    metrics in xapi's cache. *)
