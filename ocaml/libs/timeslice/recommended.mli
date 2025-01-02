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

val measure : ?max_overhead_percentage:float -> unit -> float
(** [measure  ?max_overhead_percentage ()] returns the recommended timeslice for the current system.

  The returned value should be used in a call to {!val:Timeslice.set}.

  @param max_overhead_percentage default 1%
  @returns [interval] such that [overhead / interval <= max_overhead_percentage / 100]
 *)
