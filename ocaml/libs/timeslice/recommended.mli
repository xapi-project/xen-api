val measure : ?max_overhead_percentage:float -> unit -> float
(** [measure  ?max_overhead_percentage ()] returns the recommended timeslice for the current system.

  The returned value should be used in a call to {!val:Timeslice.set}.

  @param max_overhead_percentage default 1%
  @returns [interval] such that [overhead / interval <= max_overhead_percentage / 100]
 *)
