val parse_proc_xen_balloon : unit -> (string * int64 option) list
val set_memory_target : xs:Xs.xsh -> Xs.domid -> int64 -> unit
val _current_allocation : string
val _requested_target : string
val _low_mem_balloon : string
val _high_mem_balloon : string
