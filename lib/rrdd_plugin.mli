module Common : functor (N : (sig val name : string end)) -> sig
	val wait_until_next_reading : ?neg_shift:float -> unit -> unit

	val now : unit -> int64

	val cut : string -> string list

	val exec_cmd : cmdstring:string -> f:(string -> 'a option) -> 'a list

	val list_directory_unsafe : string -> string list

	val list_directory_entries_unsafe : string -> string list

	val main_loop : neg_shift:float -> dss_f:(unit -> (Rrd.ds_owner * Ds.ds) list) -> unit
end
