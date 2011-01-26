type t = 
	| In_memory of Db_cache_types.Database.t ref ref
	| Remote

exception Database_not_in_memory

let in_memory (rf: Db_cache_types.Database.t ref ref) = In_memory rf

let get_database = function
	| In_memory x -> !(!(x))
	| Remote -> raise Database_not_in_memory

let update_database t f = match t with
	| In_memory x ->
		let d : Db_cache_types.Database.t = f (get_database t) in
		(!(x)) := d
	| Remote -> raise Database_not_in_memory

