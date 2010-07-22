type vhd

val dd_blk_unused : int64

type open_flags = 
	| Open_rdonly
	| Open_rdwr
	| Open_fast
	| Open_strict
	| Open_ignore_disabled
	| Open_cached
	| Open_io_write_sparse

type create_flags =
	| Flag_creat_file_size_fixed
	| Flag_creat_parent_raw

type vhd_type =
	| Ty_none
	| Ty_fixed
	| Ty_dynamic 
	| Ty_diff

val _open : string -> open_flags list -> vhd 
val close : vhd -> unit
val create : string -> int64 -> vhd_type -> int64 -> create_flags list -> unit 
val snapshot : string -> int64 -> string -> int64 -> create_flags list -> unit
val get_phys_size : vhd -> int64
val get_uid : vhd -> string
val get_max_bat_size : vhd -> int64 
val get_parent_uid : vhd -> string 
val get_parent : vhd -> string
val get_virtual_size : vhd -> int64
val get_type : vhd -> vhd_type
val get_creator : vhd -> string
val get_hidden : vhd -> int 
val set_hidden : vhd -> int -> unit 
val set_phys_size : vhd -> int64 -> unit
val set_virt_size : vhd -> int64 -> unit
val coalesce : vhd -> unit
val write_sector : vhd -> int64 -> string -> int
val read_sector : vhd -> int64 -> string
val set_log_level : int -> unit
val set_parent : vhd -> string -> bool -> unit
val get_bat : vhd -> (int*int) list
val get_first_allocated_block : vhd -> int64 option
