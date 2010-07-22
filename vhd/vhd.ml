type vhd

let dd_blk_unused = 0xFFFFFFFFL

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

external __open : string -> int -> vhd = "stub_vhd_open"

let _open file flags =
	let flag_value = function 
		| Open_rdonly -> 1
		| Open_rdwr -> 2
		| Open_fast -> 4
		| Open_strict -> 8
		| Open_ignore_disabled -> 16
		| Open_cached -> 32
		| Open_io_write_sparse -> 64
	in
	let flags_value = List.fold_left (+) 0 (List.map flag_value flags)  in
	__open file flags_value

let create_flags_value flags =
	let flag_value = function
		| Flag_creat_file_size_fixed -> 1
		| Flag_creat_parent_raw -> 2
	in
	List.fold_left (+) 0 (List.map flag_value flags) 

let create_ty_value ty =
	match ty with
		| Ty_none -> 0
		| Ty_fixed -> 2
		| Ty_dynamic -> 3
		| Ty_diff -> 4

external close : vhd -> unit = "stub_vhd_close"

external _create : string -> int64 -> int -> int64 -> int -> unit = "stub_vhd_create"

let create name bytes ty mbytes flags =
	let flags_value = create_flags_value flags in
	let ty_value = create_ty_value ty in
	_create name bytes ty_value mbytes flags_value

external _snapshot : string -> int64 -> string -> int64 -> int -> unit = "stub_vhd_snapshot"

let snapshot name bytes parent mbytes flags =
	let flags_value = create_flags_value flags in
	_snapshot name bytes parent mbytes flags_value

external get_phys_size : vhd -> int64 = "stub_vhd_get_phys_size"
external get_uid : vhd -> string = "stub_vhd_get_uid"
external get_max_bat_size : vhd -> int64 = "stub_vhd_get_max_bat_size"
external get_parent_uid : vhd -> string = "stub_vhd_get_parent_uid"
external get_parent : vhd -> string = "stub_vhd_get_parent"
external get_virtual_size : vhd -> int64 = "stub_vhd_get_virtual_size"
external _get_type : vhd -> int = "stub_vhd_get_type"

let get_type vhd =
	let ty = _get_type vhd in
	match ty with
		| 0 -> Ty_none
		| 2 -> Ty_fixed
		| 3 -> Ty_dynamic
		| 4 -> Ty_diff
		| _ -> failwith "Unknown VHD type"

external get_creator : vhd -> string = "stub_vhd_get_creator"
external get_hidden : vhd -> int = "stub_vhd_get_hidden"
external set_hidden : vhd -> int -> unit = "stub_vhd_set_hidden"
external set_phys_size : vhd -> int64 -> unit = "stub_vhd_set_phys_size"
external set_virt_size : vhd -> int64 -> unit = "stub_vhd_set_virt_size"
external coalesce : vhd -> unit = "stub_vhd_coalesce"
external write_sector : vhd -> int64 -> string -> int = "stub_vhd_write_sector"
external read_sector : vhd -> int64 -> string = "stub_vhd_read_sector"
external set_log_level : int -> unit = "stub_vhd_set_log_level"
external set_parent : vhd -> string -> bool -> unit = "stub_vhd_set_parent"
external get_bat : vhd -> (int*int) list = "stub_vhd_get_bat"
external _get_first_allocated_block : vhd -> int64 = "stub_vhd_get_first_allocated_block"

let get_first_allocated_block vhd =
	let blk = _get_first_allocated_block vhd in
	if blk = dd_blk_unused then None else Some blk

let with_vhd filename rw f =
	let vhd = _open filename (if rw then [Open_rdwr] else [Open_rdonly]) in
	try
		let result = f vhd in
		close vhd;
		result
	with e ->
		close vhd;
		raise e

