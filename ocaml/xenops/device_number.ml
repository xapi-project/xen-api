type spec = 
	| Xen of int * int
	| Scsi of int * int
	| Ide of int * int

type interface = spec

let debug_string_of_interface = function
	| Xen(disk, partition)  -> Printf.sprintf "Xen(%d, %d)"  disk partition
	| Scsi(disk, partition) -> Printf.sprintf "Scsi(%d, %d)" disk partition
	| Ide(disk, partition)  -> Printf.sprintf "Ide(%d, %d)"  disk partition

let (<<) = (lsl)

let int_of_string x = try int_of_string x with _ -> failwith (Printf.sprintf "int_of_string [%s]" x)

let make (x: spec) : interface = 
	let max_xen = ((1 << 20) - 1), 15 in
	let max_scsi = 15, 15 in
	let max_ide = 3, 63 in
	let assert_in_range description (disk_limit, partition_limit) (disk, partition) = 
		if disk < 0 || (disk > disk_limit) 
		then failwith (Printf.sprintf "%s disk number out of range 0 <= %d <= %d" description disk disk_limit);
		if partition < 0 || partition > partition_limit 
		then failwith (Printf.sprintf "%s partition number out of range 0 <= %d <= %d" description partition partition_limit) in
	begin match x with
		| Xen(disk, partition) -> assert_in_range "xen" max_xen (disk, partition)
		| Scsi(disk, partition) -> assert_in_range "scsi" max_scsi (disk, partition)
		| Ide(disk, partition) -> assert_in_range "ide" max_ide (disk, partition)
	end;
	x

let (||) = (lor)

let xenstore_int_of_interface = function
	| Xen (disk, partition) when disk < 16 -> (202 << 8) || (disk << 4)       || partition
	| Xen (disk, partition)                -> (1 << 28)  || (disk << 8)       || partition
	| Scsi (disk, partition)               -> (8 << 8)   || (disk << 4)       || partition
	| Ide (disk, partition) when disk < 2  -> (3 << 8)   || (disk << 6)       || partition
	| Ide (disk, partition)                -> (22 << 8)  || ((disk - 2) << 6) || partition

let interface_of_xenstore_int x =
	let (&&) = (land) in
	let (>>) = (lsr) in

	if (x && (1 << 28)) <> 0
	then Xen( (x >> 8) && ((1 << 20) - 1), x && ((1 << 8) - 1))
	else match x >> 8 with
		| 202 -> Xen ( (x >> 4) && ((1 << 4) - 1), x && ((1 << 4) - 1))
		| 8   -> Scsi ( (x >> 4) && ((1 << 4) - 1), x && ((1 << 4) - 1))
		| 3   -> Ide ( (x >> 6) && ((1 << 2) - 1), x && ((1 << 6) - 1))
		| 22  -> Ide ( ((x >> 6) && ((1 << 2) - 1)) + 2, x && ((1 << 6) - 1))
		| _   -> failwith (Printf.sprintf "Unknown device number: %d" x)

type xenstore_key = string

let xenstore_key_of_interface x = string_of_int (xenstore_int_of_interface x)
let interface_of_xenstore_key x = interface_of_xenstore_int (int_of_string x)

(** Return an integer in base 26 *)
let rec base_26_of_int x = 
	let high, low = x / 26, x mod 26 in
	let high' = if high = 0 then "" else base_26_of_int high in
	let low' = String.make 1 (char_of_int (low + (int_of_char 'a'))) in
	high' ^ low'

open Stringext

(** Convert a base 26 string back into an integer *)
let int_of_base_26 x = 
	let ints = List.map (fun c -> int_of_char c - (int_of_char 'a')) (String.explode x) in
	List.fold_left (fun acc x -> acc * 26 + x) 0 ints

let linux_device_of_interface = 
	let p x = if x = 0 then "" else string_of_int x in 
	function
		| Xen  (disk, part) -> Printf.sprintf "xvd%s%s" (base_26_of_int disk) (p part)
		| Scsi (disk, part) -> Printf.sprintf "sd%s%s"  (base_26_of_int disk) (p part)
		| Ide  (disk, part) -> Printf.sprintf "hd%s%s"  (base_26_of_int disk) (p part)

let interface_of_linux_device x =
	let letter c = 'a' <= c && (c <= 'z') in
	let digit c = '0' <= c && (c <= '9') in
	let take f x = 
		let rec inner f acc = function
			| x :: xs -> 
				if f x then inner f (x :: acc) xs else List.rev acc, x :: xs
			| [] -> List.rev acc, [] in
		inner f [] x in
	(* Parse a string "abc123" into x, y where x is "abc" interpreted as base-26
	   and y is 123 *)
	let parse_b26_int x = 
		let d, p = take letter x in
		let d' = int_of_base_26 (String.implode d) in
		let p' = if p = [] then 0 else int_of_string (String.implode p) in
		d', p' in
	(* Parse a string "123p456" into x, y where x = 123 and y = 456 *)
	let parse_int_p_int x = 
		let d, rest = take digit x in
		match rest with
			| 'p' :: rest ->
				let p, _ = take digit rest in
				int_of_string (String.implode d), int_of_string (String.implode p)
			| [] ->
				int_of_string (String.implode d), 0
			| _ -> 
				failwith 
					(Printf.sprintf "expected digit+ p digit+ got: %s" (String.implode x)) in
	match String.explode x with
		| 'x' :: 'v' :: 'd' :: rest ->
			let disk, partition = parse_b26_int rest in
			Xen(disk, partition)
		| 's' :: 'd' :: rest ->
			let disk, partition = parse_b26_int rest in
			Scsi(disk, partition)
		| 'h' :: 'd' :: rest ->
			let disk, partition = parse_b26_int rest in
			Ide(disk, partition)
		| 'd' :: rest ->
			let disk, partition = parse_int_p_int rest in
			Xen(disk, partition)
		| _ -> failwith (Printf.sprintf "Failed to parse device name: %s" x)
	
type disk_number = int

let disk_number_of_interface = function
	| Xen(disk, _) -> disk
	| Scsi(disk, _) -> disk
	| Ide(disk, _) -> disk

let interface_of_disk_number hvm n = 
	if hvm && (n < 4)
	then Ide(n, 0)
	else Xen(n, 0)

let interface_of_string hvm name = 
	try
		interface_of_disk_number hvm (int_of_string name)
	with _ ->
		interface_of_linux_device name
