type bus_type =
	| Xen 
	| Scsi
	| Floppy
	| Ide
[@@deriving rpcty]

type spec = bus_type * int * int [@@deriving rpcty]

type t = spec [@@deriving rpcty]

let to_debug_string = function
	| (Xen,    disk, partition) -> Printf.sprintf "Xen(%d, %d)"    disk partition
	| (Scsi,   disk, partition) -> Printf.sprintf "Scsi(%d, %d)"   disk partition
	| (Floppy, disk, partition) -> Printf.sprintf "Floppy(%d, %d)" disk partition
	| (Ide   , disk, partition) -> Printf.sprintf "Ide(%d, %d)"    disk partition

(* ocamlp4-friendly operators *)
let (<|) = (lsl)
let (>|) = (lsr)

let int_of_string x = try int_of_string x with _ -> failwith (Printf.sprintf "int_of_string [%s]" x)

(* If this is true then we will use the deprecated (linux-specific) IDE encodings for disks > 3 *)
let use_deprecated_ide_encoding = true

let make (x: spec) : t = 
	let max_xen = ((1 <| 20) - 1), 15 in
	let max_scsi = 15, 15 in
	let max_ide = if use_deprecated_ide_encoding then 19, 63 else 3, 63 in
	let max_floppy = 2, 0 in
	let assert_in_range description (disk_limit, partition_limit) (disk, partition) = 
		if disk < 0 || (disk > disk_limit) 
		then failwith (Printf.sprintf "%s disk number out of range 0 <= %d <= %d" description disk disk_limit);
		if partition < 0 || partition > partition_limit 
		then failwith (Printf.sprintf "%s partition number out of range 0 <= %d <= %d" description partition partition_limit) in
	begin match x with
		| Xen,    disk, partition -> assert_in_range "xen" max_xen (disk, partition)
		| Scsi,   disk, partition -> assert_in_range "scsi" max_scsi (disk, partition)
		| Floppy, disk, partition -> assert_in_range "floppy" max_floppy (disk, partition)
		| Ide,    disk, partition -> assert_in_range "ide" max_ide (disk, partition)
	end;
	x

let spec (x: t) : spec = x

let (||) = (lor)

let standard_ide_table = [ 3; 22 ]
let deprecated_ide_table = standard_ide_table @ [ 33; 34; 56; 57; 88; 89; 90; 91 ]

let to_xenstore_int = function
	| Xen,    disk, partition when disk < 16 -> (202 <| 8) || (disk <| 4) || partition
	| Xen,    disk, partition                -> (1 <| 28)  || (disk <| 8) || partition
	| Scsi,   disk, partition                -> (8 <| 8)   || (disk <| 4) || partition
	| Floppy, disk, partition                -> (203 <| 8) || (disk <| 4) || partition
	| Ide,    disk, partition                ->
		let m = List.nth deprecated_ide_table (disk / 2) in
		let n = disk - (disk / 2) * 2 in (* NB integers behave differently to reals *)
		(m <| 8) || (n <| 6) || partition

let of_xenstore_int x =
	let (&&) = (land) in

	if (x && (1 <| 28)) <> 0
	then Xen, (x >| 8) && ((1 <| 20) - 1), x && ((1 <| 8) - 1)
	else match x >| 8 with
		| 202 -> Xen,    (x >| 4) && ((1 <| 4) - 1), x && ((1 <| 4) - 1)
		| 8   -> Scsi,   (x >| 4) && ((1 <| 4) - 1), x && ((1 <| 4) - 1)
		| 203 -> Floppy, (x >| 4) && ((1 <| 4) - 1), x && ((1 <| 4) - 1)
		| n   ->
			let idx = snd(List.fold_left (fun (i, res) e -> i+1, if e = n then i else res) (0, -1) deprecated_ide_table) in
			if idx < 0
			then failwith (Printf.sprintf "Unknown device number: %d" x);
			Ide, ((x >| 6) && ((1 <| 2) - 1)) + idx * 2,  x && ((1 <| 6) - 1)

type xenstore_key = int

let to_xenstore_key x = to_xenstore_int x
let of_xenstore_key x = of_xenstore_int x

(* NB the device encoding is base 26 starting from 1 rather than 0 eg
   0  -> a
   25 -> z
   26 -> aa
*)

(** Return an integer encoded as a linux device suffix *)
let rec string_of_int26 x = 
	let high, low = x / 26 - 1, x mod 26 + 1 in
	let high' = if high = -1 then "" else string_of_int26 high in
	let low' = String.make 1 (char_of_int (low + (int_of_char 'a') - 1)) in
	high' ^ low'

module String = struct
	include String
	let fold_right f string accu =
		let accu = ref accu in
		for i = length string - 1 downto 0 do
			accu := f string.[i] !accu
		done;
		!accu

	let explode string =
		fold_right (fun h t -> h :: t) string []

	let implode list =
		concat "" (List.map (String.make 1) list)
end

(** Convert a linux device string back into an integer *)
let int26_of_string x = 
	let ints = List.map (fun c -> int_of_char c - (int_of_char 'a') + 1) (String.explode x) in
	List.fold_left (fun acc x -> acc * 26 + x) 0 ints - 1

let to_linux_device = 
	let p x = if x = 0 then "" else string_of_int x in 
	function
		| Xen,    disk, part -> Printf.sprintf "xvd%s%s" (string_of_int26 disk) (p part)
		| Scsi,   disk, part -> Printf.sprintf "sd%s%s"  (string_of_int26 disk) (p part)
		| Floppy, disk, part -> Printf.sprintf "fd%s%s"  (string_of_int26 disk) (p part)
		| Ide,    disk, part -> Printf.sprintf "xvd%s%s" (string_of_int26 disk) (p part)

let of_linux_device x =
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
		let d' = int26_of_string (String.implode d) in
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
			Xen, disk, partition
		| 's' :: 'd' :: rest ->
			let disk, partition = parse_b26_int rest in
			Scsi, disk, partition
		| 'f' :: 'd' :: rest ->
			let disk, partition = parse_b26_int rest in
			Floppy, disk, partition
		| 'h' :: 'd' :: rest ->
			let disk, partition = parse_b26_int rest in
			Ide, disk, partition
		| 'd' :: rest ->
			let disk, partition = parse_int_p_int rest in
			Xen, disk, partition
		| _ -> failwith (Printf.sprintf "Failed to parse device name: %s" x)

let upgrade_linux_device x =
	match String.explode x with
	| 'h' :: 'd' :: rest -> "xvd" ^ (String.implode rest)
	| _ -> x

type disk_number = int

let to_disk_number = function
	| Xen, disk, _ -> disk
	| Scsi, disk, _ -> disk
	| Floppy, disk, _ -> disk
	| Ide, disk, _ -> disk

let of_disk_number hvm n = 
	if hvm && (n < 4)
	then Ide, n, 0
	else Xen, n, 0

let of_string hvm name = 
	try
		of_disk_number hvm (int_of_string name)
	with _ ->
		of_linux_device name
