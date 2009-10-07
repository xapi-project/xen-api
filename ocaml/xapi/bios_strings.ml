module D = Debug.Debugger(struct let name="bios_strings" end)
open D
open Stringext

let dmidecode_prog = "/usr/sbin/dmidecode"

let remove_invisible str =
	String.fold_left (fun s c -> if c >= ' ' && c <= '~' then s ^ (String.of_char c) else s) "" str

let trim str =
	let l = String.length str in
	let rec check_left i =
		if i < l && String.isspace str.[i] then
			check_left (i+1)
		else
			i
	in
	let rec check_right i =
		if i > 0 && String.isspace str.[i] then
			check_right (i-1)
		else
			i+1
	in
	let a = check_left 0 in
	let b = (check_right (l-1)) - a in
	try	String.sub str a b with Invalid_argument _  -> ""
	
(* obtain the BIOS string with the given name from dmidecode *)
let get_bios_string name =
	try
		let str, _ = Forkhelpers.execute_command_get_output dmidecode_prog [dmidecode_prog; "-s"; name] in
		let str = trim (remove_invisible str) in
		if str = "" || str = "Not Specified" then ""
		else str
	with _ -> ""

(* obtain the Type 11 OEM strings from dmidecode *)
let get_oem_strings () =
	try
		let result, _ = Forkhelpers.execute_command_get_output dmidecode_prog [dmidecode_prog; "-t11"; "-q"] in
		let result = trim (remove_invisible result) in
		let rec loop index a =
			try
				let b = String.index_from result a ':' in
				let c = String.index_from result b '\n' in
				(index, String.sub result (b+2) (c-b-2)) :: loop (index+1) c
			with _ -> []
		in
		loop 1 0
	with _ -> []

(* Get the HP-specific ROMBIOS OEM string:
 * 6 bytes from the memory starting at 0xfffea *)
let get_hp_rombios () =
	let hp_rombios = String.make 6 ' ' in
	begin try
		let mem = Unix.openfile "/dev/mem" [Unix.O_RDONLY] 0 in
		Pervasiveext.finally (fun () ->
			ignore (Unix.lseek mem 0xfffea Unix.SEEK_SET);
			ignore (Unix.read mem hp_rombios 0 6))
		(fun () -> Unix.close mem)
	with _ -> ()
	end;
	trim (remove_invisible hp_rombios)

let set_host_bios_strings ~__context ~host =
	info "Setting host BIOS strings.";
	(* first clear the current strings *)
	Db.Host.set_bios_strings ~__context ~self:host ~value:[];
	
	(* named BIOS strings *)
	let fetch_and_add_string str =
		let value = get_bios_string str in
		Db.Host.add_to_bios_strings ~__context ~self:host ~key:str ~value:value
	in
	let dmidecode_strings = ["bios-vendor"; "bios-version"; "system-manufacturer";
		"system-product-name"; "system-version"; "system-serial-number"] in
	List.iter fetch_and_add_string dmidecode_strings;
	
	(* type 11 OEM strings *)
	let oem_strings = get_oem_strings () in
	let add_oem_string (index, value) =
		let str = "oem-" ^ (string_of_int index) in
		Db.Host.add_to_bios_strings ~__context ~self:host ~key:str ~value:value
	in
	List.iter add_oem_string oem_strings;

	(* HP-specific ROMBIOS OEM string *)
	let hp_rombios = get_hp_rombios () in
	Db.Host.add_to_bios_strings ~__context ~self:host ~key:"hp-rombios" ~value:hp_rombios
	
