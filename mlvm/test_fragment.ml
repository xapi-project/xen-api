(* Put them into a library? *)

let pvs      = "/usr/sbin/pvs"
let pvcreate = "/usr/sbin/pvcreate"
let pvremove = "/usr/sbin/pvremove"

let vgs      = "/usr/sbin/vgs"
let vgcreate = "/usr/sbin/vgcreate"
let vgextend = "/usr/sbin/vgextend"
let vgchange = "/usr/sbin/vgchange"
let vgremove = "/usr/sbin/vgremove"

let lvs       = "/usr/sbin/lvs"
let lvchange  = "/usr/sbin/lvchange"
let lvremove  = "/usr/sbin/lvremove"
let lvdisplay = "/usr/sbin/lvdisplay"
let lvcreate  = "/usr/sbin/lvcreate"
let lvremove  = "/usr/sbin/lvremove"
let lvresize  = "/usr/sbin/lvresize"



open Fun
open Stringext
open Listext
open Camldm

let with_debug s f =
	(print_endline ("Start. (" ^ s ^ ")")
	; f ()
	; print_endline ("Done. (" ^ s ^")"))

(* Wait for user (or not) *)
let wait s =
	Printf.fprintf stdout "Press Return: (%s)" s;
	flush stdout;
	Printf.fprintf stdout "\n";
	(*   read_line(); *)
	()

let p (stdout,stderr, x) = print_string stdout; print_string stderr; (stdout,stderr, x)

let fst3 (a,_,_) = a

(* free space on SR (=VG) in byte *)
let get_free_space vg =
	Int64.of_string ++ String.strip String.isspace ++ fst3 ++ Os.syscall $ (vgs^" "^vg^" --noheadings -o free --unit b --nosuffix")
		(* total space on SR (=VG) in byte *)
let get_size vg =
	Int64.of_string ++ String.strip String.isspace ++ fst3 ++ Os.syscall $ (vgs^" "^vg^" --noheadings -o size --unit b --nosuffix")

let vg_name = "vg_name"
let lv1_name = "lv1"

(* just hardcoded everything for a start.*)
(* Not used at the moment. *)
let test rpc intrpc gp sr =
	wait "pvcreate:";
	p(Os.syscall $ pvcreate ^" /dev/sda3 /dev/sda4");
	wait "vgcreate:";
	p(Os.syscall (vgcreate ^" "^vg_name^" /dev/sda3 /dev/sda4"));
	wait "size:";
	let size = fst3 $ p(Os.syscall (vgs^" -o size --units 4m --noheadings "^vg_name)) in
	wait "lvcreate:";
	p(Os.syscall (lvcreate^" --size 50g -n "^lv1_name^" "^vg_name));
	wait "lvchange:";
	p(Os.syscall (lvchange^" -a n "^vg_name^"/"^lv1_name));

	wait "lvremove:";
	Os.syscall (lvremove^" "^vg_name^"/"^lv1_name);
	wait "vgremove:";
	p $ Os.syscall (vgremove^" "^vg_name);
	wait "pvremove:";
	p $ Os.syscall (pvremove^" /dev/sda3 /dev/sda4");
in ()


(* Will give a reason for what went wong in the Left-constructor, soon.
   E.g. to distinguish between "Not enough space." and "Disk on fire."
 *)

let trace s = (print_endline ("Debug-Trace:\t"^s); s)
	(* Rounding up size to full physical extent 16.00 MB
	   Logical volume "lvol16" created
	 *)
let create_lv (* : string -> int64 -> (unit, string) Either.t *) =
	fun vg size (* in MiBytes (Bytes don't work.) *) ->
		let (out, err, status) =
			p (Os.syscall (lvcreate^" --size "^Int64.to_string size^"m "^ vg)) in
		if Os.was_successful (status)
		then (* "  Logical volume \"lvol22\" created\n" *)
			(let start = "  Logical volume \""
			 and ending = "\" created\n"
			 and l = String.length in
			if (String.startswith start out
			&& String.endswith ending out)
			then Either.right ++ trace
				$ String.sub out (l start) (l out - l ending - l start)
			else (print_endline ("Failed to parse:\t"^out);
			Either.Left ()))
		else (print_endline "lvcreate failed"; Either.Left ())

(* This is an unfoldM, or is it? *)
let until pred action =
	let rec helper acc =
		let item = action () in
		if pred item
		then helper (item :: acc)
		else acc in
	List.rev ++ helper $ []
let fill_up vg =
	let free_space = get_free_space vg in
	let lv_names = until Either.is_left (fun () -> create_lv vg (Random.int64 100L ));
		(* asks for less than 100 MiB as a workaround because mlvm has
		   only thin provisioning and no resizing, yet. *)
	in lv_names (* named just for documentation. *)
		   (* .vdi_info_location *)

let _ = with_debug "filling up" $ (fun () -> fill_up "vgfnord")

let has_devices output = on (((!=) 0) +++ String.compare) (String.strip String.isspace ++ String.lowercase) "No devices found\n" output

let lines = String.split_f ((=) '\n')
let words = String.split_f String.isspace

let dmsetup_devices () =
	let output = fst3 ++ p ++ Os.syscall $ "/sbin/dmsetup ls" in
	if has_devices output
	then Opt.cat_some ++ List.map (List.safe_hd ++ words) ++ lines $ output
	else []

let _ = dmsetup_devices ()

let _ = with_debug "ls" (fun () -> Opt.map (List.iter print_endline) ++ Camldm.ls)

let x () = p $ Os.syscall "/sbin/dmsetup table"

let test_ls_output () =
	let (Some l) = Camldm.ls () in
	print_endline "And here's what Ocaml gets:";
	if (l=[])
	then print_endline "Empty list"
	else List.iter print_endline l

let _ = with_debug "test_ls_output" test_ls_output

let test_ls_eq () =
	let (Some l) = Opt.map (List.sort compare) ++ Camldm.ls $ ()
	and ol = List.sort compare ++ dmsetup_devices $ () in
	(* print_endline "mlvm:";
	   List.iter print_endline l;
	   print_endline "olvm";
	   List.iter print_endline ol; *)
	ol = l

let _ = with_debug "test_ls_eq" test_ls_eq

module StringMap = Mapext.Make (String)

(* Supposed to fail when no char c at the end. *)
let rm_char c s =
	let (c::rest) = List.rev ++ String.explode $ s in
	String.implode ++ List.rev $ rest

let olvm_table () =
	(* &start, &length, &target_type, &params); *)
	(* "vg1-lvol0:" "0" "247414784" "linear" "8:4 384" *)
	let parse1 (device::start::length::target_type::params) =
		(rm_char ':' device,
		(Int64.of_string start, Int64.of_string length,
		target_type, params)::[]) in
	let output = fst3 ++ p ++ Os.syscall $ "/sbin/dmsetup table" in
	if has_devices output
	then (print_string "Here's the output:\n";print_endline ++ String.escaped $ output;
	StringMap.map (List.sort compare) ++ StringMap.fromListWith (@)
	++ List.map (parse1 ++ words) ++ lines $ output)
	else StringMap.empty

let camldm_table_targets () =
	let nf status = (* normal form *)
		List.map (fun (start, length, target_type, params) -> (start, length, target_type, words params)) status.targets
	in StringMap.map (List.sort compare) ++ StringMap.fromListWith (@) ++ List.make_assoc (nf ++ Camldm.table) ++ Opt.default [] ++ Camldm.ls $ ()

let _ = with_debug "camldm_table_targets"
	(fun () ->
		let c = camldm_table_targets ()
		and o = olvm_table ()
		in print_string "compare tables:\t";print_endline ++ string_of_int $ (StringMap.compare compare c o))

(* let _ = print_endline "Here comes create_new:\n"; safe_create "/dev/sda4" "name?" *)

let _ = print_endline "Here come the devices:\n"
let print_camldm_devices =
	List.map (print_endline ++ Jsonrpc.to_string ++ Camldm.rpc_of_status ++ Camldm.table) ++ Opt.default [] $ Camldm.ls () (* devices () *)

let get_free_space vg = 10L
let get_size vg = 10

let fragment vg = ()



(* StringMap.fromListWith (@) ++ List.map (parse1 ++ words) ++ lines $ output *)

let test_table_eq () =
	let (Some devs) = Camldm.ls ()
	in (List.make_assoc Camldm.table $ devs;
	olvm_table ())

let _ = print_endline ++ string_of_bool ++ test_ls_eq $ ()

let leakage () =
	let rec helper () =
		ignore(Camldm.ls ());
		helper ()
	in helper ()


(* compare (Camldm.table device) to Os.syscall "dmsetup table"
   Problem is: Whiche device?
   "dmsetup ls" doesn't give anything interesting (on "test -t olvm-mlvm").  But it does for "test -t coalesce" (test.ml in ocaml/sm)
 *)
