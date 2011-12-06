(* tool to generate pool secrets *)

type options = { force : bool; tgtfile : string }

let options = ref { force = false; tgtfile = Filename.concat Fhs.etcdir "ptoken" }

let set_force _ = options := { !options with force = true }
let set_target s = options := { !options with tgtfile = s }

let opt_force = ("-f", Arg.Unit set_force, "force generation of pool token, overwriting any existing one")
let opt_target = ("-o", Arg.String set_target, "name of file to write to [ptoken]")
let opts = [opt_force; opt_target]

let _ =
	Arg.parse opts (fun _ -> ()) "Generate a pool token";
	if Sys.file_exists !options.tgtfile
		then if !options.force
			then Sys.remove !options.tgtfile
			else begin print_endline "File exists, use -f to replace it."; exit 1 end;
	let uuid _ = Uuid.to_string (Uuid.make_uuid ()) in
	let uuids = String.concat "/" [uuid (); uuid (); uuid ()] in
	let f = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o640 !options.tgtfile in
	output_string f uuids
