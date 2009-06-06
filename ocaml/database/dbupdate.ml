(* File dbupdatemain.ml *)

open Sqlsyntax

let fix_inserts old_tbls old_inserts new_tbls defaults =
  let new_insert ins =
    let new_table = List.find (fun t -> t.name=ins.table) new_tbls in
    let old_table = List.find (fun t -> t.name=ins.table) old_tbls in
    let old_fields = List.combine old_table.fields ins.values in
    let new_values = List.map (fun n -> 
      try List.assoc n old_fields 
      with _ -> 
	begin
	  try
	    List.assoc (Printf.sprintf "%s.%s" ins.table n) defaults
	  with _ -> 
	    Printf.fprintf stderr "Param %s.%s not found. Defaulting to \"\"\n" ins.table n; ""
	end) new_table.fields in
    Insert {table=ins.table; values=new_values}
  in
  List.map new_insert old_inserts
    
let main dumpfile newschema defaults =
  let lexbuf = Lexing.from_channel (open_in dumpfile) in
  let read_sql lexbuf =
    let rec inner tbls inserts =
      try
	let result = Sqlparse.statement Sqllex.token lexbuf in
	let tbls,inserts = 
	  match result with
	      Create t -> t::tbls,inserts
	    | Insert t -> tbls,t::inserts
	    | _ -> tbls,inserts
	in inner tbls inserts
      with _ -> (List.rev tbls),(List.rev inserts)
    in inner [] []
  in
  let old_tbls,old_inserts = read_sql lexbuf in
  
  let lexbuf = Lexing.from_channel (open_in newschema) in
  let new_tbls,_ = read_sql lexbuf in
  let new_inserts = fix_inserts old_tbls old_inserts new_tbls defaults in
  (new_tbls,new_inserts)

let get_defaults fname =
  let ic = open_in fname in
  let rec inner cur =
    try
      inner ((input_line ic)::cur)
    with
	_ -> cur
  in 
  let defs = inner [] in
  List.map (fun s -> 
    match (Stringext.String.split '=' s) with
      | [a;b] -> (a,b)
      | _ -> failwith "Arguments must be of the form param=value") defs

let _ =
  let dump = ref "" in
  let schema = ref "/etc/xensource/db_schema.sql" in
  let output = ref "" in
  let defs = ref "" in
  Arg.parse 
    [ "-i", Arg.Set_string dump, "Specifies the file containing the dump of the current database";
      "-s", Arg.Set_string schema, Printf.sprintf "Specifies the file containing the new schema (defaults to %s)" !schema;
      "-o", Arg.Set_string output, "Specifies the output file";
      "-d", Arg.Set_string defs, "Defaults file (lines of the form table.field=value)";
    ] (fun s -> failwith "Unknown argument: Run with -h to get help") 
    "Tool to update an older Rio database to a newer schema";
  if (!dump = "") || (!output = "") then
    failwith "Need to specify input and output files";
  let defaults = if !defs <> "" then get_defaults !defs else [] in
  let dumpfile= !dump in
  let newschema= !schema in
  let (new_tbls,new_inserts) = main dumpfile newschema defaults in
  let oc = open_out !output in
  List.iter (Sqlsyntax.output oc) (List.map (fun t -> Create t) new_tbls);
  List.iter (Sqlsyntax.output oc) new_inserts;
  close_out oc




  
