module D = Debug.Debugger(struct let name = "xapi-db-process" end)
open D

open Pervasiveext

let format = ref ""
let filename = ref ""
let operation = ref ""
let config = ref ""
let iqn = ref ""
let xmltostdout = ref false

let _format_sqlite = "sqlite"
let _format_xml = "xml"
let _format_xml_compressed = "xmlcompressed"

let init_logs() =
  Logs.append "xapi-db-process" Log.Info "syslog:xapi_db_process"

let fatal_error s =
  print_string s;
  print_string "\n";
  exit 1
  
type operation = Db_transform | Read_gencount | Read_hostiqn | Write_hostiqn
		 | Am_i_in_the_database | Unknown of string
let parse_operation s =
  match (String.lowercase s) with
    "db_transform" -> Db_transform
  | "read_gencount" -> Read_gencount
  | "read_hostiqn" -> Read_hostiqn
  | "write_hostiqn" -> Write_hostiqn
  | "am_i_in_the_database" -> Am_i_in_the_database
  | s                      -> (Unknown s)

let valid_formats = [_format_sqlite; _format_xml; _format_xml_compressed]

let initialise_db_connections() =
  let dbs = Parse_db_conf.parse_db_conf 
    (if !config="" then Xapi_globs.db_conf_path else !config) in
  Db_conn_store.initialise_db_connections dbs

let read_in_database() =
  (* Make sure we're running in master mode: we cannot be a slave
     and then access the dbcache *)
  Db_cache.database_mode := Some Db_cache.Master;
  initialise_db_connections();
  Db_dirty.make_blank_dirty_records();  
  (* Initialiase in-memory database cache *)
  Db_cache.DBCache.initialise_db_cache_nosync()

let write_out_databases() =
  List.iter 
    (fun (_,db)-> Db_connections.force_flush_all db)
    (Db_connections.get_dbs_and_gen_counts())

(* should never be thrown due to checking argument at start *)
exception UnknownFormat
  
let write_out_database filename =
  print_string ("Dumping database to: "^filename^"\n");
  Db_connections.force_flush_all
    {Parse_db_conf.dummy_conf with
       Parse_db_conf.path=filename;
       Parse_db_conf.format=
	(if !format=_format_sqlite then Parse_db_conf.Sqlite
	 else if !format=_format_xml then Parse_db_conf.Xml
	 else if !format=_format_xml_compressed then Parse_db_conf.Xml
	 else raise UnknownFormat);
       Parse_db_conf.mode=Parse_db_conf.No_limit;
       Parse_db_conf.compress=(!format="xmlcompressed")
    }

let help_pad = "      "
let operation_list =
  String.concat "\n"
    (List.map (fun s -> help_pad^s)
       ["db_transform  -- transform database into new format";
	"read_gencount -- read maximum gencount of databases listed in db.conf";
        "read_hostiqn  -- read the initiator IQN for this host from the db host table";
	"write_hostiqn -- write a new initiator IQN for this host into all db's host tables"
       ]
    )
    
let do_db_transform() =
  if (not !xmltostdout) then
    begin
      if !filename = "" then
	fatal_error "No filename specified, and xmltostdout option not set\n";
      if not (List.mem !format valid_formats) then
	fatal_error "Invalid format specified\n"
    end;
  begin
    read_in_database();
    if !xmltostdout then
      Db_cache.DBCache.dump_db_cache (Db_cache_types.gen_manifest (Generation.read_generation())) (Unix.descr_of_out_channel stdout)
    else
      write_out_database !filename
  end

let do_read_gencount() =
  initialise_db_connections();
  let connections = Db_conn_store.read_db_connections () in
  Db_connections.init_gen_count connections;
  let maxgen = Generation.read_generation() in
  Printf.printf "%Ld\n" maxgen

let find_my_host_row() =
  Xapi_inventory.read_inventory ();
  let localhost_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
  let host_table = Db_backend.lookup_table_in_cache "host" in
  let host_rows  = Db_backend.get_rowlist host_table in
  List.find 
    (fun row -> let row_uuid = Db_backend.lookup_field_in_row row "uuid" in
     localhost_uuid=row_uuid) host_rows

let _iscsi_iqn = "iscsi_iqn"
let _other_config = "other_config"

let do_read_hostiqn() =
  read_in_database();
  let localhost_row = find_my_host_row() in
  let other_config_sexpr = Db_backend.lookup_field_in_row localhost_row _other_config in
  let other_config = String_unmarshall_helper.map (fun x->x) (fun x->x) other_config_sexpr in
  Printf.printf "%s" (List.assoc _iscsi_iqn other_config)

let do_write_hostiqn() =
  if !iqn = "" then
    fatal_error "Must specify '-hostiqn <value>'";
  let new_iqn = !iqn in
  read_in_database();
  let localhost_row = find_my_host_row() in
  (* read other_config from my row, replace host_iqn if already there, add it if its not there and write back *)
  let other_config_sexpr = Db_backend.lookup_field_in_row localhost_row _other_config in
  let other_config = String_unmarshall_helper.map (fun x->x) (fun x->x) other_config_sexpr in
  let other_config =
    if List.mem_assoc _iscsi_iqn other_config then
      (* replace if key already exists *)
      List.map (fun (k,v) ->k, if k=_iscsi_iqn then new_iqn else v) other_config
    else
      (* ... otherwise add new key/value pair *)
      (_iscsi_iqn,new_iqn)::other_config in
  let other_config = String_marshall_helper.map (fun x->x) (fun x->x) other_config in
  Db_backend.set_field_in_row localhost_row _other_config other_config;
  write_out_databases()

let do_am_i_in_the_database () = 
  read_in_database();
  try
    let (_: Db_backend.row) = find_my_host_row() in
    Printf.printf "true"
  with _ ->
    Printf.printf "false"

let _ =
  init_logs();
  Arg.parse ([
	       "-format", Arg.Set_string format, "format to write out ("^(String.concat ", " valid_formats )^")";
               "-config", Arg.Set_string config, "config file to read";
	       "-filename", Arg.Set_string filename, "filename to write to";
	       "-xmltostdout", Arg.Set xmltostdout, "write XML db to stdout [format/filename ignored if this option is present]";
	       "-operation", Arg.Set_string operation, "operation to perform:\n"^operation_list^"\n"^help_pad^"(defaults to db_transform if no operation specified)";
	       "-hostiqn", Arg.Set_string iqn, "hostiqn value"
	     ])
    (fun x -> print_string ("Warning, ignoring unknown argument: "^x))
    "XE database tool";
  if !operation = "" then
    operation := "db_transform";
  info "xapi-db-process executed: operation='%s'" !operation;
  match parse_operation !operation with
    Db_transform ->
      do_db_transform()
  | Read_gencount ->
      do_read_gencount()
  | Read_hostiqn ->
      do_read_hostiqn()
  | Write_hostiqn ->
      do_write_hostiqn()
  | Am_i_in_the_database ->
      do_am_i_in_the_database()
  | Unknown s ->
      error "unknown operation %s" s
