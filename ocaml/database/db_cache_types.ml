open Db_exn

type row = { 
  creation_event_number : Int64.t;
  mutable row_event_number : Int64.t;
  fields : (string, string) Hashtbl.t }
type table = {
  deletering : (Int64.t * Int64.t * string) Ring.t;
  rows : (string, row) Hashtbl.t }

type cache = (string, table) Hashtbl.t

let deleteringsize = 500

type where_record = {table:string; return:string; where_field:string; where_value:string}
type structured_op_t = AddSet | RemoveSet | AddMap | RemoveMap

let string_of_structured_op op = match op with
  | AddSet -> "add_set"
  | RemoveSet -> "remove_set"
  | AddMap -> "add_map"
  | RemoveMap -> "remove_map"

type db_dump_manifest =
    {
      installation_uuid : string;
      control_domain_uuid : string;
      pool_conf : string;
      pool_token : string;
      schema_major_vsn : int;
      schema_minor_vsn : int;
      product_version : string;
      product_brand : string;
      build_number : string;
      xapi_major_vsn : int;
      xapi_minor_vsn : int;
      generation_count : Generation.t
    }

let gen_manifest gen_count =
  {
    installation_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid;
    control_domain_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid;
    pool_conf = Unixext.read_whole_file_to_string Xapi_globs.pool_config_file;
    pool_token = Unixext.read_whole_file_to_string Xapi_globs.pool_secret_path;
    schema_major_vsn = Datamodel.schema_major_vsn;
    schema_minor_vsn = Datamodel.schema_minor_vsn;
    product_version = Version.product_version;
    product_brand = Version.product_brand;
    build_number = Version.build_number;
    xapi_major_vsn = Xapi_globs.version_major;
    xapi_minor_vsn = Xapi_globs.version_minor;
    generation_count = gen_count
  }

let event_number = ref 0L

(* Our versions of hashtbl.find *)
let lookup_field_in_row row fld =
  try
    Hashtbl.find row.fields fld
  with Not_found -> raise (DBCache_NotFound ("missing field",fld,""))
    
let lookup_table_in_cache cache tbl =
  try
    Hashtbl.find cache tbl
  with Not_found -> raise (DBCache_NotFound ("missing table",tbl,""))

let lookup_row_in_table tbl tblname objref =
  try
    Hashtbl.find tbl.rows objref
  with Not_found -> raise (DBCache_NotFound ("missing row",tblname,objref))

let iter_over_rows func table =
  Hashtbl.iter func table.rows

let iter_over_tables func cache =
  Hashtbl.iter func cache

let iter_over_fields func row =
  Hashtbl.iter func row.fields
  
let set_field_in_row row fldname newval =
  event_number := Int64.add 1L !event_number;
  row.row_event_number <- !event_number;
  Hashtbl.replace row.fields fldname newval

let set_row_in_table table objref newval =
  Hashtbl.replace table.rows objref newval

let set_table_in_cache cache tblname newtbl =
  Hashtbl.replace cache tblname newtbl

let create_empty_row () = {creation_event_number = !event_number; row_event_number = !event_number; fields = Hashtbl.create 20}

let create_empty_table () = {deletering = Ring.make deleteringsize (0L,0L,""); rows = Hashtbl.create 20}

let create_empty_cache () = Hashtbl.create 20

let fold_over_fields func row acc = Hashtbl.fold func row.fields acc

let fold_over_rows func table acc = Hashtbl.fold func table.rows acc

let fold_over_tables func cache acc = Hashtbl.fold func cache acc

let remove_row_from_table tbl objref = 
  event_number := Int64.add 1L !event_number;
  Ring.push tbl.deletering ((lookup_row_in_table tbl "" objref).creation_event_number, !event_number, objref);
  Hashtbl.remove tbl.rows objref

let get_rowlist tbl =
  fold_over_rows (fun k d env -> d::env) tbl []

let get_reflist tbl =
  fold_over_rows (fun k d env -> k::env) tbl []

(* Find row with specified reference in specified table *)
let find_row cache (tblname:string) (objref:string) : row =
  let tbl = lookup_table_in_cache cache tblname in
  lookup_row_in_table tbl tblname objref

(* Read column, fname, from database rows: *)
let get_column cache tblname fname =
  let rec f rows col_so_far =
    match rows with
      [] -> col_so_far
    | (r::rs) ->
	let value = try Some (lookup_field_in_row r fname) with _ -> None in
	match value with
	  None -> f rs col_so_far
	| (Some u) -> f rs (u::col_so_far) in
  f (get_rowlist (lookup_table_in_cache cache tblname)) []

(** Return a snapshot of the database cache suitable for slow marshalling across the network *)
let snapshot cache : cache = 
  Db_lock.with_lock 
    (fun () ->
       let row table rf vals = 
	 let newrow = create_empty_row () in
	 iter_over_fields (set_field_in_row newrow) vals;
	 set_row_in_table table rf newrow in
       
       let table cache name tbl = 
	 let newtable = create_empty_table () in
	 iter_over_rows (row newtable) tbl;
	 set_table_in_cache cache name newtable in
       
       let newcache = create_empty_cache () in  
       iter_over_tables (table newcache) cache;
       newcache)

let bump_event_number_in_row row =
  event_number := Int64.add 1L !event_number;
  row.row_event_number <- !event_number

exception Too_many_deletion_events

(* The exception is raised when the specified gencount is older than the oldest
 * item in the deletering. *)
let fold_over_recent_rows func table gencount acc =
  let acc = fold_over_rows (fun objref row acc ->
    if (row.creation_event_number > gencount || row.row_event_number > gencount) 
    then func row.creation_event_number row.row_event_number 0L objref acc
    else acc) table acc in
  let rec deleteloop i acc =
    if i>deleteringsize then raise Too_many_deletion_events else begin
      let (ctime,dtime,objref) = Ring.peek table.deletering i in
      if dtime <= gencount then acc else
        deleteloop (i+1) (func ctime 0L dtime objref acc)
    end
  in deleteloop 0 acc
    
let get_current_event_number () =
  !event_number
