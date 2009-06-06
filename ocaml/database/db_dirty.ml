
(* ------------------- Keep a per dbconn cache of which rows/tables are dirty *)

(* Keep track of what's dirty in the in-memory cache: *)

(* Note that "Clean" is only used as a return value when nothing is in the dirty_rows entry for
   a particular db connection. We never store an explicit "Clean" entry in the dirty_rows table;
   instead the absence of an entry for a given object reference tells us that the row is clean *)
type status = New | Modified | Clean
type dirty_record = {dirty_tables : (string,unit) Hashtbl.t;
		     dirty_rows : (string,status) Hashtbl.t}
let dirty_records : (Parse_db_conf.db_connection, dirty_record) Hashtbl.t = Hashtbl.create 20
let make_new_dirty_record() = {dirty_tables = Hashtbl.create 20; dirty_rows = Hashtbl.create 20}

let foreach_db_connection f =
  let dirty_records = List.map (fun dbconn -> Hashtbl.find dirty_records dbconn) (Db_conn_store.read_db_connections()) in
  List.iter f dirty_records

let for_my_db_connection dbconn f =
  let my_record = Hashtbl.find dirty_records dbconn in
  f my_record

let make_blank_dirty_records() =
  List.iter (fun dbconn -> Hashtbl.replace dirty_records dbconn (make_new_dirty_record())) (Db_conn_store.read_db_connections())

(* common fns that are called from fns below *)

(* When we set row dirty status then "New" must always take precendence over "Modified" *)
let set_row_dirty_status objref status dr =
  let doset() = Hashtbl.replace dr.dirty_rows objref status in
  match status with
  | Modified ->
      begin
	try
	  let current_state = Hashtbl.find dr.dirty_rows objref in
	  if current_state<>New then doset() (* do not override new with modified *)
	with _ -> doset() (* not found *)
      end
  | _ -> doset()

let clear_row_dirty_status objref dr = Hashtbl.remove dr.dirty_rows objref
let set_dirty_table_status tblname dr = Hashtbl.replace dr.dirty_tables tblname ()
let clear_dirty_table_status tblname dr = Hashtbl.remove dr.dirty_tables tblname
  
let clear_my_row_dirty_status dbconn objref =
  for_my_db_connection dbconn (clear_row_dirty_status objref)
let clear_my_dirty_table_status dbconn tblname =
  for_my_db_connection dbconn (clear_dirty_table_status tblname)

(* Functions to manipulate dirty status (below) must always be called from a context in which the
   database is locked. *)
(* "New" status must always take precendence over "Modified" status *)
let set_all_row_dirty_status objref status =
  foreach_db_connection (set_row_dirty_status objref status)

let clear_all_row_dirty_status objref =
  foreach_db_connection (clear_row_dirty_status objref)

let set_all_dirty_table_status tblname =
  foreach_db_connection (set_dirty_table_status tblname)

let clear_all_dirty_table_status tblname =
  foreach_db_connection (clear_dirty_table_status tblname)

let read_my_dirty_table_status dbconn tblname =
  for_my_db_connection dbconn (fun dr->Hashtbl.mem dr.dirty_tables tblname)

let read_my_row_dirty_status dbconn objref =
  try
    for_my_db_connection dbconn (fun dr->Hashtbl.find dr.dirty_rows objref)
  with Not_found -> Clean
