open Db_exn
open Db_lock
open Db_action_helper

module D = Debug.Debugger(struct let name = "sql" end)
open D

(* delete a db file and its generation count, best effort *)
let try_and_delete_db_file file =
  (try Unix.unlink file with _ -> ());
  (try Unix.unlink (file^".generation") with _ -> ())

(* --------------------- Useful datamodel constructions: *)

(* Return a list of all the SQL table names *)
(* ---- NOTE THIS DEPENDENCY ACTUALLY LINKS IDL DATAMODEL INTO BINARY ---- *)
let api_objs = Dm_api.objects_of_api Datamodel.all_api
let api_relations = Dm_api.relations_of_api Datamodel.all_api
let db_table_names = 
  List.map (fun x->Gen_schema.sql_of_obj x.Datamodel_types.name) api_objs

(* Build a table that maps table names onto their persistency options *)
let table_persist_options = Hashtbl.create 20
let _ =
  begin
    let objs = Dm_api.objects_of_api Datamodel.all_api in
    List.iter (fun x->Hashtbl.replace table_persist_options (Gen_schema.sql_of_obj x).Datamodel_types.name x.Datamodel_types.persist) objs
  end

let this_table_persists tblname = (Hashtbl.find table_persist_options tblname)=Datamodel_types.PersistEverything

(* Flatten fields in heirarchical namespace (as they are in IDL) into a flat list *)
let rec flatten_fields fs acc =
  match fs with
    [] -> acc
  | (Datamodel_types.Field f)::fs -> flatten_fields fs (f::acc)
  | (Datamodel_types.Namespace (_,internal_fs))::fs -> flatten_fields fs (flatten_fields internal_fs acc)
      
(* Build a table that maps table/field names onto field persistency options *)
let field_persist_options : (string (* table name *) * string (* field name *), bool) Hashtbl.t = Hashtbl.create 20
let _ =
  begin
    let objs = Dm_api.objects_of_api Datamodel.all_api in
    List.iter
      (fun obj->
	 let fields = flatten_fields obj.Datamodel_types.contents [] in
	 List.iter (fun f->Hashtbl.replace field_persist_options
		      ((Gen_schema.sql_of_obj obj).Datamodel_types.name (* table name *),
		       (Gen_schema.sql_of_id f.Datamodel_types.full_name) (* field name *))
		      f.Datamodel_types.field_persist) fields
      )
      objs
  end

let persist_field_changes tblname fldname =
  Hashtbl.find field_persist_options (tblname,fldname)

(* --------------------- Some field-name constants *)
    
(** Table column name which contains the reference *)
let reference_fname = Gen_schema.reference

(** Table column name which contains the name_label *)
let name_label_fname = Gen_schema.sql_of_id ["name";"label"]

(** Table column name which contains the uuid *)
let uuid_fname = "uuid"

(* --------------------- Constants/data-structures for storing db contents *)

let db_FLUSH_TIMER=2.0 (* flush db write buffer every db_FLUSH_TIMER seconds *)
let display_sql_writelog_val = ref true (* compute/write sql-writelog debug string *)

(* The data itself in the cache lives in these tables: *)
type row = (string,string) Hashtbl.t
type table = (string, row) Hashtbl.t
type cache = (string,table) Hashtbl.t

let cache : cache = Hashtbl.create 20

(* Keep track of all references, and which class a reference belongs to: *)
let ref_table_map : (string,string) Hashtbl.t = Hashtbl.create 100

(* --------------------- Util functions on db datastructures *)

(* These fns are called internally, and always in locked context, so don't need to take lock again *)
let add_ref_to_table_map objref tblname =
  Hashtbl.replace ref_table_map objref tblname
let remove_ref_from_table_map objref =
  Hashtbl.remove ref_table_map objref

(* Our versions of hashtbl.find *)
let lookup_field_in_row row fld =
  try
    Hashtbl.find row fld
  with Not_found -> raise (DBCache_NotFound ("missing field",fld,""))
    
let lookup_table_in_cache tbl =
  try
    Hashtbl.find cache tbl
  with Not_found -> raise (DBCache_NotFound ("missing table",tbl,""))

let lookup_row_in_table tbl tblname objref =
  try
    Hashtbl.find tbl objref
  with Not_found -> raise (DBCache_NotFound ("missing row",tblname,objref))

let get_rowlist tbl =
  Hashtbl.fold (fun k d env -> d::env) tbl []

let get_reflist tbl =
  Hashtbl.fold (fun k d env -> k::env) tbl []

(* Read column, fname, from database rows: *)
let get_column tblname fname =
  let rec f rows col_so_far =
    match rows with
      [] -> col_so_far
    | (r::rs) ->
	let r_uuid = try Some (Hashtbl.find r fname) with _ -> None in
	match r_uuid with
	  None -> f rs col_so_far
	| (Some u) -> f rs (u::col_so_far) in
  f (get_rowlist (lookup_table_in_cache tblname)) []

(* Our versions of hashtbl.replace -- this one really is just same fn for now! *)
let set_field_in_row row fldname newval =
  Hashtbl.replace row fldname newval

let set_table_in_cache tblname newtbl =
  Hashtbl.replace cache tblname newtbl

(* Find row with specified reference in specified table *)
let find_row (tblname:string) (objref:string) :row =
  let tbl = lookup_table_in_cache tblname in
  lookup_row_in_table tbl tblname objref

(* Given a (possibly partial) new row to write, check if any db
   constraints are violated *)
let check_unique_table_constraints tblname newrow =
  let check_unique_constraint tblname fname opt_value =
    match opt_value with
      None -> ()
    | Some v ->
	if List.mem v (get_column tblname fname) then begin
	  error "Uniqueness constraint violation: table %s field %s value %s" tblname fname v;
	  (* Note: it's very important that the Uniqueness_constraint_violation exception is thrown here, since
	     this is what is caught/marshalled over the wire in the remote case.. Do not be tempted to make this
	     an API exception! :) *)
	  raise (Uniqueness_constraint_violation ( tblname, fname, v )) 
	end in
  let new_uuid = try Some (Hashtbl.find newrow uuid_fname) with _ -> None in
  let new_ref = try Some (Hashtbl.find newrow reference_fname) with _ -> None in
  check_unique_constraint tblname uuid_fname new_uuid;
  check_unique_constraint tblname reference_fname new_ref

(** Return a snapshot of the database cache suitable for slow marshalling across the network *)
let snapshot (cache: Db_cache_types.cache) : Db_cache_types.cache = 
  Db_lock.with_lock 
    (fun () ->
       let row table rf vals = 
	 let result = Hashtbl.create 10 in
	 Hashtbl.iter (Hashtbl.add result) vals;
	 Hashtbl.add table rf result in
       
       let table cache name tbl = 
	 let result = Hashtbl.create 10 in
	 Hashtbl.iter (row result) tbl;
	 Hashtbl.add cache name result in
       
       let result = Hashtbl.create 10 in  
       Hashtbl.iter (table result) cache;
       result)

(* --------------------- Util functions to support incremental index generation *)

(* Incrementally build and cache indexes *)
type index = (string, string list) Hashtbl.t
    (* index takes a tbl-name, where-field, return-fld and returns an index
       mapping where-val onto a return-val list *)
let indexes : (string*string*string, index) Hashtbl.t = Hashtbl.create 50
let invalidate_indexes tbl =
  Hashtbl.iter (fun (index_tbl,w,r) _ ->
		  if tbl=index_tbl then Hashtbl.remove indexes (index_tbl,w,r)) indexes

let invalidate_indexes_for_specific_field tbl fldname =
  Hashtbl.iter (fun (index_tbl,where,r) _ ->
		  if tbl=index_tbl && where=fldname then 
		    Hashtbl.remove indexes (index_tbl,where,r)) indexes

let add_to_index i (k,v) =
  let existing_results_in_index = try Hashtbl.find i k with _ -> [] in 
  Hashtbl.replace i k (v::existing_results_in_index)

(* -------------------- Version upgrade support utils *)

(* This function (which adds default values that are not already present to table rows) is used in both the create_row
   call (where we fill in missing fields not supplied by an older vsn of the db protocol during pool-rolling-upgrade);
   and in the state.db schema upgrade process (see db_upgrade.ml) *)

(* check default values, filling in if they're not present: [for handling older vsns of the db protocol] *)
(* it's not the most efficient soln to just search in the association list, looking up default fields from datamodel
   each time; but creates are rare and fields are few, so it's fine... *)
let add_default_kvs kvs tblname =

  (* given a default value from IDL, turn it into a string for db insertion. !!! This should be merged up with other
     marshalling code at some point (although this requires some major refactoring cos that's all autogenerated and
     not "dynamically typed".. *)
  let gen_db_string_value f =
    match f.Datamodel_types.default_value with
      Some v -> Datamodel_values.to_db_string v
    | None -> "" (* !!! Should never happen *) in
  
  let this_obj = List.find (fun obj-> (Gen_schema.sql_of_obj obj.Datamodel_types.name) = tblname) (Dm_api.objects_of_api Datamodel.all_api) in
  let default_fields = List.filter (fun f -> f.Datamodel_types.default_value <> None) (flatten_fields this_obj.Datamodel_types.contents []) in
  let default_values = List.map gen_db_string_value default_fields in
  let default_field_names = List.map (fun f -> Gen_schema.sql_of_id f.Datamodel_types.full_name) default_fields in
  let all_default_kvs = List.combine default_field_names default_values in
  (* only add kv pairs for keys that have not already been supplied to create_row call (in kvs argument) *)
  let keys_supplied = List.map fst kvs in
  kvs @ (List.filter (fun (k,_) -> not (List.mem k keys_supplied)) all_default_kvs)  

(* !!! Right now this is called at cache population time. It would probably be preferable to call it on flush time instead, so we
   don't waste writes storing non-persistent field values on disk.. At the moment there's not much to worry about, since there are
   few non-persistent fields. However, if this number increases we may want to consider calling this fn on every flush instead.. *)

(* non-persistent fields will have been flushed to disk anyway [since non-persistent just means dont trigger a flush
   if I change]. Hence we blank non-persistent fields with a suitable empty value, depending on their type *)
let blow_away_non_persistent_fields() =
  (* for each table, go through and blow away any non-persistent fields *)
  let remove_non_persistent_field_values_from_tbl tblname =
    let tbl = lookup_table_in_cache tblname in
    let rows = get_rowlist tbl in
    let this_obj = List.find (fun obj-> (Gen_schema.sql_of_obj obj.Datamodel_types.name) = tblname) (Dm_api.objects_of_api Datamodel.all_api) in
    let non_persist_fields = List.filter (fun f -> not f.Datamodel_types.field_persist) (flatten_fields this_obj.Datamodel_types.contents []) in
    let non_persist_fields_and_types = List.map (fun f -> f.Datamodel_types.ty, f) non_persist_fields in
    (* if this table doesn't have any non persistent fields then there's nothing to do... *)
    if non_persist_fields <> [] then
      begin
	let process_row r =
	  List.iter
	    (fun (ftype,f) ->
	       Hashtbl.replace r (Gen_schema.sql_of_id f.Datamodel_types.full_name) (Datamodel_values.gen_empty_db_val ftype))
	    non_persist_fields_and_types in
	List.iter process_row rows
      end in
  List.iter remove_non_persistent_field_values_from_tbl (List.map (fun x->Gen_schema.sql_of_obj x.Datamodel_types.name) api_objs)
  
(* after restoring from backup, we take the master's host record and make it reflect us *)
let post_restore_hook manifest =
  debug "Executing post_restore_hook";
  let my_installation_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
  let my_control_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in

  let uuid_of_master_in_db = manifest.Db_cache_types.installation_uuid in
  let uuid_of_master_dom0_in_db = manifest.Db_cache_types.control_domain_uuid in

  let find_row_by_uuid uuid tbl =
    let rs = get_rowlist tbl in
    try Some (List.find (fun r->(lookup_field_in_row r uuid_fname)=uuid) rs)
    with e -> (debug "find_row_by_uuid 'uuid=%s' failed: %s"
		 uuid (Printexc.to_string e);
	       None) in
  
  let master_host_record = find_row_by_uuid uuid_of_master_in_db (lookup_table_in_cache "host") in
  let master_dom0_record = find_row_by_uuid uuid_of_master_dom0_in_db (lookup_table_in_cache "VM") in

  (* update master host record in db, so it has my installation uuid from my inventory file *)
  begin
    match master_host_record with
      None -> debug "Not updating host record"
    | Some mhr -> 
	(set_field_in_row mhr uuid_fname my_installation_uuid;
	 let _ref = lookup_field_in_row mhr reference_fname in
	 Ref_index.update_uuid _ref my_installation_uuid)
  end;
  (* update master dom0 record in db, so it has my control domain uuid from my inventory file in dom0 *)
  begin
    match master_dom0_record with
      None -> debug "Not updating dom0 record"
    | Some mhr -> 
	(set_field_in_row mhr uuid_fname my_control_uuid;
	 let _ref = lookup_field_in_row mhr reference_fname in
	 Ref_index.update_uuid _ref my_control_uuid)
  end;
  debug "post_restore_hook executed"
