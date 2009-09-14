open Db_cache_types
open Pervasiveext
module R = Debug.Debugger(struct let name = "redo_log" end)

(** Functions to marshall/unmarshall the database as XML *)

exception Unmarshall_error of string

let name x = ("", x) (* no namespace *)
let make_tag n attrs : Xmlm.tag = (name n), List.map (fun (k, v) -> name k, v) attrs

let _installation_uuid = "installation_uuid"
let _control_domain_uuid = "control_domain_uuid"
let _pool_conf = "pool_conf"
let _pool_token = "pool_token"
let _schema_major_vsn = "schema_major_vsn"
let _schema_minor_vsn = "schema_minor_vsn"
let _product_version = "product_version"
let _product_brand = "product_brand"
let _build_number = "build_number"
let _xapi_major_vsn = "xapi_major_vsn"
let _xapi_minor_vsn = "xapi_minor_vsn"
let _generation_count = "generation_count"

module To = struct

  (* Write out a key/value pair *)
  let pair (output: Xmlm.output) (key: string) (v: string) = 
    Xmlm.output_signal output (`S (make_tag "pair" [ "key", key; "value", v ]));
    Xmlm.output_signal output `E 
  (* Write out a string *)
  let string (output: Xmlm.output) (key: string) (x: string) = pair output key x
  (* Write out an int *)
  let int (output: Xmlm.output) (key: string) (x: int) = pair output key (string_of_int x)
  (* Write out an int64 *)
  let int64 (output: Xmlm.output) (key: string) (x: Int64.t) = pair output key (Int64.to_string x)

  (* Marshal a whole database table to an Xmlm output abstraction *)
  let table (output: Xmlm.output) name (tbl: table) = 
    let record rf (row: row) = 
      let (tag: Xmlm.tag) = make_tag "row" (("ref", rf) :: (fold_over_fields (fun k v acc -> (k, Xml_spaces.protect v) :: acc) row [])) in
      Xmlm.output_signal output (`S tag);
      Xmlm.output_signal output `E in
    let tag = make_tag "table" [ "name", name ] in
    Xmlm.output_signal output (`S tag);
    (* we write a table entry whether or not the table persists, because populate happens to assume
       that all tables will be present. However, if the table is marked as "don't persist" then we
       don't write any row entries: *)
    if Db_backend.this_table_persists name then
      iter_over_rows record tbl;
    Xmlm.output_signal output `E
	
  (* Write out a manifest *)
  let manifest (output: Xmlm.output) (manifest: db_dump_manifest) : unit = 
    Xmlm.output_signal output (`S (make_tag "manifest" []));
    string output _installation_uuid manifest.installation_uuid;
    string output _control_domain_uuid manifest.control_domain_uuid;
    string output _pool_conf manifest.pool_conf;
    string output _pool_token manifest.pool_token;
    int    output _schema_major_vsn manifest.schema_major_vsn;
    int    output _schema_minor_vsn manifest.schema_minor_vsn;
    string output _product_version manifest.product_version;
    string output _product_brand manifest.product_brand;
    string output _build_number manifest.build_number;
    int    output _xapi_major_vsn manifest.xapi_major_vsn;
    int    output _xapi_minor_vsn manifest.xapi_minor_vsn;
    int64  output _generation_count manifest.generation_count;
    Xmlm.output_signal output `E 

  (* Write out a full database cache dump *)
  let cache (output: Xmlm.output) (m, cache) : unit =
    Xmlm.output_signal output (`S (make_tag "database" []));
    manifest output m;
    iter_over_tables (table output) cache;
    Xmlm.output_signal output `E

  let fd (fd: Unix.file_descr) (m, c) : unit = 
    let oc = Unix.out_channel_of_descr fd in
    cache (Xmlm.output_of_channel oc) (m, c);
    flush oc

  let file (filename: string) (m, c) : unit = 
    let fdescr = Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
    finally
      (fun () -> fd fdescr (m, c))
      (fun () -> Unix.close fdescr)
end

module From = struct

  let cache (input: Xmlm.input) =
    (* On reading a start tag... *)
    let s (tag: Xmlm.tag) ((cache, table, manifest) as acc) = match tag with
      | (_, ("database" | "manifest")), _ -> acc
      | (_, "table"), [ (_, "name"), _ ] ->
	  cache, create_empty_table (), manifest
      | (_, "row"), ((_, "ref"), rf) :: rest ->
	  let row = create_empty_row () in
	  List.iter (fun (("", k), v) -> set_field_in_row row k (Xml_spaces.unprotect v)) rest;
	  set_row_in_table table rf row;
	  acc
      | (_, "pair"), [ (_, "key"), k; (_, "value"), v ] ->
	  (cache, table, (k, v) :: manifest)
      | (_, name), _ -> raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name)) in
    (* On reading an end tag... *)
    let e tag ((cache, table, manifest) as acc) = match tag with
      | (_, ("database" | "manifest" | "row" | "pair")), _ -> acc
      | (_, "table"), [ (_, "name"), name ] ->
	  set_table_in_cache cache name table;
	  acc
      | (_, name), _ -> raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name)) in
    let (cache, _, manifest) = Xmlm.input ~s ~e (create_empty_cache (), create_empty_table (), []) input in
    (* Manifest is actually a record *)
    let manifest = { 
      installation_uuid = List.assoc _installation_uuid manifest;
      control_domain_uuid = List.assoc _control_domain_uuid manifest;
      pool_conf = List.assoc _pool_conf manifest;
      pool_token = List.assoc _pool_token manifest;
      schema_major_vsn = int_of_string (List.assoc _schema_major_vsn manifest);
      schema_minor_vsn = int_of_string (List.assoc _schema_minor_vsn manifest);
      product_version = List.assoc _product_version manifest;
      product_brand = List.assoc _product_brand manifest;
      build_number = List.assoc _build_number manifest;
      xapi_major_vsn = int_of_string (List.assoc _xapi_major_vsn manifest);
      xapi_minor_vsn = int_of_string (List.assoc _xapi_minor_vsn manifest);
      generation_count = Int64.of_string (List.assoc _generation_count manifest)
    } in
    manifest, cache

  let file xml_filename =
    let input = open_in xml_filename in
    finally 
      (fun () -> cache (Xmlm.input_of_channel input)) 
      (fun () -> close_in input) 

  let channel inchan =
    cache (Xmlm.input_of_channel inchan)

end

