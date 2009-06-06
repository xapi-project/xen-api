(** Generate SQLite Schema from datamodel.ml. Doesn't handle many-to-many
    relationships yet; not sure if we should have those in datamodel.ml
    anyway... *)

open Datamodel_types
open Dm_api
open Datamodel_utils

(* Put any filtering we need here *)
let api = Datamodel.all_api

let reference = "_ref"
let version_table = "schema_version"

(** Take a field name as a list (including namespaces) and return a flat name *)
let sql_of_id x = String.concat "__" x

(** Object/class names are used as-is *)
let sql_of_obj x = x

let sqltype_of_ty t =
  "text" (* just make everything text in the db for now :) *)

(** We don't include fields that are the many-side of a one-to-many relationship.*)
let field_in_database = function
  | { ty = Set(Ref _) } -> false
  | _ -> true

let sql_of_api (api: api) = 
  let field f = String.concat "__" f.full_name  ^ " " ^  (sqltype_of_ty f.ty) in
  let obj x = 
    let table_name = sql_of_obj x.name in
    let all_fields = List.map field (List.filter field_in_database (fields_of_obj x)) in
    let all_fields = (reference ^ " text") :: all_fields in
    String.concat "\n" [
      Printf.sprintf "CREATE TABLE %s (" table_name;
      String.concat ", \n" all_fields;
      ");"
    ] in
  List.map obj (objects_of_api api)

let vsn_create =
  "CREATE TABLE "^(version_table)^" (major text, minor text);\n"
  ^"INSERT INTO "^(version_table)^" (major, minor) VALUES ('"
        ^(string_of_int Datamodel.schema_major_vsn)^"','"
        ^(string_of_int Datamodel.schema_minor_vsn)^"');"    

let sql_schema api =
  let from_api = sql_of_api api in
  String.concat "\n\n" (from_api @ [vsn_create])
