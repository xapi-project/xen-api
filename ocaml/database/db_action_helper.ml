(** Helper functions called from the generated Db_action code. *)

exception Cannot_read_schema_version

(** Table column name which contains the reference *)
let reference = Escaping.reference

(** Table column name which contains the uuid *)
let uuid = "uuid"

open Threadext

module D = Debug.Debugger(struct let name = "sql" end)
open D

(* General DB utils *)

let __callback : ((?snapshot: XMLRPC.xmlrpc -> string -> string -> string -> unit) option ref) = ref None
let events_register f = __callback := Some f
let events_unregister () = __callback := None
    
let events_notify ?(snapshot) ty op ref =
  match !__callback with
    | None -> ()
    | Some f -> f ?snapshot ty op ref
	  
(* Return query used by update *)
let update_query tbl fldvalue fld objref =
  let sql = Printf.sprintf "UPDATE %s SET %s=? WHERE %s=?;" tbl fld reference in
  let params = [fldvalue; objref ] in
    (sql,params)
      
(* Return query used by delete *)
let deleterow_query tbl objref =
  let sql = Printf.sprintf "DELETE FROM %s WHERE %s=?;" tbl reference in
  let params = [ objref ] in
    (sql,params)
      
exception Db_set_or_map_parse_fail of string
  
let parse_sexpr s : SExpr.t list =
  match Sexpr.of_string s with
    | SExpr.Node xs -> xs
    | _ -> raise (Db_set_or_map_parse_fail s)
	
let add_key_to_set key set =
  if List.mem (SExpr.String key) set 
  then set
  else SExpr.String key :: set
