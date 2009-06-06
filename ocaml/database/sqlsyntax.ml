
type table = {
  name : string;
  fields : string list
}
    
type insert = {
  table : string;
  values : string list;
}
    
type t = 
      Create of table
    | Insert of insert
    | Commit
    | Begin

let make_create name fields =
  Create {name=name; fields = List.map fst fields}

let output oc sql =
  match sql with
      Create t ->
	Printf.fprintf oc "CREATE TABLE %s ( %s );\n"
	  t.name 
	  (String.concat "," (List.map (fun f -> Printf.sprintf "%s text" f) t.fields))
    | Insert t ->
	Printf.fprintf oc "INSERT INTO \"%s\" VALUES(%s);\n"
	  t.table
 	  (String.concat ", " (List.map (fun f -> Printf.sprintf "'%s'" f) t.values))
    | Begin -> Printf.fprintf oc "BEGIN TRANSACTION;\n"
    | Commit -> Printf.fprintf oc "COMMIT;\n"
