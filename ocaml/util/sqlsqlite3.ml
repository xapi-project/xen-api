(** SQLite3 interface *)

open Sqlcommon
open Listext

(* Time to sleep between retries on receiving SQL_BUSY *)
let sql_RETRY_TIME = 0.2
(* !!! should perhaps add some kind of backoff here, but with db cache this should be OK *)

let sql_MAX_TRIES = 10

let db_retry f =
  let tries = ref 0 in
  let rec loop () =
    try
      tries := !tries + 1;
      f ()
    with
	(Sqlite3.Sqlite3_error s) ->
	  Sqlcommon.D.debug "SQLite error [%s] in db_retry loop. Retrying..." s;
	  Thread.delay sql_RETRY_TIME;
	  if !tries<sql_MAX_TRIES then loop()
	  else raise (Sqlite3.Sqlite3_error s)
      | e -> raise e in
    loop()

(** Calls (f x) and interprets the error code. Written in the style of 
    handle_unix_error *)
let handle_db_error f x =  match f x with
  | Sqlite3.RC_ok -> ()
  | x -> 
      Sqlcommon.D.debug "Caught database error: %s" (Sqlite3.string_of_rc x);
      raise (Database_error (Sqlite3.string_of_rc x))

(** Handy wrapper function which takes a prepared statement (containing '?' holes)
    and a list of parameters. The callback is called for each row retrieved from
    the database. NB the use of the prepared statement is only for security, not
    performance *)

(* !!! FIXME -- Need binding to column_count (rather than data_count) to do this properly.
       Currently solutions works, but only because db-access layer ensures we never have
       null values in tables. The solution below also has the consequence that you can't
       read column names when the query is empty!
*)
(* Returns column list *)
let do_prepare sql db =
  db_retry (fun ()-> Sqlite3.prepare db sql) 

let do_finalize stmt = 
  try
    match Sqlite3.finalize stmt with
      | Sqlite3.RC_ok -> ()
      | x -> Sqlcommon.D.debug "Skipping finalize error: %s" (Sqlite3.string_of_rc x)
  with
      e -> Sqlcommon.D.debug "Ignoring exception raised in finalize: %s" (Printexc.to_string e)

let do_reset stmt =
  db_retry (fun () -> Sqlite3.reset stmt)

let do_exec_prepared callback stmt params =
  let columns = ref [] in
  let read_column_names() =
    let count = Sqlite3.data_count stmt in
    let rec f x =
      if x<count then (Sqlite3.column_name stmt x)::(f (x+1)) else [] in
    
    if count=0 then [] else List.rev (f 0) in
  try
    List.iteri (fun indx txt -> 
      handle_db_error (Sqlite3.bind stmt (indx+1)) (Sqlite3.Data_text txt)) params;
    
    let finished = ref false in
    while not(!finished) do
      match Sqlite3.step stmt with
	| Sqlite3.RC_row ->
	    begin
	      if !columns = [] then columns := read_column_names();
	      callback stmt !columns;
	    end
	| Sqlite3.RC_done -> finished := true
	| Sqlite3.RC_busy -> Thread.delay sql_RETRY_TIME
	| x ->
	    Sqlcommon.D.debug "Database error in step: %s" (Sqlite3.string_of_rc x);
	    finished := true;
	    raise (Database_error (Sqlite3.string_of_rc x))
    done;
    !columns
  with e ->
    Sqlcommon.D.debug "Caught exception in do_exec_prepared: %s" (Printexc.to_string e);
    do_finalize stmt;
    raise e
    
let exec_prepared callback sql params db =
  let stmt = do_prepare sql db in
  let result = do_exec_prepared callback stmt params in
  do_finalize stmt;
  result


let get_row stmt =
  let count = Sqlite3.data_count stmt in
  let rec f x =
    if x<count then
      let r = Sqlite3.column stmt x in
	match r with
	    Sqlite3.Data_text v -> v::(f (x+1))
	  | _ -> ""::(f (x+1))
    else [] in
    
    if count=0 then [] else List.rev (f 0)

let rec zip l1 l2 =
  match l1,l2 with
      [],[] -> []
    | x::xs, y::ys -> (x,y)::(zip xs ys)

(** Utility function to return the (field,value) pairse from
    a _single row, multiple column_ query i.e. "select * from tbl where uuid-<unique val>" *)
let do_select_single sql db = 
  let row_result = ref [] in
  let callback_done = ref false in
  let cols =
    exec_prepared (fun stmt _ -> callback_done := true; row_result := get_row stmt) sql [] db in
    if (!callback_done) then Some (zip cols !row_result) else None

let do_select sql db =
  let rows = ref [] in
  let cols =
    exec_prepared
      (fun stmt cols ->
	 let row = zip cols (get_row stmt) in
	 let rowtbl: (string,string) Hashtbl.t = Hashtbl.create 40 in
	   List.iter (fun (x,y)->Hashtbl.add rowtbl x y) row;
	   rows := rowtbl :: !rows) sql [] db in
    List.rev (!rows)

(*
  let rc =
    exec db sql
      (fun res hdr ->
	 callback_done := true;
	 for i=0 to (Array.length res)-1 do
	   match res.(i) with
	       None -> results := (hdr.(i), "") :: !results
	     | (Some x) -> results := (hdr.(i), x) :: !results
	 done) in
    match rc with
	RC_ok -> if (!callback_done) then Some (List.rev !results) else None
      | x -> raise (Database_error (string_of_rc x))
*)
 

(*
let do_select_table sql db =
  let row = ref [] in
  let rows = ref [] in
  let columns = ref [] in
  let callback_done = ref false in
  let rc =
    exec db sql
      (fun res hdr ->
	 callback_done := true;
	 if (!columns = []) then
	   begin
	     for i=0 to (Array.length hdr)-1 do
	       columns := hdr.(i) :: !columns
	     done
	   end;
	 for i=0 to (Array.length res)-1 do
	   match res.(i) with
	       None -> row := "" :: !row
	     | (Some x) -> row := x :: !row
	 done;
	 rows := !row :: !rows;
      ) in
    match rc with
	RC_ok -> if (!callback_done) then Some (List.rev !results) else None
      | x -> raise (Database_error (string_of_rc x))
*)

(** Utility function to return the results of a _multiple row, single column_ query
    i.e. "select fld from tbl" *)
let do_select_single_column sql params db =
  let results = ref [] in
    ignore (
	     exec_prepared 
	       (fun x cols ->
		  match Sqlite3.column x 0 with
		    | Sqlite3.Data_text x -> results := x :: !results
		    | _ -> ())
	       sql params db
	   );
  List.rev !results

(** Utility function which takes a keys_and_values association list and inserts
    the row in the database via a prepared query *)
let do_insert table keys_and_values db = 
  let sql, params = insert_query table keys_and_values in
  ignore (exec_prepared (fun _ _ -> ()) sql params db)

let begin_transaction db = ignore (exec_prepared (fun _ _ -> ()) "BEGIN TRANSACTION;" [] db)
let end_transaction db = ignore (exec_prepared (fun _ _ -> ()) "END TRANSACTION;" [] db)

(** Runs a side-effecting query *)
let run_query sql params db =
  ignore (exec_prepared (fun _ _ -> ()) sql params db)

(** Opens the database, executes a query and closes it again. NB must
    commit any open transactions before returning and allowing the database to
    close. *)
let open_database_with_filename fname f = 
  let db = db_retry (fun ()->Sqlite3.db_open fname) in
  try
    let result = f db in
    Sqlite3.db_close db;
    result
  with e ->
    Sqlite3.db_close db;
    raise e

(** create empty database from schema *)
let create_empty_db filename =
  begin try
    (* Will throw an exception if it doesn't exist *)
    Unix.unlink filename;
  with _ -> ()
  end;
  let dbh = Sqlite3.db_open filename in
  try
    (* Build the database *)
    handle_db_error (Sqlite3.exec dbh (read_schema())) (fun _ _ -> ());
    Sqlite3.db_close dbh;
  with e ->
    Sqlite3.db_close dbh;
    raise e
