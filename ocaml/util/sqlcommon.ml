(** SQL helper functions *)
let schema_filename = ref "/etc/xensource/db_schema.sql"

module D=Debug.Debugger(struct let name="sql" end)
open D

open Listext

let insert_query table keys_and_values =
  let sql = Printf.sprintf "INSERT INTO %s (%s) VALUES(%s)"
    table
    (String.concat ", " (List.map fst keys_and_values))
    (String.concat ", " (List.map (fun _ -> "?") keys_and_values)) in
  let params = List.map snd keys_and_values in
    sql, params

exception Database_error of string

let read_schema () =
  debug "Reading SQL schema from %s" !schema_filename;
  let lines = ref [] in
  let ic = open_in !schema_filename in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done; ""
  with End_of_file ->
    close_in ic;
    String.concat "\n" (List.rev !lines)
  | e ->
      close_in ic;
      raise e
