(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module D = Debug.Make(struct let name = "xapi-db-process" end)
open D

open Db_cache_types

let compress = ref false
let filename = ref ""
let operation = ref ""
let config = ref ""
let iqn = ref ""
let xmltostdout = ref false

let init_logs() = Debug.set_facility Syslog.Local5

let fatal_error s =
  print_string s;
  print_string "\n";
  exit 1

type operation = Write_database | Read_gencount | Read_hostiqn | Write_hostiqn
               | Am_i_in_the_database | Unknown of string
let parse_operation s =
  match (String.lowercase_ascii s) with
  | "write_db" -> Write_database
  | "read_gencount" -> Read_gencount
  | "read_hostiqn" -> Read_hostiqn
  | "write_hostiqn" -> Write_hostiqn
  | "am_i_in_the_database" -> Am_i_in_the_database
  | s                      -> (Unknown s)

let initialise_db_connections() =
  let dbs = Parse_db_conf.parse_db_conf
      (if !config="" then !Db_globs.db_conf_path else !config) in
  Db_conn_store.initialise_db_connections dbs;
  dbs

let read_in_database() =
  let connections = initialise_db_connections() in
  (* Initialiase in-memory database cache *)
  Db_cache_impl.make (Db_backend.make ()) connections Schema.empty

let write_out_databases() =
  Db_cache_impl.sync (Db_conn_store.read_db_connections ()) (Db_ref.get_database (Db_backend.make ()))

(* should never be thrown due to checking argument at start *)
exception UnknownFormat

let write_out_database filename =
  print_string ("Dumping database to: "^filename^"\n");
  Db_cache_impl.sync
    [ {
      Parse_db_conf.dummy_conf with
      Parse_db_conf.path=filename;
      Parse_db_conf.mode=Parse_db_conf.No_limit;
      Parse_db_conf.compress=(!compress)
    } ] (Db_ref.get_database (Db_backend.make ()))

let help_pad = "      "
let operation_list =
  String.concat "\n"
    (List.map (fun s -> help_pad^s)
       ["write_db      -- output the database";
        "read_gencount -- read maximum gencount of databases listed in db.conf";
        "read_hostiqn  -- read the initiator IQN for this host from the db host table";
        "write_hostiqn -- write a new initiator IQN for this host into all db's host tables"
       ]
    )

let do_write_database() =
  if (not !xmltostdout) then
    begin
      if !filename = "" then
        fatal_error "No filename specified, and xmltostdout option not set\n"
    end;
  begin
    read_in_database();
    if !xmltostdout then
      Db_xml.To.fd (Unix.descr_of_out_channel stdout) (Db_ref.get_database (Db_backend.make()))
    else
      write_out_database !filename
  end

let find_my_host_row() =
  Inventory.read_inventory ();
  let localhost_uuid = Inventory.lookup Inventory._installation_uuid in
  let db = Db_ref.get_database (Db_backend.make ()) in
  let tbl = TableSet.find Db_names.host (Database.tableset db) in
  Table.fold (fun r _ row acc -> if Schema.Value.Unsafe_cast.string (Row.find Db_names.uuid row) = localhost_uuid then (Some (r, row)) else acc) tbl None

let _iscsi_iqn = "iscsi_iqn"
let _other_config = "other_config"

let do_read_hostiqn() =
  read_in_database();
  match find_my_host_row() with
  | None -> failwith "No row for localhost"
  | Some (_, row) ->
    let other_config = Schema.Value.Unsafe_cast.pairs (Row.find Db_names.other_config row) in
    Printf.printf "%s" (List.assoc _iscsi_iqn other_config)

let do_write_hostiqn() =
  if !iqn = "" then
    fatal_error "Must specify '-hostiqn <value>'";
  let new_iqn = !iqn in
  read_in_database();
  match find_my_host_row() with
  | None -> failwith "No row for localhost"
  | Some (r, row) ->
    (* read other_config from my row, replace host_iqn if already there, add it if its not there and write back *)
    let other_config = Schema.Value.Unsafe_cast.pairs (Row.find Db_names.other_config row) in
    let other_config =
      if List.mem_assoc _iscsi_iqn other_config then
        (* replace if key already exists *)
        List.map (fun (k,v) ->k, if k=_iscsi_iqn then new_iqn else v) other_config
      else
        (* ... otherwise add new key/value pair *)
        (_iscsi_iqn,new_iqn)::other_config in
    let other_config = Schema.Value.Pairs other_config in
    Db_ref.update_database (Db_backend.make ()) (set_field Db_names.host r Db_names.other_config other_config);
    write_out_databases()

let do_am_i_in_the_database () =
  read_in_database();
  Printf.printf "%b" (find_my_host_row () <> None)

let _ =
  init_logs();
  Arg.parse ([
      "-compress", Arg.Set compress, "whether to compress the XML output";
      "-config", Arg.Set_string config, "config file to read";
      "-filename", Arg.Set_string filename, "filename to write to";
      "-xmltostdout", Arg.Set xmltostdout, "write XML db to stdout [compress/filename ignored if this option is present]";
      "-operation", Arg.Set_string operation, "operation to perform:\n"^operation_list^"\n"^help_pad^"(defaults to write_db if no operation specified)";
      "-hostiqn", Arg.Set_string iqn, "hostiqn value"
    ])
    (fun x -> print_string ("Warning, ignoring unknown argument: "^x))
    "XE database tool";
  if !operation = "" then
    operation := "write_db";
  info "xapi-db-process executed: operation='%s'" !operation;
  match parse_operation !operation with
  | Write_database ->
    do_write_database()
  | Read_hostiqn ->
    do_read_hostiqn()
  | Write_hostiqn ->
    do_write_hostiqn()
  | Am_i_in_the_database ->
    do_am_i_in_the_database()
  | _ ->
    error "unknown operation %s" !operation
