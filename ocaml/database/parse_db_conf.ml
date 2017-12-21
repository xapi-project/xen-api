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
(* !!! This needs to be moved out of xapi and into the database directory; probably being merged with db_connections !!! *)

open Xapi_stdext_std.Xstringext
open Xapi_stdext_unix

module D=Debug.Make(struct let name="xapi" end)
open D

type db_connection_mode = Write_limit | No_limit

type db_connection =
  {path:string;
   mode:db_connection_mode;
   compress:bool;
   write_limit_period:int;
   write_limit_write_cycles:int;
   is_on_remote_storage:bool;
   other_parameters:(string*string) list;
   mutable last_generation_count: Generation.t;
  }

let default_write_limit_period = 21600 (* 6 hours *)
let default_write_cycles = 10

(* a useful "empty" config value *)
let dummy_conf =
  {path=""; mode=No_limit;
   write_limit_period=default_write_limit_period;
   write_limit_write_cycles=default_write_cycles;
   compress=false;
   is_on_remote_storage=false;
   other_parameters=[];
   last_generation_count = Generation.null_generation;
  }

let make path = { dummy_conf with path = path }

let generation_filename dbconn = dbconn.path ^ Generation.suffix

(** Return the generation of a given database 'connection'. Note we normally
    	expect the database file and the generation file to be present together;
    	however after upgrade only the database file will be present. *)
let generation_read dbconn =
  let gencount_fname = generation_filename dbconn in
  try Generation.of_string (Unixext.string_of_file gencount_fname) with _ -> 0L


(* The db conf used for bootstrap purposes, e.g. mounting the 'real' db on shared storage *)
let db_snapshot_dbconn = {dummy_conf with
                          path=Db_globs.snapshot_db
                         }

let from_mode v =
  match v with
    Write_limit -> "write_limit"
  | No_limit -> "no_limit"

let from_block r =
  String.concat ""
    [
      Printf.sprintf
        "[%s]\nmode:%s\nformat:xml\ncompress:%b\nis_on_remote_storage:%b\n"
        r.path (from_mode r.mode) r.compress
        r.is_on_remote_storage;
      if r.mode = Write_limit then
        Printf.sprintf "write_limit_period:%d\nwrite_limit_write_cycles:%d\n"
          r.write_limit_period r.write_limit_write_cycles
      else "";
      String.concat "" (List.map (fun (k,v) -> Printf.sprintf "%s:%s\n" k v) r.other_parameters)
    ]

let write_db_conf connections =
  let dbconf = String.concat "\n" (List.map from_block connections) in
  Unixext.write_string_to_file !Db_globs.db_conf_path dbconf

let to_mode s =
  match s with
    "write_limit" -> Write_limit
  | "no_limit" -> No_limit
  | _ -> failwith (Printf.sprintf "unknown mode: %s" s)

exception Cannot_parse_database_config_file
exception Cannot_have_multiple_dbs_in_sr

let sanity_check connections =
  let conns_in_sr = List.filter (fun r->r.is_on_remote_storage) connections in
  if (List.length conns_in_sr)>1 then raise Cannot_have_multiple_dbs_in_sr

let parse_db_conf s =
  try
    let conf = Unixext.string_of_file s in
    let lines : string list ref = ref [] in
    let consume_line() = lines := List.tl !lines in
    lines := String.split '\n' conf;
    List.iter (fun line -> debug "%s" line) !lines;
    let read_block () =
      let path_line = List.hd !lines in
      let path = String.sub path_line 1 ((String.length path_line)-2) in
      consume_line();
      let key_values = ref [] in
      while (!lines<>[] && (List.hd !lines)<>"") do
        let line = List.hd !lines in
        key_values := (match (String.split ':' line) with
              k::vs->(String.lowercase k,String.lowercase (String.concat ":" vs))
            | _ -> failwith (Printf.sprintf "Failed to parse: %s" line)
          )::!key_values;
        consume_line();
      done;

      (* if the key_name exists then return the value; otherwise return the default.
         	 if the key_name exists we remove the value from the association list -- this is so at the end of
         	 populating the record what we have left are the "other_fields" *)
      let maybe_put_in key_name default conv_fn =
        if List.mem_assoc key_name !key_values then
          begin
            let value = List.assoc key_name !key_values in
            key_values := List.remove_assoc key_name !key_values;
            conv_fn value
          end
        else default in
      {path=path;
       mode=maybe_put_in "mode" (* key name *) No_limit (* default if key not present *) to_mode (* fn to conv string->mode type *);
       compress = maybe_put_in "compress" false bool_of_string;
       is_on_remote_storage = maybe_put_in "is_on_remote_storage" false bool_of_string;
       write_limit_period=maybe_put_in "write_limit_period" default_write_limit_period int_of_string;
       write_limit_write_cycles=maybe_put_in "write_limit_write_cycles" default_write_cycles int_of_string;
       other_parameters = !key_values; (* the things remaining in key_values at this point are the ones we haven't parsed out explicitly above.. *)
       last_generation_count = Generation.null_generation;
      } in
    let connections : db_connection list ref = ref [] in
    while !lines<>[] do
      let line = List.hd !lines in
      if String.startswith "[" line then
        connections := read_block() :: !connections
      else consume_line()
    done;
    sanity_check !connections;
    !connections
  with exn ->
    error "Database config parse failed: %s" (Printexc.to_string exn);
    Backtrace.reraise exn Cannot_parse_database_config_file

let get_db_conf path =
  if Sys.file_exists path then
    parse_db_conf path
  else begin
    warn "No db_conf file. Using default";
    [{path="/var/lib/xcp/state.db";
      mode=No_limit;
      compress=false;
      is_on_remote_storage=false;
      write_limit_period=default_write_limit_period;
      write_limit_write_cycles=default_write_cycles;
      other_parameters=["available_this_boot","true"];
      last_generation_count=Generation.null_generation}]
  end
