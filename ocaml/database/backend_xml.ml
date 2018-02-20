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

module D = Debug.Make(struct let name = "sql" end)
open D

open Db_cache_types
open Db_backend

let unmarshall schema dbconn =
  let filename = dbconn.Parse_db_conf.path in
  if not dbconn.Parse_db_conf.compress
  then Db_xml.From.file schema filename
  else
    let compressed = Unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () ->
         let result = ref None in
         Gzip.decompress_passive compressed
           (fun uncompressed ->
              result := Some (Db_xml.From.channel schema (Unix.in_channel_of_descr uncompressed))
           );
         match !result with
         | None -> failwith "unmarshal failure"
         | Some x -> x
      )
      (fun () -> Unix.close compressed)

(* Given table name, read all rows from db and store in cache *)
let populate schema dbconn =
  debug "attempting to restore database from %s" dbconn.Parse_db_conf.path;
  let db = unmarshall schema dbconn in
  let major, minor = Manifest.schema (Database.manifest db) in
  debug "database unmarshalled, schema version = %d.%d" major minor;
  (* version_check manifest; *)
  db

(* atomically flush entire db cache to disk. If we are given a cache then flush that, otherwise flush the
   current state of the global in-memory cache *)

let flush ?(fsync=false) dbconn db =
  let open Xapi_stdext_unix in
  let time = Unix.gettimeofday() in

  let do_flush_xml db filename =
    Redo_log.flush_db_to_all_active_redo_logs db;
    Unixext.atomic_write_to_file filename 0o0644
      (fun fd ->
         if not dbconn.Parse_db_conf.compress
         then Db_xml.To.fd fd db ~fsync
         else
           Gzip.compress fd
             (fun uncompressed -> Db_xml.To.fd uncompressed db ~fsync);
      ) in

  let do_flush_gen db filename =
    let generation = Manifest.generation (Database.manifest db) in
    Unixext.write_string_to_file filename (Generation.to_string generation) in

  let filename = dbconn.Parse_db_conf.path in
  do_flush_xml db filename;
  let generation_filename = Parse_db_conf.generation_filename dbconn in
  do_flush_gen db generation_filename;

  debug "XML backend [%s] -- Write buffer flushed. Time: %f" filename (Unix.gettimeofday() -. time)


(* NB We don't do incremental flushing *)

let flush_dirty ?(fsync=false) dbconn =
  let db = Db_ref.get_database (Db_backend.make ()) in
  let g = Manifest.generation (Database.manifest db) in
  if g > dbconn.Parse_db_conf.last_generation_count then begin
    flush dbconn db ~fsync;
    dbconn.Parse_db_conf.last_generation_count <- g;
    true
  end else false



