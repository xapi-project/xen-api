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
module R = Debug.Make(struct let name = "redo_log" end)

exception NoGeneration
exception DeltaTooOld
exception DatabaseWrongSize of int * int

let read_from_redo_log log staging_path db_ref =
  try
    (* 1. Start the process with which we communicate to access the redo log *)
    R.debug "Starting redo log";
    Redo_log.startup log;

    (* 2. Read the database and any deltas from it into the in-memory cache, applying the deltas to the database *)
    let latest_generation = ref None in

    let read_db gen_count fd expected_length latest_response_time =
      (* Read the database from the fd into a file *)
      let temp_file = Filename.temp_file "from-vdi" ".db" in
      Stdext.Pervasiveext.finally
        (fun () ->
           let outfd = Unix.openfile temp_file [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] 0o755 in
           (* ideally, the reading would also respect the latest_response_time *)
           let total_read = Stdext.Unixext.read_data_in_string_chunks (fun str length -> 
            Stdext.Unixext.time_limited_write_substring outfd length str latest_response_time) fd 
          in
           R.debug "Reading database from fd into file %s" temp_file;

           (* Check that we read the expected amount of data *)
           R.debug "We read %d bytes and were told to expect %d bytes" total_read expected_length;
           if total_read <> expected_length then raise (DatabaseWrongSize (expected_length, total_read));

           (* Read from the file into the cache *)
           let conn = Parse_db_conf.make temp_file in
           (* ideally, the reading from the file would also respect the latest_response_time *)
           let db = Backend_xml.populate (Datamodel_schema.of_datamodel ()) conn in
           Db_ref.update_database db_ref (fun _ -> db);

           R.debug "Finished reading database from %s into cache (generation = %Ld)" temp_file gen_count;

           (* Set the generation count *)
           latest_generation := Some gen_count
        )
        (fun () ->
           (* Remove the temporary file *)
           Stdext.Unixext.unlink_safe temp_file
        )
    in

    let read_delta gen_count delta =
      (* Apply the delta *)
      Db_cache.apply_delta_to_cache delta db_ref;
      (* Update the generation count *)
      match !latest_generation with
      | None -> raise NoGeneration (* we should have already read in a database with a generation count *)
      | Some g ->
        if gen_count > g
        then latest_generation := Some gen_count
        else raise DeltaTooOld (* the delta should be at least as new as the database to which it applies *)
    in

    R.debug "Reading from redo log";
    Redo_log.apply read_db read_delta log;

    (* 3. Write the database and generation to a file
     * Note: if there were no deltas applied then this is semantically
     *   equivalent to copying the temp_file used above in read_db rather than
     *   deleting it.
     * Note: we don't do this using the DB lock since this is only executed at
     *   startup, before the database engine has been started, so there's no
     *   danger of conflicting writes. *)
    R.debug "Staging redo log to file %s" staging_path;
    (* Remove any existing file *)
    Stdext.Unixext.unlink_safe staging_path;
    begin
      match !latest_generation with
      | None ->
        R.debug "No database was read, so no staging is necessary";
        raise NoGeneration
      | Some generation ->
        R.debug "Database from redo log has generation %Ld" generation;
        (* Write the in-memory cache to the file *)
        (* Make sure the generation count is right -- is this necessary? *)
        Db_ref.update_database db_ref (Db_cache_types.Database.set_generation generation);
        let db = Db_ref.get_database db_ref in
        Db_xml.To.file staging_path db;
        Stdext.Unixext.write_string_to_file (staging_path ^ ".generation") (Generation.to_string generation)
    end
  with _ -> () (* it's just a best effort. if we can't read from the log, then don't worry. *)

let stop_using_redo_log log =
  R.debug "Stopping using redo log";
  try
    Redo_log.shutdown log
  with _ -> () (* best effort only *)

