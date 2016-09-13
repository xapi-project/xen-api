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
(** Keep track of changes to the database by writing deltas to a block device. Communicates with another process which does the block device I/O. *)

(** {2 VDI related} *)

val get_static_device : string -> string option
(** Finds an attached metadata VDI with a given reason *)
val minimum_vdi_size : int64
(** Minimum size for redo log VDI *)
val redo_log_sm_config : (string * string) list
(** SM config for redo log VDI *)

(** {redo_log data type} *)
type redo_log = {
  name: string;
  marker: string;
  read_only: bool;
  enabled: bool ref;
  device: string option ref;
  currently_accessible: bool ref;
  state_change_callback: (bool -> unit) option;
  time_of_last_failure: float ref;
  backoff_delay: int ref;
  sock: Unix.file_descr option ref;
  pid: (Forkhelpers.pidty * string * string) option ref;
  dying_processes_mutex: Mutex.t;
  num_dying_processes: int ref;
}

(** {2 Enabling and disabling writing} *)

val is_enabled : redo_log -> bool
(** Returns [true] iff writing deltas to the block device is enabled. *)
val enable : redo_log -> string -> unit
(** Enables writing deltas to the block device. Subsequent modifications to the database will be persisted to the block device. Takes a static-VDI reason as argument to select the device to use. *)
val enable_block : redo_log -> string -> unit
(** Enables writing deltas to the block device. Subsequent modifications to the database will be persisted to the block device. Takes a path as argument to select the device to use. *)
val disable : redo_log -> unit
(** Disables writing deltas to the block device. Subsequent modifications to the database will not be persisted to the block device. *)

(** Communication with other threads. *)

val redo_log_events: (string * bool) Event.channel

(** {2 Lifecycle of I/O process} *)

val startup : redo_log -> unit
(** Start the I/O process. Will do nothing if it's already started. *)
val shutdown : redo_log -> unit
(** Stop the I/O process. Will do nothing if it's not already started. *)
val switch : redo_log -> string -> unit
(** Start using the VDI with the given reason as redo-log, discarding the current one. *)

(** {Keeping track of existing redo_log instances} *)
val create: name:string -> state_change_callback:(bool -> unit) option -> read_only:bool -> redo_log
(* Create a redo log instance and add it to the set. *)

val delete: redo_log -> unit
(* Shutdown a redo_log instance and remove it from the set. *)

(** {Finding active redo_log instances} *)
val with_active_redo_logs : (redo_log -> unit) -> unit
(* Apply the supplied function to all active redo_logs. *)

(** {2 Interacting with the block device} *)

(** The type of a delta, describing an incremental change to the database. *)
type t =
  | CreateRow of string * string * (string*string) list
  (** [CreateRow (tblname, newobjref, [(k1,v1); ...])]
      represents the creation of a row in table [tblname], with key [newobjref], and columns [[k1; ...]] having values [[v1; ...]]. *)
  | DeleteRow of string * string
  (** [DeleteRow (tblname, objref)]
      represents the deletion of a row in table [tblname] with key [objref]. *)
  | WriteField of string * string * string * string
  (** [WriteField (tblname, objref, fldname, newval)]
      represents the write to the field with name [fldname] of a row in table [tblname] with key [objref], overwriting its value with [newval]. *)

val write_db : Generation.t -> (Unix.file_descr -> unit) -> redo_log -> unit
(** Write a database.
    This function is best-effort only and does not raise any exceptions in the case of error.
    [write_db gen_count f] is used to write a database with generation count [gen_count] to the block device.
    A file descriptor is passed to [f] which is expected to write the contents of the database to it. *)

val write_delta : Generation.t -> t -> (unit -> unit) -> redo_log -> unit
(** Write a database delta.
    This function is best-effort only and does not raise any exceptions in the case of error.
    [write_delta gen_count delta db_flush_fn] writes a delta [delta] with generation count [gen_count] to the block device.
    If the redo log wishes to flush the database before writing the delta, it will invoke [db_flush_fn]. It is expected that this function implicitly attempts to reconnect to the block device I/O process if not already connected. *)

val apply : (Generation.t -> Unix.file_descr -> int -> float -> unit) -> (Generation.t -> t -> unit) -> redo_log -> unit
(** Read from the block device.
    This function is best-effort only and does not raise any exceptions in the case of error.
    [apply db_fn delta_fn] will cause [db_fn] and [delta_fn] to be invoked for each database or database delta which is read.
    The block device will consist of a sequence of zero or more databases and database deltas.
    For each database, [db_fn] is invoked with the database's generation count, a file descriptor from which to read the database's contents, the length of the database in bytes and the latest response time. The [db_fn] function may raise {!Unixext.Timeout} if the transfer is not complete by the latest response time.
    For each database delta, [delta_fn] is invoked with the delta's generation count and the value of the delta. *)

val empty : redo_log -> unit
(** Invalidate the block device. This means that subsequent attempts to read from the block device will not find anything.
    This function is best-effort only and does not raise any exceptions in the case of error. *)

val flush_db_to_redo_log: Db_cache_types.Database.t -> redo_log -> bool
(** Immediately write the given database to the given redo_log instance *)

val flush_db_to_all_active_redo_logs: Db_cache_types.Database.t -> unit
(** Immediately write the given database to all active redo logs *)

val database_callback: Db_cache_types.update -> Db_cache_types.Database.t -> unit
(** Given a database update, add it to all active redo logs *)

