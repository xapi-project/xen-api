(*
 * Copyright (C) 2024 Cloud Software Group.
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

(** Eventgen is responsible for producing events for the event APIs
    ([event.next] and [event.from]) to provide to clients.

    Each update may result in many events, each with a snapshot of the
    related object(s), marshalled as [Rpc.t] dictionaries. *)

open Xapi_database.Db_cache_types

type get_record = unit -> Rpc.t
(* Type of the thunks used to read a record, marshalled as an [Rpc.t]
   dictionary, from the database.

   In practice, such functions arise from functions provided by the
   generated [Db_actions] module - which contains the logic to
   serialise internal and external views of database records.

   This type is emphasised because values of this type are used as
   thunks to delay evaluation, such that the actual fetching of
   object snapshots can be done in an orderly way by this module,
   when producing events. The closures produced by [Db_actions]
   capture the relevant context and reference information to produce
   a snapshot on-demand, when invoked. *)

val set_get_record :
  string -> (__context:Context.t -> self:string -> get_record) -> unit
(** [set_get_record table accessor] is used by [Db_actions] to
    register a means by which this module can read records from
    database, in order to produce snapshots used by the events
    mechanism.

    Upon initialisation, [Db_actions] calls [set_get_record] to
    register an accessor for each object type stored in the
    database. These accessors consist of logic internal to
    [Db_actions] which performs all the related reading and
    marshalling of values from the database.

    This function should not be called by any module other than
    [Db_actions]. *)

val find_get_record :
  string -> __context:Context.t -> self:string -> unit -> Rpc.t option
(** [find_get_record table context reference] yields a partial
    function which, when invoked, attempts to read a record snapshot from
    the database. Any [table] used must have already been registered
    by initialisation code within [Db_actions] (i.e. from a previous
    call to [set_get_record]).

    The function returns an option type as a convenience, as the
    inherent delaying of the evaluation of snapshots could mean that a
    record referred to by [reference] is no longer present in the
    database. *)

val database_callback : update -> Database.t -> unit
(** [database_call update db] notifies [Xapi_event] (indirectly) of
    transitive events arising from a single logical [update] within the
    database.

    Many events may follow a single [update] as referential fields,
    related by the datamodel schema, may produce changes in related
    objects (so, previously and newly referenced objects' snapshots
    must be emitted as modification events). *)
