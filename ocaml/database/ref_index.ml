(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
(* This data is accessible only on the pool master, since the data all lives there.
   Unlike the "first-class" db calls, these are not marshalled over the wire if called
   on a slave -- so don't ever call them on slaves! :) *)

open Db_cache_types
open Xapi_stdext_monadic

type indexrec = {
  name_label:string option;
  uuid: string;
  _ref:string
}
let string_of (x: indexrec) =
  Printf.sprintf "%s%s" x.uuid (Opt.default "" (Opt.map (fun name -> Printf.sprintf " (%s)" name) x.name_label))

let lookup key =
  let t = Db_backend.make () in
  let db = Db_ref.get_database t in
  let r (tblname, objref) =
    let row = Table.find objref (TableSet.find tblname (Database.tableset db)) in {
      name_label = (try Some (Schema.Value.Unsafe_cast.string (Row.find Db_names.name_label row)) with _ -> None);
      uuid = Schema.Value.Unsafe_cast.string (Row.find Db_names.uuid row);
      _ref = Schema.Value.Unsafe_cast.string (Row.find Db_names.ref row);
    } in
  Opt.map r (Database.lookup_key key db)


