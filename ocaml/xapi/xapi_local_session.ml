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
(** Code to handle local sessions, used so that slaves can communicate even when
    the master is down. *)

type t = {
  r: API.ref_session;
  pool: bool;
  last_active: Stdext.Date.iso8601 }

open Stdext.Threadext

let m = Mutex.create ()
let table = Hashtbl.create 10

let get_all ~__context = Mutex.execute m (fun () -> Hashtbl.fold (fun k v acc -> k :: acc) table [])

let create ~__context ~pool =
  let r = Ref.make () in
  let session = { r = r; pool = pool; last_active = Stdext.Date.of_float (Unix.gettimeofday ()) } in
  Mutex.execute m (fun () -> Hashtbl.replace table r session);
  r

let get_record ~__context ~self =
  Mutex.execute m (fun () -> Hashtbl.find table self)

let destroy ~__context ~self =
  Mutex.execute m (fun () -> Hashtbl.remove table self)

let local_session_hook ~__context ~session_id =
  try ignore(get_record ~__context ~self:session_id); true
  with _ -> false

