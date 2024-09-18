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

type t = {r: API.ref_session; pool: bool; last_active: Xapi_stdext_date.Date.t}

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let m = Mutex.create ()

let table = Hashtbl.create 10

let get_all ~__context =
  with_lock m (fun () -> Hashtbl.fold (fun k _ acc -> k :: acc) table [])

let create ~__context ~pool =
  let r = Ref.make () in
  let session = {r; pool; last_active= Xapi_stdext_date.Date.now ()} in
  with_lock m (fun () -> Hashtbl.replace table r session) ;
  r

let get_record ~__context ~self = with_lock m (fun () -> Hashtbl.find table self)

let destroy ~__context ~self = with_lock m (fun () -> Hashtbl.remove table self)

let local_session_hook ~__context ~session_id =
  try
    ignore (get_record ~__context ~self:session_id) ;
    true
  with _ -> false
