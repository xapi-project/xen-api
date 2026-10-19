(*
 * Copyright (c) Cloud Software Group, Inc.
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

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

type claim = {host: string; guest: Vm_placement.Guest_snapshot.t}

type t = {m: Mutex.t; claims: (API.ref_VM, claim) Hashtbl.t}

let create () = {m= Mutex.create (); claims= Hashtbl.create 64}

let coordinator = create ()

let current t = Hashtbl.fold (fun vm c acc -> (vm, c) :: acc) t.claims []

let claims t = with_lock t.m (fun () -> current t)

let claim t ~vm choose =
  with_lock t.m (fun () ->
      let c = choose (current t) in
      Option.iter (Hashtbl.replace t.claims vm) c ;
      c
  )

let release t ~vm = with_lock t.m (fun () -> Hashtbl.remove t.claims vm)

let count t = with_lock t.m (fun () -> Hashtbl.length t.claims)
