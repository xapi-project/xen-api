(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

open Stdext.Threadext

(** A table of 'instance' locks with a single master lock *)
type ('a, 'b) t = {
  m: Mutex.t;
  c: Condition.t;
  t: ('a, 'b) Hashtbl.t;
  mutable master_lock: bool; (* Acquire this to prevent other locks being held *)
}

let make () = {
  m = Mutex.create ();
  c = Condition.create ();
  t = Hashtbl.create 10;
  master_lock = false
}

(** Execute the function with the specified instance locked *)
let with_instance_lock t key f =
  let r = Locking_helpers.Lock ("SM/" ^ (Ref.really_pretty_and_small (Ref.of_string key))) in
  Locking_helpers.Thread_state.waiting_for r;
  Mutex.execute t.m
    (fun () ->
       (* Wait for the lock to be free (ie the table entry to be removed and the master lock to be released *)
       while Hashtbl.mem t.t key || t.master_lock do Condition.wait t.c t.m done;
       Hashtbl.replace t.t key ()
    );
  Locking_helpers.Thread_state.acquired r;
  Stdext.Pervasiveext.finally f
    (fun () ->
       Mutex.execute t.m
         (fun () ->
            Hashtbl.remove t.t key;
            Condition.broadcast t.c
         );
       Locking_helpers.Thread_state.released r;
    )

(** Execute the function with the master_lock held and no instance locks held *)
let with_master_lock t f =
  let r = Locking_helpers.Lock "SM" in
  Locking_helpers.Thread_state.waiting_for r;
  Mutex.execute t.m
    (fun () ->
       (* Wait for the master_lock to be released *)
       while t.master_lock do Condition.wait t.c t.m done;
       (* Grab the master_lock *)
       t.master_lock <- true;
       (* Wait for all instance locks to be released *)
       while Hashtbl.length t.t > 0 do Condition.wait t.c t.m done
    );
  Locking_helpers.Thread_state.acquired r;
  Stdext.Pervasiveext.finally f
    (fun () ->
       Mutex.execute t.m
         (fun () ->
            t.master_lock <- false;
            Condition.broadcast t.c
         );
       Locking_helpers.Thread_state.released r;
    )


