(*
 * Copyright (C) Citrix Systems Inc.
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

module D=Debug.Make(struct let name="xapi_clustering" end)
open D

(* host-local clustering lock *)
let clustering_lock_m = Mutex.create ()

let with_clustering_lock f =
  debug "Trying to grab host-local clustering lock...";
  Stdext.Threadext.Mutex.execute clustering_lock_m
    (fun () ->
       Stdext.Pervasiveext.finally
         (fun () ->
            debug "Grabbed host-local clustering lock; executing function...";
            f ())
         (fun () -> debug "Function execution finished; returned host-local clustering lock."))
