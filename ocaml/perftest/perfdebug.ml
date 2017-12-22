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
let stdout_m = Mutex.create ()

let debug ?(out=stdout) (fmt: ('a , unit, string, unit) format4) =
  Xapi_stdext_threads.Threadext.Mutex.execute stdout_m
    (fun () ->
       Printf.kprintf (fun s -> Printf.fprintf out "%s\n" s; flush stdout) fmt
    )
