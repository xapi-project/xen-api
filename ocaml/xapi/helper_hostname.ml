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
open Stdext.Threadext
open Helper_process

let filter_newline s =
  let l = String.length s in
  let rec count_newlines i =
    if i=0 then 0
    else
      let chr = String.get s i in
      if chr='\n' || chr='\r' then count_newlines (i-1)
      else i in
  let newline_end = count_newlines (l-1) in
  String.sub s 0 (newline_end+1)

let _cached_hostname = ref ""
let _cached_hostname_m = Mutex.create ()
let get_hostname () =
  Mutex.execute _cached_hostname_m
    (fun () ->
       if !_cached_hostname = ""
       then
         _cached_hostname :=
           (try filter_newline (get_process_output "/bin/hostname")
            with _ -> "unknown");
       !_cached_hostname
    )

(* Fetch the hostname again in case it has changed beneath us *)
let reget_hostname () =
  Mutex.execute _cached_hostname_m (fun () -> _cached_hostname := "");
  get_hostname ()
