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

(* A common requirement is to execute an idempotent operation in a background thread when 'something' changes but
   to minimise the number of times we run the operation i.e. if a large set of changes happen we'd ideally like to
   just execute the function once or twice but not once per thing that changed. *)

open Stdext
open Pervasiveext
open Threadext

module D = Debug.Make(struct let name="at_least_once_more" end)
open D

(** Type of the function executed in the background *)
type operation = unit -> unit

type manager = {
  f: operation;                    (** called in a background thread when 'something' changes *)
  name: string;                    (** human-readable name for logging *)
  m: Mutex.t;
  mutable needs_doing_again: bool; (** if true a further request arrived during an execution *)
  mutable in_progress: bool;       (** thread is currently servicing a request *)
}

let name_of_t (x: manager) = x.name

(** Make an instance of this kind of background operation *)
let make name f = {
  f = f;
  name = name;
  m = Mutex.create ();
  needs_doing_again = false;
  in_progress = false;
}

(** Signal that 'something' has changed and so the operation needs re-executed. *)
let again (x: manager) =
  Mutex.execute x.m
    (fun () ->
       if x.in_progress
       then x.needs_doing_again <- true (* existing thread will go around the loop again *)
       else begin
         (* no existing thread so we need to start one off *)
         x.in_progress <- true;
         x.needs_doing_again <- false;
         let (_: Thread.t) =
           Thread.create
             (fun () ->
                (* Always do the operation immediately: thread is only created when work needs doing *)
                x.f ();
                while Mutex.execute x.m
                    (fun () ->
                       if x.needs_doing_again
                       then (x.needs_doing_again <- false; true) (* another request came in while we were processing *)
                       else (x.in_progress <- false; false) (* no more requests: thread will shutdown *)
                    ) do
                  x. f()
                done) () in
         ()
       end)
