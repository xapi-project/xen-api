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

(* Tests whether the xenstore watches get cancelled successfully after the
   timeout *)

open Xenops_interface
open Cancel_utils
open Xenops_task

exception Did_not_cancel

let tasks = Xenops_task.empty ()

let xenstore_test xs =
  let task = Xenops_task.add tasks "test" (fun _ -> None) in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        Thread.delay 1. ;
        Xenops_task.with_cancel task (fun () -> ()) (fun () -> ())
      )
      ()
  in
  try
    let (_ : bool) =
      cancellable_watch (TestPath "/test/cancel") [] [] task ~xs ~timeout:3. ()
    in
    Printf.printf "%s: failure: watch was not cancelled!" __MODULE__ ;
    raise Did_not_cancel
  with Xenopsd_error (Cancelled _) ->
    Printf.printf "%s: success: watch cancelled successfully" __MODULE__

let () =
  try Xenstore.with_xs xenstore_test
  with Xs_transport.Could_not_find_xenstore ->
    Printf.printf
      "%s: Xenstore not found, cannot test cancellable watches, are you \
       running on dom0?"
      __MODULE__
