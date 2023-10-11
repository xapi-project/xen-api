(*
  Copyright (C) Cloud Software Group
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation; version 2.1 only. with the special
  exception on linking described in file LICENSE.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
*)

open Bechamel

let args = [1; 2; 4; 8; 16]


open Ezbechamel_concurrent
let derived_measures =
  [ Toolkit.Instance.monotonic_clock
  , operations
  , "monotonic clock/op"
  , "ms/op"
  , (fun ns ops ->
    let ops = if ops = 0. then 1. else ops in
    ns *. 1e-6 /. ops
    )
  ]


let pam_start_stop () =
  let h = Pam.authorize_start () in
  Pam.authorize_stop h

(* TODO: forward cmdliner args  *)
let username = "pamtest-edvint"
let password = "pamtest-edvint"

let pam_start_run_stop () =
  let h = Pam.authorize_start () in
  Pam.authorize_run h username password;
  Pam.authorize_stop h

let pam_authenticate () =
  Pam.authenticate username password

let pam_run h =
  Pam.authorize_run h username password

let sleepfix_start () =
  Pam.workaround ()

let sleepfix_stop () = ()

let sleepfix_start' () =
  sleepfix_start ();
  Pam.authorize_start ()

let sleepfix_stop' h =
  Pam.authorize_stop h

let pam_authenticate' _ = Pam.authenticate username password

let measures = Toolkit.Instance.[Ezbechamel_concurrent.operations; monotonic_clock; minor_allocated ]

let max_auth_threads = 8

let threadpool_start () =
  Threadpool.create ~name:"pam"
    Pam.authorize_start
    Pam.authorize_stop
    max_auth_threads

let threadpool_sleepfix_start () =
  sleepfix_start ();
  threadpool_start ()

let threadpool_stop = Threadpool.shutdown

let threadpool_run t =
  Threadpool.run_in_pool t ignore

let threadpool_sleepfix_free = Threadpool.shutdown

let threadpool_sleepfix_run t =
  Threadpool.run_in_pool t pam_run

let sleepfix_semaphore_allocate () =
  sleepfix_start ();
  Semaphore.Counting.make max_auth_threads

let sleepfix_semaphore_free = ignore

let sleepfix_semaphore_run sem =
  Semaphore.Counting.acquire sem;
  let finally () =  Semaphore.Counting.release sem in
  Fun.protect ~finally pam_authenticate


let () =
  Ezbechamel_alcotest_notty.run ~measures ~derived_measures
    [ Test.make ~name:"PAM start+stop" (Staged.stage pam_start_stop)
    ; Test.make ~name:"PAM start+run+stop" (Staged.stage pam_start_run_stop)
    ; Test.make ~name:"PAM authenticate (current)" (Staged.stage pam_authenticate)
    ; test_concurrently ~allocate:threadpool_start ~free:threadpool_stop ~name:"Threadpool dispatch overhead" (Staged.stage threadpool_run)
    ; Test.make_with_resource ~allocate:Pam.authorize_start ~free:Pam.authorize_stop ~name:"PAM run (uniq)" Test.uniq (Staged.stage pam_run)  
    ; Test.make_with_resource ~allocate:Pam.authorize_start ~free:Pam.authorize_stop ~name:"PAM run (multiple)" Test.multiple (Staged.stage pam_run)  
    ; test_concurrently ~allocate:Pam.authorize_start ~free:Pam.authorize_stop ~name:"concurrent authorize (reuse)" (Staged.stage pam_run)
    ; test_concurrently ~allocate:ignore ~free:ignore ~name:"concurrent authenticate (no reuse)" (Staged.stage pam_authenticate)
    ; test_concurrently ~allocate:sleepfix_start' ~free:sleepfix_stop' ~name:"concurrent authenticate (reuse)" (Staged.stage pam_run)
    ; test_concurrently ~allocate:sleepfix_start ~free:sleepfix_stop ~name:"concurrent authenticate (sleep fix, actual)" (Staged.stage pam_authenticate')
    ; test_concurrently ~allocate:threadpool_sleepfix_start ~free:threadpool_sleepfix_free ~name:"concurrent threadpool + sleep fix"
        (Staged.stage threadpool_sleepfix_run)
    ; test_concurrently ~allocate:sleepfix_semaphore_allocate ~free:sleepfix_semaphore_free ~name:"concurrent semaphore + sleep fix"
        (Staged.stage sleepfix_semaphore_run)

   ]
