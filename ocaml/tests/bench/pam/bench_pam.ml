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
  (* FIXME: this adds a 5s pause on startup, any way to initialize the code but not incurr the fail delay? *)
  (* To avoid the sleep(1) in the libgcrypt/NSS initialization code that gets called from Pam.authorize_run create and run a fake auth command,
     and keep the handle open
 *)
  Pam.workaround ();
  let h = Pam.authorize_start ()  in
  h

let sleepfix_stop = Pam.authorize_stop

let pam_run' (_, h) = pam_run h

let sleepfix_start' () =
  sleepfix_start (), Pam.authorize_start ()

let sleepfix_stop' (h, h') =
  sleepfix_stop h;
  Pam.authorize_stop h'

let pam_authenticate' _ = Pam.authenticate username password

let measures = Toolkit.Instance.[Ezbechamel_concurrent.operations; monotonic_clock; minor_allocated ]

let () =
  Ezbechamel_alcotest_notty.run ~measures
    [ Test.make ~name:"PAM start+stop" (Staged.stage pam_start_stop)
    ; Test.make ~name:"PAM start+run+stop" (Staged.stage pam_start_run_stop)
    ; Test.make ~name:"PAM authenticate (current)" (Staged.stage pam_authenticate)
    ; Test.make_with_resource ~allocate:Pam.authorize_start ~free:Pam.authorize_stop ~name:"PAM run (uniq)" Test.uniq (Staged.stage pam_run)  
    ; Test.make_with_resource ~allocate:Pam.authorize_start ~free:Pam.authorize_stop ~name:"PAM run (multiple)" Test.multiple (Staged.stage pam_run)  
    ; test_concurrently ~allocate:Pam.authorize_start ~free:Pam.authorize_stop ~name:"concurrent authorize (reuse)" (Staged.stage pam_run)
    ; test_concurrently ~allocate:ignore ~free:ignore ~name:"concurrent authenticate (no reuse)" (Staged.stage pam_authenticate)
    ; test_concurrently ~allocate:sleepfix_start' ~free:sleepfix_stop' ~name:"concurrent authenticate (reuse)" (Staged.stage pam_run')
    ; test_concurrently ~allocate:sleepfix_start ~free:sleepfix_stop ~name:"concurrent authenticate (sleep fix, actual)" (Staged.stage pam_authenticate')
   ]
