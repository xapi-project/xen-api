(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



include Register
(* [Register] is a side-effecting module. In particular, it registers the
   bisect_ppx PPX with the PPX driver. *)

let () = Migrate_parsetree.Driver.run_main ()
