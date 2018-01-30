#!/usr/bin/env ocamlscript
Ocaml.sources := ["common.ml"; "lvm.ml"];
Ocaml.packs := ["xapi-storage"; "cmdliner"; "re.str"; "oUnit"; "uri"];
Ocaml.ocamlflags := ["-thread"]
--
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
open Common

module Command = struct
  open Xapi_storage.V.Types
  include SR.Ls

  let command common { SR.Ls.In.dbg; sr } =
    List.map (Lvm.volume_of_lv sr) (Lvm.lvs sr)
end

module Test = struct
  open OUnit

  let test_lvs () =
    let vol = Lvm.make_temp_volume () in
    let vg_name = "hello" in
    finally
      (fun () ->
        Lvm.vgcreate vg_name [ vol ];
        Lvm.lvcreate vg_name "testvol" 1L;
        finally
          (fun () ->
            match Lvm.lvs vg_name with
            | [ { Lvm.name = "testvol" } ] -> ()
            | [ ] -> failwith "I created 'testvol' but it didnt show in 'lvs'"
            | _ -> failwith "I created 'testvol' but multiple volumes showed up in 'lvs'"
          ) (fun () ->
            Lvm.lvremove vg_name "testvol"
          )
      ) (fun () ->
        Lvm.remove_temp_volume vol
      )

  let test common =
    let suite = "create" >::: [
      "lvs" >:: test_lvs;
    ] in
    ignore(run_test_tt ~verbose:common.Common.verbose suite)

end

module M = Make(Command)(Test)
let _ = M.main ()
