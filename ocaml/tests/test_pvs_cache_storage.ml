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

open Test_common
open Test_highlevel

let string_of_unit_result =
  Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))

module Assert_not_already_present = Generic.MakeStateful (struct
  module Io = struct
    type input_t = string * string

    type output_t = (unit, exn) result

    let string_of_input_t = Test_printers.(pair string string)

    let string_of_output_t = string_of_unit_result
  end

  module State = Test_state.XapiDb

  let host1 = Ref.make ()

  let site1 = Ref.make ()

  (*
		Scenario:
		- Two sites and hosts.
		- Only site1 has cache storage setup, and only on host1.
	*)
  let load_input __context _ =
    let _ = make_host2 ~__context ~ref:host1 ~name_label:"host1" () in
    let _ = make_host2 ~__context ~name_label:"host2" () in
    let _ = make_pvs_site ~__context ~ref:site1 ~name_label:"site1" () in
    let _ = make_pvs_site ~__context ~name_label:"site2" () in
    let _ = make_pvs_cache_storage ~__context ~site:site1 ~host:host1 () in
    ()

  let extract_output __context (site, host) =
    let site' =
      List.hd (Db.PVS_site.get_by_name_label ~__context ~label:site)
    in
    let host' = List.hd (Db.Host.get_by_name_label ~__context ~label:host) in
    try
      Ok
        (Xapi_pvs_cache_storage.assert_not_already_present ~__context site'
           host'
        )
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( ("site1", "host1")
        , Error
            Api_errors.(
              Server_error
                ( pvs_cache_storage_already_present
                , [Ref.string_of site1; Ref.string_of host1]
                )
            )
        )
      ; (("site1", "host2"), Ok ())
      ; (("site2", "host1"), Ok ())
      ; (("site2", "host2"), Ok ())
      ]
end)

module Assert_not_in_use = Generic.MakeStateful (struct
  module Io = struct
    type input_t = string * string

    type output_t = (unit, exn) result

    let string_of_input_t = Test_printers.(pair string string)

    let string_of_output_t = string_of_unit_result
  end

  module State = Test_state.XapiDb

  let pcs1 = Ref.make ()

  (*
		Scenario:
		- Two sites with storage on two hosts.
		- A running VM on host 1, with
			- an attached proxy on site1 and
			- an unattached proxy on site2.
	*)
  let load_input __context _ =
    let host1 = make_host2 ~__context ~name_label:"host1" () in
    let host2 = make_host2 ~__context ~name_label:"host2" () in
    let site1 = make_pvs_site ~__context ~name_label:"site1" () in
    let site2 = make_pvs_site ~__context ~name_label:"site2" () in
    let vM = make_vm ~__context () in
    Db.VM.set_resident_on ~__context ~self:vM ~value:host1 ;
    let vif1 = make_vif ~__context ~vM () in
    let vif2 = make_vif ~__context ~vM () in
    let _ =
      make_pvs_cache_storage ~__context ~ref:pcs1 ~site:site1 ~host:host1 ()
    in
    let _ = make_pvs_cache_storage ~__context ~site:site1 ~host:host2 () in
    let _ = make_pvs_cache_storage ~__context ~site:site2 ~host:host1 () in
    let _ = make_pvs_cache_storage ~__context ~site:site2 ~host:host2 () in
    let _ =
      make_pvs_proxy ~__context ~site:site1 ~vIF:vif1 ~currently_attached:true
        ()
    in
    let _ =
      make_pvs_proxy ~__context ~site:site2 ~vIF:vif2 ~currently_attached:false
        ()
    in
    ()

  let extract_output __context (site, host) =
    let site' =
      List.hd (Db.PVS_site.get_by_name_label ~__context ~label:site)
    in
    let host' = List.hd (Db.Host.get_by_name_label ~__context ~label:host) in
    let pcs =
      Db.PVS_site.get_cache_storage ~__context ~self:site'
      |> List.filter (fun pcs ->
             Db.PVS_cache_storage.get_host ~__context ~self:pcs = host'
         )
      |> List.hd
    in
    try Ok (Xapi_pvs_cache_storage.assert_not_in_use ~__context pcs)
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( ("site1", "host1")
        , Error
            Api_errors.(
              Server_error (pvs_cache_storage_is_in_use, [Ref.string_of pcs1])
            )
        )
      ; (("site1", "host2"), Ok ())
      ; (("site2", "host1"), Ok ())
      ; (("site2", "host2"), Ok ())
      ]
end)

let tests =
  make_suite "pvs_site_"
    [
      ("is_already_present", Assert_not_already_present.tests)
    ; ("assert_not_in_use", Assert_not_in_use.tests)
    ]
