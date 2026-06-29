(*
 * Copyright (C) 2026 Vates
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

module T = Test_common

let test_trunks_parameter () =
  let __context = T.make_test_database () in
  let vM = T.make_vm ~__context () in
  let network = T.make_network ~__context () in
  let vif = T.make_vif ~__context ~device:"1" ~vM ~network ~trunks:[2201L] () in
  Alcotest.(check (list int64))
    "test_trunks_parameter testing add_trunks" [3201L; 2201L]
    ( Xapi_vif.add_trunks ~__context ~self:vif ~value:3201L ;
      Db.VIF.get_trunks ~__context ~self:vif
    ) ;
  Alcotest.(check (list int64))
    "test_trunks_parameter testing remove_trunks" [2201L]
    ( Xapi_vif.remove_trunks ~__context ~self:vif ~value:3201L ;
      Db.VIF.get_trunks ~__context ~self:vif
    ) ;
  Alcotest.(check (list int64))
    "test_trunks_parameter testing set_trunks" [3201L]
    ( Xapi_vif.set_trunks ~__context ~self:vif ~value:[3201L] ;
      Db.VIF.get_trunks ~__context ~self:vif
    ) ;
  Alcotest.(check (list int64))
    "test_trunks_parameter testing set_trunks empty" []
    ( Xapi_vif.set_trunks ~__context ~self:vif ~value:[] ;
      Db.VIF.get_trunks ~__context ~self:vif
    ) ;
  Alcotest.(check_raises)
    "test_trunks_parameter testing invalid VLAN tag"
    Api_errors.(Server_error (Api_errors.vlan_tag_invalid, ["9999"]))
    (fun () -> Xapi_vif.add_trunks ~__context ~self:vif ~value:9999L)

(** try to set trunks on VIF on incompatible network *)
let test_trunks_coherence_vif_set () =
  let __context = T.make_test_database () in
  (* create a VLAN *)
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let tagged_PIF = T.make_pif ~__context ~network ~host () in
  let tag = 3201L in
  let vlan_network = T.make_network ~__context ~bridge:"xapi0" () in
  let untagged_PIF =
    T.make_pif ~__context ~network:vlan_network ~host ~vLAN:tag ()
  in
  let _vlan = T.make_vlan ~__context ~tagged_PIF ~untagged_PIF ~tag () in
  (* create VM + VIF using this network *)
  let vM = T.make_vm ~__context () in
  let vif = T.make_vif ~__context ~device:"1" ~vM ~network:vlan_network () in
  Alcotest.(check_raises)
    "test_trunks_coherence_vif_set testing"
    Api_errors.(
      Server_error
        ( Api_errors.network_incompatible_with_trunks
        , [Ref.string_of vlan_network]
        )
    )
    (fun () -> Xapi_vif.add_trunks ~__context ~self:vif ~value:2201L)

(** try to add VIF (with trunks) on incompatible network *)
let test_trunks_coherence_vif_add () =
  let __context = T.make_test_database () in
  (* create a VLAN *)
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let tagged_PIF = T.make_pif ~__context ~network ~host () in
  let tag = 3201L in
  let vlan_network = T.make_network ~__context ~bridge:"xapi0" () in
  let untagged_PIF =
    T.make_pif ~__context ~network:vlan_network ~host ~vLAN:tag ()
  in
  let _vlan = T.make_vlan ~__context ~tagged_PIF ~untagged_PIF ~tag () in
  (* create VM + VIF using this network *)
  let vM = T.make_vm ~__context () in
  Alcotest.(check_raises)
    "test_trunks_coherence_vif_add testing"
    Api_errors.(
      Server_error
        ( Api_errors.network_incompatible_with_trunks
        , [Ref.string_of vlan_network]
        )
    )
    (fun () ->
      let _ : API.ref_VIF =
        Xapi_vif.create ~__context ~device:"1" ~network:vlan_network ~vM
          ~mAC:"00:00:00:00:00:00" ~mTU:1500L ~other_config:[]
          ~currently_attached:true ~qos_algorithm_type:""
          ~qos_algorithm_params:[] ~locking_mode:`unlocked ~ipv4_allowed:[]
          ~ipv6_allowed:[] ~trunks:[2201L]
      in
      ()
    )

(** try to associate PIF (with VLAN) on Network with trunked-VIF *)
let test_trunks_coherence_pif_vlan () =
  let __context = T.make_test_database () in
  (* create VM + VIF on plain network *)
  let network = T.make_network ~__context () in
  let vM = T.make_vm ~__context () in
  let _vif =
    T.make_vif ~__context ~device:"1" ~vM ~network ~trunks:[3201L] ()
  in
  Alcotest.(check_raises)
    "test_trunks_coherence_pif_vlan testing"
    Api_errors.(
      Server_error
        (Api_errors.network_incompatible_with_trunks, [Ref.string_of network])
    )
    (fun () ->
      (* create a VLAN *)
      let host = T.make_host ~__context () in
      let network2 = T.make_network ~__context () in
      let tagged_PIF = T.make_pif ~__context ~network:network2 ~host () in
      let tag = 2201L in
      let untagged_PIF = T.make_pif ~__context ~network ~host ~vLAN:tag () in
      let _vlan = T.make_vlan ~__context ~tagged_PIF ~untagged_PIF ~tag () in
      ()
    )

let test_trunks_move () =
  let __context = T.make_test_database () in
  (* create VM + VIF using this network *)
  let network1 = T.make_network ~__context () in
  let vM = T.make_vm ~__context () in
  let vif = T.make_vif ~__context ~network:network1 ~vM () in
  let network2 = T.make_network ~__context () in
  Alcotest.(check unit)
    "test_trunks_move testing" ()
    (Xapi_vif.move ~__context ~self:vif ~network:network2)

let test_trunks_move_to_vlan () =
  let __context = T.make_test_database () in
  (* create VM + VIF (with trunks) *)
  let network1 = T.make_network ~__context () in
  let vM = T.make_vm ~__context () in
  let vif = T.make_vif ~__context ~network:network1 ~vM ~trunks:[3201L] () in
  (* create a VLAN *)
  let host = T.make_host ~__context () in
  let network2 = T.make_network ~__context () in
  let tagged_PIF = T.make_pif ~__context ~network:network2 ~host () in
  let tag = 3201L in
  let vlan_network2 = T.make_network ~__context ~bridge:"xapi0" () in
  let untagged_PIF =
    T.make_pif ~__context ~network:vlan_network2 ~host ~vLAN:tag ()
  in
  let _vlan = T.make_vlan ~__context ~tagged_PIF ~untagged_PIF ~tag () in
  (* move the VIF to the vlan_network *)
  Alcotest.(check_raises)
    "test_trunks_move_to_vlan testing"
    Api_errors.(
      Server_error
        ( Api_errors.network_incompatible_with_trunks
        , [Ref.string_of vlan_network2]
        )
    )
    (fun () -> Xapi_vif.move ~__context ~self:vif ~network:vlan_network2)

let test =
  [
    ("test_trunks_parameter", `Quick, test_trunks_parameter)
  ; ("test_trunks_coherence_vif_set", `Quick, test_trunks_coherence_vif_set)
  ; ("test_trunks_coherence_vif_add", `Quick, test_trunks_coherence_vif_add)
  ; ("test_trunks_coherence_pif_vlan", `Quick, test_trunks_coherence_pif_vlan)
  ; ("test_trunks_move", `Quick, test_trunks_move)
  ; ("test_trunks_move_to_vlan", `Quick, test_trunks_move_to_vlan)
  ]
