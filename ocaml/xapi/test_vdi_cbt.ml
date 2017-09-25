

let test_cbt_enable_disable () =
  let __context = Test_common.make_test_database () in
  let sr_ref = Test_common.make_sr ~__context () in
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr_ref in
  let vdi_location = "test123" in
  let vdi_ref = Test_common.make_vdi ~__context ~sR:sr_ref ~location:vdi_location () in
  let assert_vdi_cbt_enabled_is value msg =
    OUnit.assert_equal ~msg value (Db.VDI.get_cbt_enabled ~__context ~self:vdi_ref) in

  let enable_cbt_params = ref None in
  let disable_cbt_params = ref None in
  Test_storage_common.register_smapiv2_server
    ~vdi_enable_cbt:(fun _ ~dbg ~sr ~vdi -> enable_cbt_params := Some (sr, vdi))
    ~vdi_disable_cbt:(fun _ ~dbg ~sr ~vdi -> disable_cbt_params := Some (sr, vdi))
    sr_uuid;

  Xapi_vdi.enable_cbt ~__context ~self:vdi_ref;
  OUnit.assert_equal ~msg:"The parameters should be correctly passed to SMAPIv2 from VDI.enable_cbt" (Some (sr_uuid, vdi_location)) !enable_cbt_params;
  assert_vdi_cbt_enabled_is true "cbt_enabled should be true when VDI.enable_cbt returns successfully";

  Xapi_vdi.enable_cbt ~__context ~self:vdi_ref;
  assert_vdi_cbt_enabled_is true "VDI.enable_cbt should be idempotent";

  Xapi_vdi.disable_cbt ~__context ~self:vdi_ref;
  OUnit.assert_equal ~msg:"The parameters should be correctly passed to SMAPIv2 from VDI.disable_cbt" (Some (sr_uuid, vdi_location)) !disable_cbt_params;
  assert_vdi_cbt_enabled_is false "cbt_enabled should be false when VDI.disable_cbt returns successfully";

  Xapi_vdi.disable_cbt ~__context ~self:vdi_ref;
  assert_vdi_cbt_enabled_is false "VDI.disable_cbt should be idempotent"


let test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi () =
  let __context = Test_common.make_test_database () in
  let self = Test_common.make_vdi ~__context ~_type:`cbt_metadata () in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  OUnit.assert_raises
    ~msg:"VDI.set_metadata_of_pool should throw VDI_INCOMPATIBLE_TYPE for a cbt_metadata VDI"
    Api_errors.(Server_error (vdi_incompatible_type, [Ref.string_of self; Record_util.vdi_type_to_string `cbt_metadata]))
    (fun () -> Xapi_vdi.set_metadata_of_pool ~__context ~self ~value:pool)

let test_clone_and_snapshot_correctly_sets_cbt_enabled_field () =
  let __context = Test_common.make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let assert_cbt_enabled_field_is ~vdi ~value ~msg =
    OUnit.assert_equal ~msg value (Db.VDI.get_cbt_enabled ~__context ~self:vdi) in

  let sR = Test_common.make_sr ~__context () in
  let _: _ API.Ref.t = Test_common.make_pbd ~__context ~host ~sR () in
  let vdi =
    let uuid = Test_common.make_uuid () in
    Test_common.make_vdi ~__context ~sR ~uuid ~managed:true ()
  in
  let stub _ ~dbg ~sr ~vdi_info =
    let uuid = Test_common.make_uuid () in
    Storage_interface.{ default_vdi_info with vdi = uuid }
  in
  Test_storage_common.register_smapiv2_server ~vdi_snapshot:stub ~vdi_clone:stub (Db.SR.get_uuid ~__context ~self:sR);

  let snapshot = Xapi_vdi.snapshot ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:snapshot ~value:false ~msg:"The cbt_enabled property should be inherited from the snapshotted VDI - it should be false for the snapshot of a VDI with CBT disabled.";
  let clone = Xapi_vdi.clone ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:clone ~value:false ~msg:"CBT should always be disabled on the VDI created by VDI.clone";

  Db.VDI.set_cbt_enabled ~__context ~self:vdi ~value:true;
  let snapshot = Xapi_vdi.snapshot ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:snapshot ~value:true ~msg:"The cbt_enabled property should be inherited from the snapshotted VDI - it should be true for the snapshot of a VDI with CBT enabled.";
  let clone = Xapi_vdi.clone ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:clone ~value:false ~msg:"CBT should always be disabled on the VDI created by VDI.clone"

let test_get_nbd_info =
  let make_host __context sR ?(pbd_attached=true) pifs () =
    let host = Test_common.make_host ~__context () in
    let _: _ API.Ref.t = Test_common.make_pbd ~__context ~host ~sR ~currently_attached:pbd_attached () in
    List.iter (function (network, iP, iPv6, attached) ->
        let self = Test_common.make_pif ~__context ~network ~host ~iP ~iPv6 () in
        Db.PIF.set_currently_attached ~__context ~self ~value:attached
      ) pifs;
    host
  in

  let setup_test () =
    let __context = Mock.make_context_with_new_db "Mock context" in
    let session_id = Test_common.make_session ~__context () in
    let __context = Context.make ~__context ~session_id "test" in
    let make_host = make_host __context in

    let sr_of_vdi = Test_common.make_sr ~__context () in
    let network = Test_common.make_network ~__context () in
    let vdi = Test_common.make_vdi ~__context ~sR:sr_of_vdi () in

    (__context, make_host, sr_of_vdi, network, vdi)
  in

  let test_returns_correct_uris () =
    let (__context, make_host, sr_of_vdi, network_1, self) = setup_test () in
    let uuid = Db.VDI.get_uuid ~__context ~self in
    let other_sr = Test_common.make_sr ~__context () in
    let network_2 = Test_common.make_network ~__context () in

    (* Hosts connected to both the VDI's SR and a network *)
    let _: _ API.Ref.t = make_host
        sr_of_vdi
        [(network_1, "92.40.98.91", [], true); (network_1, "92.40.98.92", [], true);
         (* this PIF is not currently attached: *)
         (network_2, "92.40.98.93", ["10e1:bdb8:05a3:0002:03ae:8a24:0371:0001"], false)] () in
    let _: _ API.Ref.t = make_host
        sr_of_vdi
        [(network_2, "92.40.98.94", ["10e1:bdb8:05a3:0002:03ae:8a24:0371:0002";"10e1:bdb8:05a3:0002:03ae:8a24:0371:0003"], true)] () in
    (* Hosts not connected to a network or without an IP address *)
    let _: _ API.Ref.t = make_host sr_of_vdi [(network_1, "92.40.98.95", ["10e1:bdb8:05a3:0002:03ae:8a24:0371:0004"], false)] () in
    let _: _ API.Ref.t = make_host sr_of_vdi [(network_1, "", [], true)] () in
    (* Hosts not connected to the VDI's SR *)
    let _: _ API.Ref.t = make_host sr_of_vdi ~pbd_attached:false [(network_1, "92.40.98.96", [], true)] () in
    let _: _ API.Ref.t = make_host other_sr [(network_1, "92.40.98.97", ["10e1:bdb8:05a3:0002:03ae:8a24:0371:0005"], true)] () in
    (* Hosts not connected to either *)
    let _: _ API.Ref.t = make_host sr_of_vdi [] () in

    let nbd_info = Xapi_vdi.get_nbd_info ~__context ~self in

    let session_id = Context.get_session_id __context |> Ref.string_of in
    let expected =
      [ "nbd://92.40.98.91:10809/" ^ uuid ^ "?session_id=" ^ session_id
      ; "nbd://92.40.98.92:10809/" ^ uuid ^ "?session_id=" ^ session_id
      ; "nbd://92.40.98.94:10809/" ^ uuid ^ "?session_id=" ^ session_id
      ; "nbd://[10e1:bdb8:05a3:0002:03ae:8a24:0371:0002]:10809/" ^ uuid ^ "?session_id=" ^ session_id
      ; "nbd://[10e1:bdb8:05a3:0002:03ae:8a24:0371:0003]:10809/" ^ uuid ^ "?session_id=" ^ session_id
      ]
    in
    Ounit_comparators.StringSet.(assert_equal (of_list expected) (of_list nbd_info))
  in

  let test_returns_empty_list_when_no_host_is_connected_to_sr () =
    let (__context, make_host, sr_of_vdi, network, self) = setup_test () in
    let _: _ API.Ref.t = make_host sr_of_vdi ~pbd_attached:false [(network, "92.40.98.93", [], true)] () in
    OUnit.assert_equal [] (Xapi_vdi.get_nbd_info ~__context ~self);
    Db.PBD.get_all ~__context |> List.iter (fun self -> Db.PBD.destroy ~__context ~self);
    OUnit.assert_equal [] (Xapi_vdi.get_nbd_info ~__context ~self)
  in

  let test_returns_empty_list_when_no_host_is_connected_to_network () =
    let (__context, make_host, sr_of_vdi, network, self) = setup_test () in
    let _: _ API.Ref.t = make_host sr_of_vdi [(network, "92.40.98.93", [], false)] () in
    OUnit.assert_equal [] (Xapi_vdi.get_nbd_info ~__context ~self);
    Db.PIF.get_all ~__context |> List.iter (fun self -> Db.PIF.destroy ~__context ~self);
    OUnit.assert_equal [] (Xapi_vdi.get_nbd_info ~__context ~self)
  in

  let test_disallowed_for_cbt_metadata_vdi () =
    let (__context, make_host, sr_of_vdi, network, self) = setup_test () in
    let _: _ API.Ref.t = make_host sr_of_vdi [(network, "92.40.98.93", [], true)] () in
    Db.VDI.set_type ~__context ~self ~value:`cbt_metadata;
    let _type = Record_util.vdi_type_to_string `cbt_metadata in
    OUnit.assert_raises
      Api_errors.(Server_error (vdi_incompatible_type, [Ref.string_of self; _type]))
      (fun () -> Xapi_vdi.get_nbd_info ~__context ~self)
  in

  let open OUnit in
  "test_get_nbd_info" >:::
  [ "test_returns_correct_uris" >:: test_returns_correct_uris
  ; "test_returns_empty_list_when_no_host_is_connected_to_sr" >:: test_returns_empty_list_when_no_host_is_connected_to_sr
  ; "test_returns_empty_list_when_no_host_is_connected_to_network" >:: test_returns_empty_list_when_no_host_is_connected_to_network
  ; "test_disallowed_for_cbt_metadata_vdi" >:: test_disallowed_for_cbt_metadata_vdi
  ]

let test =
  let open OUnit in
  "test_vdi_cbt" >:::
  [ "test_cbt_enable_disable" >:: test_cbt_enable_disable
  ; "test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi" >:: test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi
  ; "test_clone_and_snapshot_correctly_sets_cbt_enabled_field" >:: test_clone_and_snapshot_correctly_sets_cbt_enabled_field
  ; test_get_nbd_info
  ]
