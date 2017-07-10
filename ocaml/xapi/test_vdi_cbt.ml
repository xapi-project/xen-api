
let register_smapiv2_server (module S: Storage_interface.Server_impl with type context = unit) sr_ref =
  let module S = Storage_interface.Server(S) in
  let rpc = S.process () in
  let dummy_query_result = Storage_interface.({ driver=""; name=""; description=""; vendor=""; copyright=""; version=""; required_api_version=""; features=[]; configuration=[]; required_cluster_stack=[] }) in
  Storage_mux.register sr_ref rpc "" dummy_query_result

let make_smapiv2_storage_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_snapshot ?vdi_clone () =
  let default a b = match a with
    | Some a -> a
    | None -> b
  in
  (module struct
    include (Storage_skeleton: module type of Storage_skeleton with module VDI := Storage_skeleton.VDI)
    module VDI = struct
      include Storage_skeleton.VDI
      let enable_cbt = default vdi_enable_cbt Storage_skeleton.VDI.enable_cbt
      let disable_cbt = default vdi_disable_cbt Storage_skeleton.VDI.disable_cbt
      let snapshot = default vdi_snapshot Storage_skeleton.VDI.snapshot
      let clone = default vdi_snapshot Storage_skeleton.VDI.clone
    end
  end : Storage_interface.Server_impl with type context = unit)

let register_smapiv2_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_snapshot ?vdi_clone sr_ref =
  let s = make_smapiv2_storage_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_snapshot ?vdi_clone () in
  register_smapiv2_server s sr_ref

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
  register_smapiv2_server
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
  register_smapiv2_server ~vdi_snapshot:stub ~vdi_clone:stub (Db.SR.get_uuid ~__context ~self:sR);

  let snapshot = Xapi_vdi.snapshot ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:snapshot ~value:false ~msg:"The cbt_enabled property should be inherited from the snapshotted VDI - it should be false for the snapshot of a VDI with CBT disabled.";
  let clone = Xapi_vdi.clone ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:clone ~value:false ~msg:"CBT should always be disabled on the VDI created by VDI.clone";

  Db.VDI.set_cbt_enabled ~__context ~self:vdi ~value:true;
  let snapshot = Xapi_vdi.snapshot ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:snapshot ~value:true ~msg:"The cbt_enabled property should be inherited from the snapshotted VDI - it should be true for the snapshot of a VDI with CBT enabled.";
  let clone = Xapi_vdi.clone ~__context ~vdi ~driver_params:[] in
  assert_cbt_enabled_field_is ~vdi:clone ~value:false ~msg:"CBT should always be disabled on the VDI created by VDI.clone"

let test =
  let open OUnit in
  "test_vdi_cbt" >:::
  [ "test_cbt_enable_disable" >:: test_cbt_enable_disable
  ; "test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi" >:: test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi
  ; "test_clone_and_snapshot_correctly_sets_cbt_enabled_field" >:: test_clone_and_snapshot_correctly_sets_cbt_enabled_field
  ]
