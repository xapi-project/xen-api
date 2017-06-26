
let register_smapiv2_server (module S: Storage_interface.Server_impl with type context = unit) sr_ref =
  let module S = Storage_interface.Server(S) in
  let rpc = S.process () in
  let dummy_query_result = Storage_interface.({ driver=""; name=""; description=""; vendor=""; copyright=""; version=""; required_api_version=""; features=[]; configuration=[]; required_cluster_stack=[] }) in
  Storage_mux.register sr_ref rpc "" dummy_query_result

let override_cbt_functions ?vdi_enable_cbt ?vdi_disable_cbt () =
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
    end
  end : Storage_interface.Server_impl with type context = unit)

let register_cbt_server ?vdi_enable_cbt ?vdi_disable_cbt sr_ref =
  let s = override_cbt_functions ?vdi_enable_cbt ?vdi_disable_cbt () in
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
  register_cbt_server
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

let test =
  let open OUnit in
  "test_vdi_cbt" >:::
  [ "test_cbt_enable_disable" >:: test_cbt_enable_disable ]
