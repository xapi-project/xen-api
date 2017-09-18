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
