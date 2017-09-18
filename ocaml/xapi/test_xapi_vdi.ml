open OUnit

let vdi_create () ~dbg ~(sr:Storage_interface.sr) ~(vdi_info:Storage_interface.vdi_info) =
  vdi_info


let setup f =
  let __context = Test_common.make_test_database () in
  let sR = Test_common.make_sr ~__context () in
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sR in
  let host = Helpers.get_localhost ~__context in
  let _pbd = Test_common.make_pbd ~__context ~host ~sR ~currently_attached:true () in
  Test_storage_common.register_smapiv2_server
    ~vdi_create
    sr_uuid;
  f ~__context ~sR ~sr_uuid ~host

let test_vdi_create_ok () =
  setup (fun ~__context ~sR ~sr_uuid ~host ->
      let vdi_ref = Xapi_vdi.create
          ~__context
          ~name_label:"test"
          ~name_description:"description"
          ~sR
          ~virtual_size:1000L
          ~_type:`crashdump
          ~sharable:false
          ~read_only:false
          ~other_config:[]
          ~xenstore_data:[]
          ~sm_config:[]
          ~tags:[]
      in
      assert_equal (Db.VDI.get_name_label ~__context ~self:vdi_ref) "test")

let test_vdi_create_bad_type_fail () =
  setup (fun ~__context ~sR ~sr_uuid ~host ->
    Test_common.assert_raises_api_error Api_errors.vdi_incompatible_type (fun () ->
          Xapi_vdi.create
            ~__context
            ~name_label:"test"
            ~name_description:"description"
            ~sR
            ~virtual_size:1000L
            ~_type:`cbt_metadata
            ~sharable:false
            ~read_only:false
            ~other_config:[]
            ~xenstore_data:[]
            ~sm_config:[]
            ~tags:[]))

let test_vdi_create_bad_type2_fail () =
  setup (fun ~__context ~sR ~sr_uuid ~host ->
      Test_common.assert_raises_api_error Api_errors.field_type_error (fun () ->
          Xapi_vdi.create
            ~__context
            ~name_label:"test"
            ~name_description:"description"
            ~sR
            ~virtual_size:1000L
            ~_type:`unknown
            ~sharable:false
            ~read_only:false
            ~other_config:[]
            ~xenstore_data:[]
            ~sm_config:[]
            ~tags:[]))

let test =
  "test_xapi_vdi" >::: [
    "test_vdi_create_ok" >:: test_vdi_create_ok;
    "test_vdi_create_bad_type_fail" >:: test_vdi_create_bad_type_fail;
    "test_vdi_create_bad_type2_fail" >:: test_vdi_create_bad_type2_fail;
  ]