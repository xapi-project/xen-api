(*
-  Copyright (C) 2017 Citrix Systems Inc.
-
-  This program is free software; you can redistribute it and/or modify
-  it under the terms of the GNU Lesser General Public License as published
-  by the Free Software Foundation; version 2.1 only. with the special
-  exception on linking described in file LICENSE.
-
-  This program is distributed in the hope that it will be useful,
-  but WITHOUT ANY WARRANTY; without even the implied warranty of
-  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-  GNU Lesser General Public License for more details.
  *)


let register_smapiv2_server (module S: Storage_interface.Server_impl with type context = unit) sr_ref =
  let module S = Storage_interface.Server(S) in
  let rpc = S.process () in
  let dummy_query_result = Storage_interface.({ driver=""; name=""; description=""; vendor=""; copyright=""; version=""; required_api_version=""; features=[]; configuration=[]; required_cluster_stack=[] }) in
  Storage_mux.register sr_ref rpc "" dummy_query_result

let make_smapiv2_storage_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_list_changed_blocks ?vdi_data_destroy ?vdi_snapshot ?vdi_clone () =
  let default = Xapi_stdext_monadic.Opt.default in
  (module struct
    include (Storage_skeleton: module type of Storage_skeleton with module VDI := Storage_skeleton.VDI)
    module VDI = struct
      include Storage_skeleton.VDI
      let enable_cbt = default Storage_skeleton.VDI.enable_cbt vdi_enable_cbt
      let disable_cbt = default Storage_skeleton.VDI.disable_cbt vdi_disable_cbt
      let list_changed_blocks  = default Storage_skeleton.VDI.list_changed_blocks vdi_list_changed_blocks
      let data_destroy = default Storage_skeleton.VDI.data_destroy vdi_data_destroy
      let snapshot = default Storage_skeleton.VDI.snapshot vdi_snapshot
      let clone = default Storage_skeleton.VDI.clone vdi_snapshot
    end
  end : Storage_interface.Server_impl with type context = unit)

let register_smapiv2_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_list_changed_blocks ?vdi_data_destroy ?vdi_snapshot ?vdi_clone sr_ref =
  let s = make_smapiv2_storage_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_list_changed_blocks ?vdi_data_destroy ?vdi_snapshot ?vdi_clone () in
  register_smapiv2_server s sr_ref

(** Create host -> (SM) -> PBD -> SR infrastructure for mock storage layer, return SR *)
let make_mock_server_infrastructure ~__context =
  let host = Helpers.get_localhost ~__context in
  (* This SM instance has CBT capabilities by default *)
  let _: _ API.Ref.t = Test_common.make_sm ~__context () in
  let sR = Test_common.make_sr ~__context ~is_tools_sr:false () in
  let _: _ API.Ref.t = Test_common.make_pbd ~__context ~host ~sR ~currently_attached:true () in
  sR

let test_cbt_enable_disable () =
  let __context = Test_common.make_test_database () in
  let sr_ref = Test_common.make_sr ~__context () in
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr_ref in
  let vdi_location = "test123" in
  let vdi_ref = Test_common.make_vdi ~__context ~sR:sr_ref ~location:vdi_location () in
  let assert_vdi_cbt_enabled_is value msg =
    Alcotest.(check bool) msg value (Db.VDI.get_cbt_enabled ~__context ~self:vdi_ref) in
  let check_params = Alcotest.(check (option (pair string string))) in

  let enable_cbt_params = ref None in
  let disable_cbt_params = ref None in
  register_smapiv2_server
    ~vdi_enable_cbt:(fun _ ~dbg ~sr ~vdi -> enable_cbt_params := Some (sr, vdi))
    ~vdi_disable_cbt:(fun _ ~dbg ~sr ~vdi -> disable_cbt_params := Some (sr, vdi))
    sr_uuid;

  Xapi_vdi.enable_cbt ~__context ~self:vdi_ref;
  check_params "The parameters should be correctly passed to SMAPIv2 from VDI.enable_cbt" (Some (sr_uuid, vdi_location)) !enable_cbt_params;
  assert_vdi_cbt_enabled_is true "cbt_enabled should be true when VDI.enable_cbt returns successfully";

  Xapi_vdi.enable_cbt ~__context ~self:vdi_ref;
  assert_vdi_cbt_enabled_is true "VDI.enable_cbt should be idempotent";

  Xapi_vdi.disable_cbt ~__context ~self:vdi_ref;
  check_params "The parameters should be correctly passed to SMAPIv2 from VDI.disable_cbt" (Some (sr_uuid, vdi_location)) !disable_cbt_params;
  assert_vdi_cbt_enabled_is false "cbt_enabled should be false when VDI.disable_cbt returns successfully";

  Xapi_vdi.disable_cbt ~__context ~self:vdi_ref;
  assert_vdi_cbt_enabled_is false "VDI.disable_cbt should be idempotent"


let test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi () =
  let __context = Test_common.make_test_database () in
  let self = Test_common.make_vdi ~__context ~_type:`cbt_metadata () in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  Alcotest.check_raises
    "VDI.set_metadata_of_pool should throw VDI_INCOMPATIBLE_TYPE for a cbt_metadata VDI"
    Api_errors.(Server_error (vdi_incompatible_type, [Ref.string_of self; Record_util.vdi_type_to_string `cbt_metadata]))
    (fun () -> Xapi_vdi.set_metadata_of_pool ~__context ~self ~value:pool)

let test_get_nbd_info =
  let assert_same_infos =
    Alcotest.check (Alcotest_comparators.vdi_nbd_server_info_set) "same vdi_nbd_server_info set"
  in
  let make_host __context sR ?(pbd_attached=true) ?hostname pifs () =
    let host = Test_common.make_host ~__context ?hostname () in
    let _: _ API.Ref.t = Test_common.make_pbd ~__context ~host ~sR ~currently_attached:pbd_attached () in
    List.iter (function (network, iP, iPv6, attached) ->
        let self = Test_common.make_pif ~__context ~network ~host ~iP ~iPv6 () in
        Db.PIF.set_currently_attached ~__context ~self ~value:attached
      ) pifs;
    host
  in

  let setup_test () =
    let __context = Test_common.make_test_database () in
    let session_id = Test_common.make_session ~__context () in
    let __context = Context.make ~__context ~session_id "test" in
    let make_host ?hostname = make_host __context ?hostname in

    let sr_of_vdi = Test_common.make_sr ~__context () in
    let network = Test_common.make_network ~__context () in
    let vdi = Test_common.make_vdi ~__context ~sR:sr_of_vdi () in

    (__context, make_host, sr_of_vdi, network, vdi)
  in

  let test_returns_correct_infos () =
    let (__context, make_host, sr_of_vdi, network_1, self) = setup_test () in
    let uuid = Db.VDI.get_uuid ~__context ~self in
    let other_sr = Test_common.make_sr ~__context () in
    Db.Network.set_purpose ~__context ~self:network_1 ~value:[`nbd];
    let network_2 = Test_common.make_network ~__context ~purpose:[`nbd] () in
    let network_3 = Test_common.make_network ~__context ~purpose:[] () in

    (* Hosts connected to both the VDI's SR and a network *)
    let host1 = make_host
        ~hostname:"host1"
        sr_of_vdi
        [(network_1, "92.40.98.91", [], true); (network_1, "92.40.98.92", [], true);
         (* this PIF is not currently attached: *)
         (network_2, "92.40.98.93", ["10e1:bdb8:05a3:0002:03ae:8a24:0371:0001"], false)] () in
    let host2 = make_host
        ~hostname:"host2"
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
    (* NBD is not allowed on network_3 *)
    let _: _ API.Ref.t = make_host sr_of_vdi [(network_3, "92.40.98.98", ["10e1:bdb8:05a3:0002:03ae:8a24:0371:0006";"10e1:bdb8:05a3:0002:03ae:8a24:0371:0007"], true)] () in

    let host1_cert = "-----BEGIN CERTIFICATE-----
MIIBwTCCASqgAwIBAgIJAKl3gkjAjMdWMA0GCSqGSIb3DQEBCwUAMBIxEDAOBgNV
BAMTB0hvc3RPbmUwHhcNMTcxMDIwMTU0NzE1WhcNMjcxMDE4MTU0NzE1WjASMRAw
DgYDVQQDEwdIb3N0T25lMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDHL9+o
102DHs2ZSYFlKeg8sCsKNPr3x2UziG6fvyE6FWtZR0scToxXU8SaAdrDE0floLsy
TaF+aX1yUfFTA/SqeQ6E78rCRwf30IVJ8gE75oDzjqB0k7jm4lLFJARhiio0WIFy
/AKitLBZ8o9gD54EtZ7IuhXonMUDZrCBQ8cSOQIDAQABox8wHTAbBgNVHREEFDAS
ghBob3N0MS5kb21haW4udGxkMA0GCSqGSIb3DQEBCwUAA4GBAAwsd/Sn9jXM4+G+
qPfHEt6aRLrVbEV5jepgSuatrnCV+duykXjPMMhp3e8+5Q1rbYC/f43UHaFp7QlD
9d3e/mjzWhfgyBiNjE1Cbd06ZAHKsV1SaU6Y+TVMEOWg0HMishBt80w1dTrQeBOF
UPwgCGaWtsuDCBHHwHFij3Blm2fd
-----END CERTIFICATE-----
"
    in
    let host2_cert = "-----BEGIN CERTIFICATE-----
MIIBwTCCASqgAwIBAgIJAIhSYyKmHX1lMA0GCSqGSIb3DQEBCwUAMBIxEDAOBgNV
BAMTB0hvc3RUd28wHhcNMTcxMDIwMTU0NzM4WhcNMjcxMDE4MTU0NzM4WjASMRAw
DgYDVQQDEwdIb3N0VHdvMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCv4Rji
4mj33QJblsaiQAMVhszuYNg40iI45CaJ9A8VWUw8daqLPEzJZzc6kQemGjaUtLZb
eYX3vVYMyONjRXstPoiTdTWS1slOV/eQnbJ4N9Lt28aDFvtxBKLByavPK6Km2QdV
7E1LeZBdYakvABI3QOO38vQ+VVbewSqQlHiemwIDAQABox8wHTAbBgNVHREEFDAS
ghBob3N0Mi5kb21haW4udGxkMA0GCSqGSIb3DQEBCwUAA4GBAEMsmwc2vioVG7Qf
vBPXurSr1RittWb0OjzCwevx0fpTjNGFTBTbZJaVrtscPyaI/MZhcRCRJ125E87Q
Dk813cqiwTs4L2600lHqh3pTUL9X59jwmPqGdgelpQ8hY7uIeAocAiuWVGD42ZZA
mffVFwXJ7B8kLP5BeRyBi6nwLhjd
-----END CERTIFICATE-----
"
    in
    let host1_subject = "host1.domain.tld" in
    let host2_subject = "host2.domain.tld" in

    let get_server_certificate ~host =
      if host = host1 then host1_cert
      else if host = host2 then host2_cert
      else failwith (Printf.sprintf "unexpected host: %s" (Ref.string_of host))
    in

    let nbd_info = Xapi_vdi._get_nbd_info ~__context ~self ~get_server_certificate in

    let session_id = Context.get_session_id __context |> Ref.string_of in
    let expected:(API.vdi_nbd_server_info_t_set) =
      let vdi_nbd_server_info_exportname = "/" ^ uuid ^ "?session_id=" ^ session_id in
      let vdi_nbd_server_info_port = 10809L in
      API.[
        { vdi_nbd_server_info_exportname;
          vdi_nbd_server_info_address = "92.40.98.91";
          vdi_nbd_server_info_port;
          vdi_nbd_server_info_cert = host1_cert;
          vdi_nbd_server_info_subject = host1_subject
        };
        { vdi_nbd_server_info_exportname = "/" ^ uuid ^ "?session_id=" ^ session_id;
          vdi_nbd_server_info_address = "92.40.98.92";
          vdi_nbd_server_info_port;
          vdi_nbd_server_info_cert = host1_cert;
          vdi_nbd_server_info_subject = host1_subject
        };
        { vdi_nbd_server_info_exportname = "/" ^ uuid ^ "?session_id=" ^ session_id;
          vdi_nbd_server_info_address = "92.40.98.94";
          vdi_nbd_server_info_port;
          vdi_nbd_server_info_cert = host2_cert;
          vdi_nbd_server_info_subject = host2_subject
        };
        { vdi_nbd_server_info_exportname = "/" ^ uuid ^ "?session_id=" ^ session_id;
          vdi_nbd_server_info_address = "10e1:bdb8:05a3:0002:03ae:8a24:0371:0002";
          vdi_nbd_server_info_port;
          vdi_nbd_server_info_cert = host2_cert;
          vdi_nbd_server_info_subject = host2_subject
        };
        { vdi_nbd_server_info_exportname = "/" ^ uuid ^ "?session_id=" ^ session_id;
          vdi_nbd_server_info_address = "10e1:bdb8:05a3:0002:03ae:8a24:0371:0003";
          vdi_nbd_server_info_port;
          vdi_nbd_server_info_cert = host2_cert;
          vdi_nbd_server_info_subject = host2_subject
        };
      ]
    in
    assert_same_infos expected nbd_info
  in

  let test_returns_empty_list_when_no_host_is_connected_to_sr () =
    let (__context, make_host, sr_of_vdi, network, self) = setup_test () in
    let _: _ API.Ref.t = make_host sr_of_vdi ~pbd_attached:false [(network, "92.40.98.93", [], true)] () in
    assert_same_infos [] (Xapi_vdi.get_nbd_info ~__context ~self);
    Db.PBD.get_all ~__context |> List.iter (fun self -> Db.PBD.destroy ~__context ~self);
    assert_same_infos [] (Xapi_vdi.get_nbd_info ~__context ~self)
  in

  let test_returns_empty_list_when_no_host_is_connected_to_network () =
    let (__context, make_host, sr_of_vdi, network, self) = setup_test () in
    let _: _ API.Ref.t = make_host sr_of_vdi [(network, "92.40.98.93", [], false)] () in
    assert_same_infos [] (Xapi_vdi.get_nbd_info ~__context ~self);
    Db.PIF.get_all ~__context |> List.iter (fun self -> Db.PIF.destroy ~__context ~self);
    assert_same_infos [] (Xapi_vdi.get_nbd_info ~__context ~self)
  in

  let test_disallowed_for_cbt_metadata_vdi () =
    let (__context, make_host, sr_of_vdi, network, self) = setup_test () in
    let _: _ API.Ref.t = make_host sr_of_vdi [(network, "92.40.98.93", [], true)] () in
    Db.VDI.set_type ~__context ~self ~value:`cbt_metadata;
    let _type = Record_util.vdi_type_to_string `cbt_metadata in
    Alcotest.check_raises
      "Disallowed for cbt_metadata VDI"
      Api_errors.(Server_error (vdi_incompatible_type, [Ref.string_of self; _type]))
      (fun () -> Xapi_vdi.get_nbd_info ~__context ~self |> ignore)
  in

  [ "test_get_nbd_info_returns_correct_infos", `Quick, test_returns_correct_infos
  ; "test_get_nbd_info_returns_empty_list_when_no_host_is_connected_to_sr", `Quick, test_returns_empty_list_when_no_host_is_connected_to_sr
  ; "test_get_nbd_info_returns_empty_list_when_no_host_is_connected_to_network", `Quick, test_returns_empty_list_when_no_host_is_connected_to_network
  ; "test_get_nbd_info_disallowed_for_cbt_metadata_vdi", `Quick, test_disallowed_for_cbt_metadata_vdi
  ]

(** Initializes the test so that Xapi_vdi.data_destroy invocations will not
    fail, registers a SMAPIv2 data_destroy function that is a no-op by default,
    and returns a CBT-enabled snapshot VDI on which it is allowed to run
    data_destroy. *)
let setup_test_for_data_destroy ?(vdi_data_destroy=(fun _ ~dbg ~sr ~vdi -> ())) () =
  (* data_destroy uses the event mechanism, this is required to make the unit test work *)
  let __context, _ = Test_event.event_setup_common () in

  let sR = make_mock_server_infrastructure ~__context in
  let vdi = Test_common.make_vdi ~__context ~is_a_snapshot:true ~managed:true ~cbt_enabled:true ~sR () in
  register_smapiv2_server ~vdi_data_destroy (Db.SR.get_uuid ~__context ~self:sR);
  (__context, sR, vdi)

let test_allowed_operations_updated_when_necessary () =
  let __context, sR, self = setup_test_for_data_destroy () in
  register_smapiv2_server ~vdi_data_destroy:(fun _ ~dbg ~sr ~vdi -> ()) (Db.SR.get_uuid ~__context ~self:sR);

  let assert_allowed_operations msg check =
    Alcotest.(check bool) msg true (check (Db.VDI.get_allowed_operations ~__context ~self))
  in
  (* Populate the allowed_operations list after creating the VDI *)
  Xapi_vdi.update_allowed_operations ~__context ~self;
  assert_allowed_operations "contains `copy for a newly-created VDI" (fun ops -> List.mem `copy ops);
  (* Call data_destroy through the the message forwarding layer *)
  Api_server.Forwarder.VDI.data_destroy ~__context ~self;
  assert_allowed_operations "does not contain `copy after VDI has been data-destroyed" (fun ops -> not @@ List.mem `copy ops)

let test_data_destroy =

  (* Confirm VDI.data_destroy changes requisite fields of VDI *)
  let test_vdi_after_data_destroy () =
    let __context, sR, vDI = setup_test_for_data_destroy () in
    Db.VDI.set_type ~__context ~self:vDI ~value:`suspend;
    let vM = Test_common.make_vm ~__context () in

    let check_vdi_is_snapshot_and_type ~vDI ~snapshot ~vdi_type ~managed =
      let open Printf in
      Alcotest.check (Alcotest_comparators.vdi_type) "VDI.type"
        vdi_type
        (Db.VDI.get_type ~__context ~self:vDI);
      Alcotest.(check bool) "VDI.managed"
        managed
        (Db.VDI.get_managed ~__context ~self:vDI);
      Alcotest.(check bool) "VDI.is_a_snapshot"
        (Db.VDI.get_is_a_snapshot ~__context ~self:vDI) snapshot
    in
    check_vdi_is_snapshot_and_type ~vDI ~snapshot:true ~vdi_type:`suspend ~managed:true;

    (* set vDI as the suspend VDI of vM *)
    Db.VM.set_suspend_VDI ~__context ~self:vM ~value:vDI;
    Alcotest.check (Alcotest_comparators.ref ()) "VM.suspend_VDI should point to previously created VDI"
      vDI
      (Db.VM.get_suspend_VDI ~__context ~self:vM);

    (* run VDI.data_destroy, check it has updated VDI fields *)
    Xapi_vdi.data_destroy ~__context ~self:vDI;

    Alcotest.check (Alcotest_comparators.vdi_type) "VDI.data_destroy should set VDI type to cbt_metadata"
      `cbt_metadata
      (Db.VDI.get_type ~__context ~self:vDI);

    Alcotest.check (Alcotest_comparators.ref ()) "VM.suspend_VDI should be set to null"
      Ref.null
      (Db.VM.get_suspend_VDI ~__context ~self:vM);

    (* check for idempotence for metadata snapshot VDIs *)
    check_vdi_is_snapshot_and_type ~vDI ~snapshot:true ~vdi_type:`cbt_metadata ~managed:true;
    Xapi_vdi.data_destroy ~__context ~self:vDI;
    check_vdi_is_snapshot_and_type ~vDI ~snapshot:true ~vdi_type:`cbt_metadata ~managed:true
  in

  (* check VDI.data_destroy throws VDI_NOT_MANAGED if managed:false *)
  let test_vdi_managed_data_destroy () =
    let __context, sR, vDI = setup_test_for_data_destroy () in
    Db.VDI.set_managed ~__context ~self:vDI ~value:false;
    Alcotest.check_raises
      "VDI.data_destroy only works on managed VDI"
      Api_errors.(Server_error (vdi_not_managed, [Ref.string_of vDI]))
      (fun () -> Xapi_vdi.data_destroy ~__context ~self:vDI)
  in

  let test_does_not_change_vdi_type_to_cbt_metadata_if_it_fails () =
    let __context, sR, vdi = setup_test_for_data_destroy ~vdi_data_destroy:(fun _ ~dbg ~sr ~vdi -> raise (Failure "error")) () in
    let original_type = Db.VDI.get_type ~__context ~self:vdi in
    try Xapi_vdi.data_destroy ~__context ~self:vdi with _ -> ();
      Alcotest.check (Alcotest_comparators.vdi_type)
        "data_destroy should not change the VDI's type to cbt_metadata when it did not succeed, it should preserve the original type"
        original_type
        (Db.VDI.get_type ~__context ~self:vdi)
  in

  let test_data_destroy_timing =
    let bg f = Thread.create f () in

    (* Creates a VBD that is currently_attached to our VDI *)
    let setup_test () =
      let __context, sR, vDI = setup_test_for_data_destroy () in
      let vM = Test_common.make_vm ~__context () in
      let vbd = Test_common.make_vbd ~__context ~vDI ~vM ~currently_attached:true () in
      let start_vbd_unplug () =
        Db.VBD.set_current_operations ~__context ~self:vbd ~value:["x", `unplug]
      in
      let finish_vbd_unplug () =
        Db.VBD.set_currently_attached ~__context ~self:vbd ~value:false;
        Db.VBD.set_current_operations ~__context ~self:vbd ~value:[]
      in
      let destroy_vbd () = Db.VBD.destroy ~__context ~self:vbd in
      let data_destroy ~timeout =
        (* We need a 1-second tolerance here, otherwise the test will fail *)
        let timebox_timeout = timeout +. 1.0 in
        Helpers.timebox
          ~timeout:timebox_timeout
          ~otherwise:(fun () -> Alcotest.fail (Printf.sprintf "data_destroy did not return in %f seconds" timebox_timeout))
          (fun () -> Xapi_vdi._data_destroy ~__context ~self:vDI ~timeout)
      in
      (vDI, start_vbd_unplug, finish_vbd_unplug, destroy_vbd, data_destroy)
    in

    let test_data_destroy_succeeds_when_unplug_starts_later () =
      let _vdi, start_vbd_unplug, finish_vbd_unplug, destroy_vbd, data_destroy = setup_test () in
      let t = bg (fun () ->
          Thread.delay 0.05;
          start_vbd_unplug ();
          Thread.delay 0.05;
          finish_vbd_unplug ();
          Thread.delay 0.05;
          destroy_vbd ()
        ) in
      data_destroy ~timeout:0.4;
      Thread.join t
    in

    let test_data_destroy_succeeds_when_vbd_is_being_unplugged () =
      let _vdi, start_vbd_unplug, finish_vbd_unplug, destroy_vbd, data_destroy = setup_test () in
      let t = bg (fun () ->
          start_vbd_unplug ();
          Thread.delay 0.05;
          finish_vbd_unplug ();
          Thread.delay 0.05;
          destroy_vbd ()
        ) in
      Thread.delay 0.02;
      data_destroy ~timeout:0.4;
      Thread.join t
    in

    let test_data_destroy_succeeds_when_vbd_is_being_destroyed () =
      let _vdi, start_vbd_unplug, finish_vbd_unplug, destroy_vbd, data_destroy = setup_test () in
      let t = bg (fun () ->
          start_vbd_unplug ();
          finish_vbd_unplug ();
          Thread.delay 0.05;
          destroy_vbd ()
        ) in
      Thread.delay 0.02;
      data_destroy ~timeout:0.4;
      Thread.join t
    in

    let test_data_destroy_times_out_when_vbd_does_not_get_unplugged_in_time () =
      let vDI, start_vbd_unplug, finish_vbd_unplug, destroy_vbd, data_destroy = setup_test () in
      let t = bg (fun () ->
          Thread.delay 0.05;
          start_vbd_unplug ();
          (* finish_vbd_unplug does not happen in time *)
        ) in
      Alcotest.check_raises "data_destroy should raise VDI_IN_USE after its timeout"
        Api_errors.(Server_error (vdi_in_use, [Ref.string_of vDI; "data_destroy"]))
        (fun () -> data_destroy ~timeout:0.4);
      Thread.join t
    in

    let test_data_destroy_times_out_when_vbd_does_not_get_destroyed_in_time () =
      let vDI, start_vbd_unplug, finish_vbd_unplug, destroy_vbd, data_destroy = setup_test () in
      let t = bg (fun () ->
          Thread.delay 0.05;
          start_vbd_unplug ();
          Thread.delay 0.05;
          finish_vbd_unplug ();
          (* destroy_vbd does not happen in time *)
        ) in
      Alcotest.check_raises "data_destroy should raise VDI_IN_USE after its timeout"
        Api_errors.(Server_error (vdi_in_use, [Ref.string_of vDI; "data_destroy"]))
        (fun () -> data_destroy ~timeout:0.4);
      Thread.join t
    in

    [ "test_data_destroy_succeeds_when_unplug_starts_later", `Slow, test_data_destroy_succeeds_when_unplug_starts_later
    ; "test_data_destroy_succeeds_when_vbd_is_being_unplugged", `Slow, test_data_destroy_succeeds_when_vbd_is_being_unplugged
    ; "test_data_destroy_succeeds_when_vbd_is_being_destroyed", `Slow, test_data_destroy_succeeds_when_vbd_is_being_destroyed
    ; "test_data_destroy_times_out_when_vbd_does_not_get_unplugged_in_time", `Slow, test_data_destroy_times_out_when_vbd_does_not_get_unplugged_in_time
    ; "test_data_destroy_times_out_when_vbd_does_not_get_destroyed_in_time", `Slow, test_data_destroy_times_out_when_vbd_does_not_get_destroyed_in_time
    ]

  in

  [ "test_vdi_after_data_destroy", `Quick, test_vdi_after_data_destroy
  ; "test_vdi_managed_data_destroy", `Quick, test_vdi_managed_data_destroy
  ; "test_data_destroy_does_not_change_vdi_type_to_cbt_metadata_if_it_fails", `Quick, test_does_not_change_vdi_type_to_cbt_metadata_if_it_fails
  ] @ test_data_destroy_timing

let test_vdi_list_changed_blocks () =
  let __context = Test_common.make_test_database () in
  let sR = make_mock_server_infrastructure ~__context in
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sR in

  let list_changed_blocks_params = ref None in
  let list_changed_blocks_string = "listchangedblocks000" in

  let vdi_from_location = "vdi_from_location" in
  let vdi_from = Test_common.make_vdi ~__context ~location:vdi_from_location ~is_a_snapshot:true ~managed:true ~cbt_enabled:true ~sR () in
  let vdi_to_location = "vdi_to_location" in
  let vdi_to = Test_common.make_vdi ~__context~location:vdi_to_location  ~sR ~cbt_enabled:true ~managed:true () in

  register_smapiv2_server
    ~vdi_list_changed_blocks:(fun _ ~dbg ~sr ~vdi_from ~vdi_to -> list_changed_blocks_params := Some (sr,(vdi_from,vdi_to)); list_changed_blocks_string)
    (Db.SR.get_uuid ~__context ~self:sR);

  Alcotest.(check string) "VDI.list_changed_blocks"
    (Xapi_vdi.list_changed_blocks ~__context ~vdi_from ~vdi_to)
    list_changed_blocks_string;
  Alcotest.(check (option (pair string (pair string string)))) "VDI.list_changed_blocks parameters"
    (Some (sr_uuid, (vdi_from_location, vdi_to_location)))
    !list_changed_blocks_params

let test =
  [ "test_cbt_enable_disable", `Quick, test_cbt_enable_disable
  ; "test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi", `Quick, test_set_metadata_of_pool_doesnt_allow_cbt_metadata_vdi
  ; "test_allowed_operations_updated_when_necessary", `Quick, test_allowed_operations_updated_when_necessary
  ; "test_vdi_list_changed_blocks", `Quick, test_vdi_list_changed_blocks ]
  @ test_get_nbd_info
  @ test_data_destroy
