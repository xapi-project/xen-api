module T = Test_common

let make_configurable_vif
    ?(guest_features = [("feature-static-ip-setting", "1")]) () =
  let __context = T.make_test_database () in
  let vm = T.make_vm ~__context () in
  let guest_metrics = Ref.make () in
  Db.VM_guest_metrics.create ~__context ~ref:guest_metrics
    ~uuid:(T.make_uuid ()) ~os_version:[] ~netbios_name:[]
    ~pV_drivers_version:[] ~pV_drivers_up_to_date:false ~memory:[] ~disks:[]
    ~networks:[] ~services:[] ~pV_drivers_detected:false ~other:guest_features
    ~last_updated:Clock.Date.epoch ~other_config:[] ~live:false
    ~can_use_hotplug_vbd:`unspecified ~can_use_hotplug_vif:`unspecified ;
  Db.VM.set_guest_metrics ~__context ~self:vm ~value:guest_metrics ;
  let network = T.make_network ~__context () in
  let vif = T.make_vif ~__context ~device:"0" ~network ~vM:vm () in
  (__context, vm, vif)

let test_configure_ipv4_dns () =
  let __context, _, vif = make_configurable_vif () in
  let dns = ["192.0.2.53"; "198.51.100.53"] in
  Xapi_vif.configure_ipv4 ~__context ~self:vif ~mode:`Static
    ~address:"192.0.2.10/24" ~gateway:"192.0.2.1" ~dns ;
  Alcotest.(check (list string))
    "IPv4 DNS is stored" dns
    (Db.VIF.get_ipv4_dns ~__context ~self:vif) ;
  Xapi_vif.configure_ipv4 ~__context ~self:vif ~mode:`Static
    ~address:"192.0.2.10/24" ~gateway:"192.0.2.1" ~dns:[] ;
  Alcotest.(check (list string))
    "empty IPv4 DNS clears the value" []
    (Db.VIF.get_ipv4_dns ~__context ~self:vif)

let test_configure_ipv6_dns () =
  let __context, _, vif = make_configurable_vif () in
  let dns = ["2001:db8::53"; "2001:db8::54"] in
  Xapi_vif.configure_ipv6 ~__context ~self:vif ~mode:`Static
    ~address:"2001:db8::10/64" ~gateway:"2001:db8::1" ~dns ;
  Alcotest.(check (list string))
    "IPv6 DNS is stored" dns
    (Db.VIF.get_ipv6_dns ~__context ~self:vif) ;
  Xapi_vif.configure_ipv6 ~__context ~self:vif ~mode:`Static
    ~address:"2001:db8::10/64" ~gateway:"2001:db8::1" ~dns:[] ;
  Alcotest.(check (list string))
    "empty IPv6 DNS clears the value" []
    (Db.VIF.get_ipv6_dns ~__context ~self:vif)

let test_configure_ipv4_rejects_ipv6_dns () =
  let __context, _, vif = make_configurable_vif () in
  Alcotest.check_raises "IPv4 configuration rejects IPv6 DNS"
    Api_errors.(Server_error (invalid_ip_address_specified, ["dns"]))
    (fun () ->
      Xapi_vif.configure_ipv4 ~__context ~self:vif ~mode:`Static
        ~address:"192.0.2.10/24" ~gateway:"" ~dns:["2001:db8::53"]
    )

let test_configure_ipv6_rejects_ipv4_dns () =
  let __context, _, vif = make_configurable_vif () in
  Alcotest.check_raises "IPv6 configuration rejects IPv4 DNS"
    Api_errors.(Server_error (invalid_ip_address_specified, ["dns"]))
    (fun () ->
      Xapi_vif.configure_ipv6 ~__context ~self:vif ~mode:`Static
        ~address:"2001:db8::10/64" ~gateway:"" ~dns:["192.0.2.53"]
    )

let test_configure_ipv4_requires_guest_static_ip_feature () =
  let __context, vm, vif = make_configurable_vif ~guest_features:[] () in
  Alcotest.check_raises "IPv4 configuration requires the guest feature"
    Api_errors.(Server_error (vm_lacks_feature, [Ref.string_of vm]))
    (fun () ->
      Xapi_vif.configure_ipv4 ~__context ~self:vif ~mode:`Static
        ~address:"192.0.2.10/24" ~gateway:"" ~dns:[]
    )

let test_configure_ipv6_requires_guest_static_ip_feature_enabled () =
  let __context, vm, vif =
    make_configurable_vif
      ~guest_features:[("feature-static-ip-setting", "0")]
      ()
  in
  Alcotest.check_raises "IPv6 configuration requires feature value 1"
    Api_errors.(Server_error (vm_lacks_feature, [Ref.string_of vm]))
    (fun () ->
      Xapi_vif.configure_ipv6 ~__context ~self:vif ~mode:`Static
        ~address:"2001:db8::10/64" ~gateway:"" ~dns:[]
    )

let check_dns_is_passed_to_xenops_model ~ipv4_dns ~ipv6_dns () =
  let __context = T.make_test_database () in
  let vm = T.make_vm ~__context () in
  let network = T.make_network ~__context () in
  let vif =
    T.make_vif ~__context ~device:"0" ~network ~vM:vm
      ~ipv4_configuration_mode:`Static ~ipv4_addresses:["192.0.2.10/24"]
      ~ipv4_gateway:"192.0.2.1" ~ipv4_dns ~ipv6_configuration_mode:`Static
      ~ipv6_addresses:["2001:db8::10/64"] ~ipv6_gateway:"2001:db8::1" ~ipv6_dns
      ()
  in
  let model =
    Xapi_xenops.MD.of_vif ~__context
      ~vm:(Db.VM.get_record ~__context ~self:vm)
      ~vif:(vif, Db.VIF.get_record ~__context ~self:vif)
  in
  let open Xenops_interface.Vif in
  ( match model.ipv4_configuration with
  | Static4 (addresses, gateway, dns) ->
      Alcotest.(check (list string))
        "xenops IPv4 addresses" ["192.0.2.10/24"] addresses ;
      Alcotest.(check (option string))
        "xenops IPv4 gateway" (Some "192.0.2.1") gateway ;
      Alcotest.(check (list string)) "xenops IPv4 DNS" ipv4_dns dns
  | _ ->
      Alcotest.fail "xenops model did not use static IPv4 configuration"
  ) ;
  match model.ipv6_configuration with
  | Static6 (addresses, gateway, dns) ->
      Alcotest.(check (list string))
        "xenops IPv6 addresses" ["2001:db8::10/64"] addresses ;
      Alcotest.(check (option string))
        "xenops IPv6 gateway" (Some "2001:db8::1") gateway ;
      Alcotest.(check (list string)) "xenops IPv6 DNS" ipv6_dns dns
  | _ ->
      Alcotest.fail "xenops model did not use static IPv6 configuration"

let test_dns_is_passed_to_xenops_model () =
  check_dns_is_passed_to_xenops_model ~ipv4_dns:["192.0.2.53"]
    ~ipv6_dns:["2001:db8::53"] ()

let test_no_dns_is_passed_to_xenops_model () =
  check_dns_is_passed_to_xenops_model ~ipv4_dns:[] ~ipv6_dns:[] ()

let test =
  [
    ("configure_ipv4_dns", `Quick, test_configure_ipv4_dns)
  ; ("configure_ipv6_dns", `Quick, test_configure_ipv6_dns)
  ; ( "configure_ipv4_rejects_ipv6_dns"
    , `Quick
    , test_configure_ipv4_rejects_ipv6_dns
    )
  ; ( "configure_ipv6_rejects_ipv4_dns"
    , `Quick
    , test_configure_ipv6_rejects_ipv4_dns
    )
  ; ( "configure_ipv4_requires_guest_static_ip_feature"
    , `Quick
    , test_configure_ipv4_requires_guest_static_ip_feature
    )
  ; ( "configure_ipv6_requires_guest_static_ip_feature_enabled"
    , `Quick
    , test_configure_ipv6_requires_guest_static_ip_feature_enabled
    )
  ; ("dns_is_passed_to_xenops_model", `Quick, test_dns_is_passed_to_xenops_model)
  ; ( "no_dns_is_passed_to_xenops_model"
    , `Quick
    , test_no_dns_is_passed_to_xenops_model
    )
  ]
