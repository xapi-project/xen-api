(*
 * Copyright (c) Cloud Software Group, Inc.
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
(* Tests for Lldp.parse_neighbors: parsing the JSON emitted by
   [lldpcli -f json0 show neighbors]. *)
let neighbor_testable =
  Alcotest.testable
    (fun ppf (n : Network_stats.lldp_neighbor) ->
      Fmt.pf ppf "{system_name=%a; port_id=%a; port_description=%a}"
        Fmt.(option string)
        n.system_name
        Fmt.(option string)
        n.port_id
        Fmt.(option string)
        n.port_description
    )
    ( = )

let result_testable = Alcotest.(list (pair string neighbor_testable))

let rx ?system_name ?port_id ?port_description () =
  Network_stats.{system_name; port_id; port_description}

(* One interface with one neighbour (a Cisco Nexus switch). *)
let single_json =
  {|
  { "lldp": [ { "interface": [
    { "name": "eno8303",
      "chassis": [ { "name": [ { "value": "NKG-ESWA07-2.eng.citrite.net" } ] } ],
      "port": [ { "id": [ { "value": "Ethernet1/28" } ],
                 "descr": [ { "value": "nkg-dt16/idrac" } ] } ] }
  ] } ] }
  |}

(* One interface with two neighbours (wider multicast scope / repeater). *)
let multi_json =
  {|
  { "lldp": [ { "interface": [
    { "name": "eno8303",
      "chassis": [ { "name": [ { "value": "TOR-A-01" } ] } ],
      "port": [ { "id": [ { "value": "Eth1/8/3" } ],
                 "descr": [ { "value": "rack7-a" } ] } ] },
    { "name": "eno8303",
      "chassis": [ { "name": [ { "value": "TOR-B-02" } ] } ],
      "port": [ { "id": [ { "value": "Eth2/8/3" } ],
                 "descr": [ { "value": "rack7-b" } ] } ] }
  ] } ] }
  |}

let empty_json = {| { "lldp": [ { "interface": [] } ] } |}

let test_single () =
  Alcotest.check result_testable "single neighbour"
    [
      ( "eno8303"
      , rx ~system_name:"NKG-ESWA07-2.eng.citrite.net" ~port_id:"Ethernet1/28"
          ~port_description:"nkg-dt16/idrac" ()
      )
    ]
    (Lldp.parse_neighbors single_json)

let test_multi () =
  (* The parser returns all neighbours; picking one is done by the caller. *)
  Alcotest.check result_testable "two neighbours on one interface"
    [
      ( "eno8303"
      , rx ~system_name:"TOR-A-01" ~port_id:"Eth1/8/3"
          ~port_description:"rack7-a" ()
      )
    ; ( "eno8303"
      , rx ~system_name:"TOR-B-02" ~port_id:"Eth2/8/3"
          ~port_description:"rack7-b" ()
      )
    ]
    (Lldp.parse_neighbors multi_json)

let test_empty () =
  Alcotest.check result_testable "no neighbours" []
    (Lldp.parse_neighbors empty_json)

let test_malformed () =
  Alcotest.check result_testable "malformed JSON yields empty" []
    (Lldp.parse_neighbors "not json {")

let state_testable =
  Alcotest.testable (Fmt.of_to_string Network_stats.string_of_lldp_state) ( = )

(* [state_of] on a device with no real driver never matches the blocklist, so a
   non-enabled interface is reported as [Disabled] rather than [Blocked]. *)
let test_state_disabled () =
  Alcotest.check state_testable "not rx-and-tx is disabled"
    Network_stats.Disabled
    (Lldp.state_of "lldptest0" ~enabled:false)

let test_state_enabled () =
  Alcotest.check state_testable "rx-and-tx is enabled" Network_stats.Enabled
    (Lldp.state_of "lldptest0" ~enabled:true)

let interfaces_json =
  {|
  { "lldp": [ { "interface": [
    { "name": "eno0", "status": [ { "value": "RX and TX" } ] },
    { "name": "eno1", "status": [ { "value": "disabled" } ] },
    { "name": "ovs-system", "status": [ { "value": "disabled" } ] }
  ] } ] }
  |}

let test_parse_enabled () =
  Alcotest.check
    Alcotest.(list string)
    "only rx-and-tx interfaces" ["eno0"]
    (Lldp.parse_enabled_interfaces interfaces_json)

let test_parse_enabled_empty () =
  Alcotest.check
    Alcotest.(list string)
    "no interfaces" []
    (Lldp.parse_enabled_interfaces {| { "lldp": { "interface": [] } } |})

let test_parse_enabled_malformed () =
  Alcotest.check
    Alcotest.(list string)
    "malformed JSON yields empty" []
    (Lldp.parse_enabled_interfaces "not json {")

let tests =
  [
    ( "lldp_parse_neighbors"
    , [
        ("single", `Quick, test_single)
      ; ("multi", `Quick, test_multi)
      ; ("empty", `Quick, test_empty)
      ; ("malformed", `Quick, test_malformed)
      ]
    )
  ; ( "lldp_state_of"
    , [
        ("disabled", `Quick, test_state_disabled)
      ; ("enabled", `Quick, test_state_enabled)
      ]
    )
  ; ( "lldp_parse_enabled_interfaces"
    , [
        ("enabled", `Quick, test_parse_enabled)
      ; ("empty", `Quick, test_parse_enabled_empty)
      ; ("malformed", `Quick, test_parse_enabled_malformed)
      ]
    )
  ]
