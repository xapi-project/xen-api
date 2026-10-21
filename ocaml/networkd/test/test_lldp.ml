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

(* Each LLDP value is sanitised before being surfaced -- UTF-8 verified
   (malformed -> dropped), control characters removed, and truncated to 256
   bytes on a UTF-8 codepoint boundary. *)
let neighbour_json sys pid descr =
  Printf.sprintf
    {|{ "lldp": [ { "interface": [
      { "name": "eno0",
        "chassis": [ { "name": [ { "value": "%s" } ] } ],
        "port": [ { "id": [ { "value": "%s" } ],
                   "descr": [ { "value": "%s" } ] } ] }
    ] } ] }|}
    sys pid descr

let parse_one json : Network_stats.lldp_neighbor =
  match Lldp.parse_neighbors json with
  | [(_, n)] ->
      n
  | _ ->
      Alcotest.fail "expected exactly one neighbour"

let a n = String.make n 'A'

let test_cap_over_length () =
  let n = parse_one (neighbour_json (a 300) (a 257) (a 255)) in
  Alcotest.(check (option string))
    "system_name 300B capped to 256"
    (Some (a 256))
    n.system_name ;
  Alcotest.(check (option string))
    "port_id 257B capped to 256"
    (Some (a 256))
    n.port_id ;
  Alcotest.(check (option string))
    "port_description 255B unchanged"
    (Some (a 255))
    n.port_description

let test_cap_exact_256 () =
  let n = parse_one (neighbour_json (a 256) (a 256) (a 256)) in
  Alcotest.(check (option string))
    "exactly 256B unchanged"
    (Some (a 256))
    n.system_name

let test_cap_utf8_no_split () =
  (* 255 ASCII bytes + one 2-byte codepoint = 257 bytes: capping at 256 must
     drop the whole codepoint, not split it, leaving 255 valid bytes. *)
  let n = parse_one (neighbour_json (a 255 ^ "\xc3\xa9") "p" "d") in
  Alcotest.(check (option string))
    "codepoint not split at boundary"
    (Some (a 255))
    n.system_name

let test_cap_utf8_fits () =
  (* 254 ASCII bytes + one 2-byte codepoint = 256 bytes: fits exactly. *)
  let v = a 254 ^ "\xc3\xa9" in
  let n = parse_one (neighbour_json v "p" "d") in
  Alcotest.(check (option string))
    "256B multibyte value unchanged" (Some v) n.system_name

let test_reject_invalid_utf8 () =
  (* Raw invalid UTF-8 bytes (lone 0xff): Yojson passes them through, so Uutf
     must reject the value and it is dropped to None. *)
  let n = parse_one (neighbour_json "AB\xffCD" "p" "d") in
  Alcotest.(check (option string)) "invalid UTF-8 dropped" None n.system_name ;
  Alcotest.(check (option string)) "port_id kept" (Some "p") n.port_id

let test_strip_control_chars () =
  (* C0 controls (incl. tab/newline) and DEL are removed, printable kept. *)
  let n = parse_one (neighbour_json "A\x01B\tC\x7fD\nE" "p" "d") in
  Alcotest.(check (option string))
    "control characters removed" (Some "ABCDE") n.system_name

let test_strip_all_control_is_empty () =
  (* A value that is only control characters sanitises to the empty string,
     not None -- it was valid UTF-8, just had nothing printable. *)
  let n = parse_one (neighbour_json "\x01\x02\x03" "p" "d") in
  Alcotest.(check (option string))
    "all-control value becomes empty" (Some "") n.system_name

let test_strip_xml_noncharacters () =
  (* U+FFFE (\xef\xbf\xbe) and U+FFFF (\xef\xbf\xbf) are valid UTF-8 but illegal
     in XML; they are removed so they cannot poison the XML-backed database. *)
  let n = parse_one (neighbour_json "A\xef\xbf\xbeB\xef\xbf\xbfC" "p" "d") in
  Alcotest.(check (option string))
    "XML-illegal noncharacters removed" (Some "ABC") n.system_name

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
      ; ("cap_over_length", `Quick, test_cap_over_length)
      ; ("cap_exact_256", `Quick, test_cap_exact_256)
      ; ("cap_utf8_no_split", `Quick, test_cap_utf8_no_split)
      ; ("cap_utf8_fits", `Quick, test_cap_utf8_fits)
      ; ("reject_invalid_utf8", `Quick, test_reject_invalid_utf8)
      ; ("strip_control_chars", `Quick, test_strip_control_chars)
      ; ("strip_all_control_is_empty", `Quick, test_strip_all_control_is_empty)
      ; ("strip_xml_noncharacters", `Quick, test_strip_xml_noncharacters)
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
