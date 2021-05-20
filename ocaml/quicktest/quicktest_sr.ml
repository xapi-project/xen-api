(** Make sure sr_scan doesn't throw an exception *)
let sr_scan_test rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  Alcotest.(check unit)
    "SR_SCAN should be able to scan a working SR" ()
    (Client.Client.SR.scan rpc session_id sr)

(** If SR_UPDATE is present then try it out *)
let sr_update_test rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  Alcotest.(check unit)
    "SR_UPDATE should not fail" ()
    (Client.Client.SR.update rpc session_id sr)

(** Basic support for parsing the SR probe result *)
type sr_probe_sr = {uuid: string}

let parse_sr_probe_xml (xml : string) : sr_probe_sr list =
  match Xml.parse_string xml with
  | Xml.Element ("SRlist", _, children) ->
      let parse_sr = function
        | Xml.Element ("SR", _, children) ->
            let parse_kv = function
              | Xml.Element (key, _, [Xml.PCData v]) ->
                  (key, Astring.String.trim v)
                  (* remove whitespace at both ends *)
              | Xml.Element (key, _, []) ->
                  (key, "") (* We get an empty array for empty tag contents *)
              | _ ->
                  failwith "Malformed key/value pair"
            in
            let all = List.map parse_kv children in
            {uuid= List.assoc "UUID" all}
        | _ ->
            failwith "Malformed or missing <SR>"
      in
      List.map parse_sr children
  | _ ->
      failwith "Missing <SRlist> element"

(** If SR_PROBE is present then probe for an existing plugged in SR and make sure it can
    be found. *)
let sr_probe_test rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  (* Acquire device config parameters from an attached PBD *)
  let all_pbds = Client.Client.SR.get_PBDs rpc session_id sr in
  match
    List.filter
      (fun pbd -> Client.Client.PBD.get_currently_attached rpc session_id pbd)
      all_pbds
  with
  | [] ->
      Alcotest.fail "Couldn't find an attached PBD"
  | pbd :: _ ->
      let srr = Client.Client.SR.get_record rpc session_id sr in
      let pbdr = Client.Client.PBD.get_record rpc session_id pbd in
      Client.Client.PBD.unplug rpc session_id pbd ;
      let xml =
        Xapi_stdext_pervasives.Pervasiveext.finally
          (fun () ->
            Client.Client.SR.probe ~rpc ~session_id ~host:pbdr.API.pBD_host
              ~device_config:pbdr.API.pBD_device_config
              ~sm_config:srr.API.sR_sm_config ~_type:srr.API.sR_type
            )
          (* Restore the original state even if the above code fails *)
            (fun () -> Client.Client.PBD.plug rpc session_id pbd
            )
      in
      let srs = parse_sr_probe_xml xml in
      List.iter
        (fun sr -> print_endline (Printf.sprintf "Probe found SR: %s" sr.uuid))
        srs ;
      ( match srs with
      | [] ->
          Alcotest.fail
            "Probe failed to find an SR, even though one is plugged in"
      | _ ->
          ()
      ) ;
      let all_uuids = List.map (fun sr -> sr.uuid) srs in
      if not (List.mem srr.API.sR_uuid all_uuids) then
        Alcotest.fail
          (Printf.sprintf
             "Probe failed to find SR %s even though it is plugged in"
             srr.API.sR_uuid
          )

let sr_set_name_test rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  let old_name_label =
    Client.Client.SR.get_name_label ~rpc ~session_id ~self:sr
  in
  let old_name_description =
    Client.Client.SR.get_name_description ~rpc ~session_id ~self:sr
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Client.Client.SR.set_name_label ~rpc ~session_id ~sr
        ~value:"test_name_label" ;
      Client.Client.SR.set_name_description ~rpc ~session_id ~sr
        ~value:"test_name_description" ;
      Alcotest.(check string)
        "SR.name_label" "test_name_label"
        (Client.Client.SR.get_name_label ~rpc ~session_id ~self:sr) ;
      Alcotest.(check string)
        "SR.name_description" "test_name_description"
        (Client.Client.SR.get_name_description ~rpc ~session_id ~self:sr) ;
      Qt.SR.test_update rpc session_id sr
      )
    (fun () ->
      Client.Client.SR.set_name_label ~rpc ~session_id ~sr ~value:old_name_label ;
      Client.Client.SR.set_name_description ~rpc ~session_id ~sr
        ~value:old_name_description
      )

let tests () =
  let open Qt_filter in
  [
    [("sr_scan_test", `Slow, sr_scan_test)]
    |> conn
    |> sr SR.(all |> allowed_operations [`scan])
  ; [("sr_update_test", `Slow, sr_update_test)]
    |> conn
    |> sr SR.(all |> has_capabilities [Sr_capabilities.sr_update])
  ; [("sr_probe_test", `Slow, sr_probe_test)]
    |> conn
    |> sr SR.(all |> has_capabilities [Sr_capabilities.sr_probe] |> can_unplug)
  ; [("sr_set_name_test", `Quick, sr_set_name_test)]
    |> conn
    |> sr SR.(all |> allowed_operations [`update])
  ]
  |> List.concat
